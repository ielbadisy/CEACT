#' Cost-Effectiveness Analysis on Multiply Imputed Data
#'
#' Estimates the incremental cost, incremental effect, ICER, and incremental
#' net monetary benefit (INMB) in each completed data set, then pools them with
#' Rubin's rules. Within-imputation variances use the unequal-variance
#' (Welch-type) standard error of each mean difference, and the cost-effect
#' covariance is carried through so that the INMB variance is correct at every
#' willingness-to-pay threshold. Degrees of freedom follow Barnard and Rubin
#' (1999) with complete-data degrees of freedom `n - 2`.
#'
#' The probability that the treatment is cost-effective at threshold `k` is
#' `P(INMB(k) > 0)` from the pooled t reference distribution.
#'
#' @param formula A formula of the form `cost + effect ~ group`.
#' @param data Multiply imputed data: a `mimar_imputation` object (from
#'   `mimar::impute()`), a `mids` object (from `mice::mice()`), or a list of
#'   completed data frames.
#' @param ref Reference group label.
#' @param wtp_range Numeric vector of willingness-to-pay thresholds.
#' @param weights Optional name of a numeric column holding case weights (for
#'   example matching or inverse probability weights), looked up in each
#'   completed data set so weights can differ across imputations. The
#'   within-imputation variance treats weights as fixed.
#' @param conf Confidence level.
#'
#' @return An object of class `micea`: a list with `summary` (pooled estimates,
#'   standard errors, confidence intervals, degrees of freedom, and fraction of
#'   missing information), `ceac` (an `nmb_ceac` table usable with
#'   [plot_ceac()]), `estimates` (per-imputation estimates), the pooled
#'   components `Qbar`, `Ubar`, `B`, and `Tvar`, and `m`, `n`, `ref`,
#'   `formula`, and `call`.
#'
#' @references
#' Rubin DB. Multiple Imputation for Nonresponse in Surveys. Wiley; 1987.
#'
#' Barnard J, Rubin DB. Small-sample degrees of freedom with multiple
#' imputation. Biometrika. 1999;86(4):948-955.
#'
#' @examples
#' df <- simulate_ce_trial(n = 100, seed = 123)
#' set.seed(1)
#' df$cost[sample(nrow(df), 40)] <- NA
#' # Toy within-group hot-deck imputation, repeated five times
#' imps <- lapply(1:5, function(i) {
#'   d <- df
#'   for (g in levels(d$group)) {
#'     miss <- d$group == g & is.na(d$cost)
#'     pool <- d$cost[d$group == g & !is.na(d$cost)]
#'     d$cost[miss] <- sample(pool, sum(miss), replace = TRUE)
#'   }
#'   d
#' })
#' res <- micea(cost + effect ~ group, data = imps, ref = "control",
#'              wtp_range = seq(0, 50000, 5000))
#' res
#' head(res$ceac)
#'
#' @export
micea <- function(formula, data, ref, wtp_range = seq(0, 100000, 1000),
                  weights = NULL, conf = 0.95) {
  imps <- as_completed_list(data)
  m <- length(imps)
  if (m < 2L) stop("`data` must contain at least two imputations.", call. = FALSE)

  fits <- lapply(imps, function(d) ce_fit(formula, d, ref, weights))
  n <- fits[[1]]$n
  if (any(vapply(fits, function(f) f$n, numeric(1)) != n)) {
    stop("All completed data sets must have the same number of rows.",
         call. = FALSE)
  }

  Q <- do.call(rbind, lapply(fits, function(f) f$theta))
  Ubar <- Reduce(`+`, lapply(fits, function(f) f$U)) / m
  Qbar <- colMeans(Q)
  B <- stats::cov(Q)
  Tvar <- Ubar + (1 + 1 / m) * B
  dfcom <- n - 2

  pool_contrast <- function(a) {
    est <- sum(a * Qbar)
    ubar <- drop(t(a) %*% Ubar %*% a)
    b <- drop(t(a) %*% B %*% a)
    rubin_scalar(est, ubar, b, m, dfcom, conf)
  }

  dc <- pool_contrast(c(1, 0))
  de <- pool_contrast(c(0, 1))
  summary_tbl <- data.frame(
    Metric = c("Delta Cost", "Delta Effect", "ICER"),
    Estimate = c(dc$est, de$est, dc$est / de$est),
    StdError = c(dc$se, de$se, NA_real_),
    Lower = c(dc$lower, de$lower, NA_real_),
    Upper = c(dc$upper, de$upper, NA_real_),
    df = c(dc$df, de$df, NA_real_),
    FMI = c(dc$fmi, de$fmi, NA_real_),
    stringsAsFactors = FALSE
  )

  inb <- lapply(wtp_range, function(k) pool_contrast(c(-1, k)))
  ceac <- data.frame(
    WTP = wtp_range,
    ENMB = vapply(inb, function(z) z$est, numeric(1)),
    SE = vapply(inb, function(z) z$se, numeric(1)),
    Lower = vapply(inb, function(z) z$lower, numeric(1)),
    Upper = vapply(inb, function(z) z$upper, numeric(1)),
    Prob_CE = vapply(inb, function(z) {
      if (z$se == 0) return(as.numeric(z$est > 0))
      stats::pt(z$est / z$se, df = z$df)
    }, numeric(1))
  )
  attr(ceac, "formula") <- formula
  class(ceac) <- c("nmb_ceac", "data.frame")

  colnames(Q) <- c("DeltaCost", "DeltaEffect")
  structure(
    list(
      summary = summary_tbl,
      ceac = ceac,
      estimates = Q,
      Qbar = Qbar,
      Ubar = Ubar,
      B = B,
      Tvar = Tvar,
      m = m,
      n = n,
      ref = ref,
      treatment = fits[[1]]$treatment,
      formula = formula,
      call = match.call()
    ),
    class = "micea"
  )
}

#' @export
summary.micea <- function(object, ...) {
  object$summary
}

#' @export
print.micea <- function(x, ...) {
  cat("Cost-effectiveness analysis pooled over ", x$m, " imputations\n",
      sep = "")
  cat("Reference: ", x$ref, "; treatment: ", x$treatment, "; n = ", x$n, "\n\n",
      sep = "")
  print(round_df(x$summary, 3), row.names = FALSE)
  invisible(x)
}

# Convert supported multiple-imputation objects to a list of data frames.
as_completed_list <- function(x) {
  if (inherits(x, "mimar_imputation")) {
    return(lapply(x$imputations, as.data.frame))
  }
  if (inherits(x, "mids")) {
    if (!requireNamespace("mice", quietly = TRUE)) {
      stop("Package 'mice' is needed for `mids` input.", call. = FALSE)
    }
    return(lapply(mice::complete(x, action = "all"), as.data.frame))
  }
  if (is.data.frame(x)) return(list(x))
  if (is.list(x) && length(x) > 0L && all(vapply(x, is.data.frame, logical(1)))) {
    return(lapply(x, as.data.frame))
  }
  stop("`data` must be a `mimar_imputation`, a `mids`, or a list of data frames.",
       call. = FALSE)
}

# Incremental cost and effect with their 2 x 2 sampling covariance matrix in
# one complete data set, optionally weighted.
ce_fit <- function(formula, data, ref, weights = NULL) {
  ce_data <- parse_ce_formula(formula, data, ref = ref, require_group = TRUE,
                              na.omit = FALSE)
  if (anyNA(ce_data$cost) || anyNA(ce_data$effect) || anyNA(ce_data$group)) {
    stop("Completed data sets must not contain missing cost, effect, or group.",
         call. = FALSE)
  }
  w <- if (is.null(weights)) {
    rep(1, nrow(ce_data))
  } else {
    if (!weights %in% names(data)) {
      stop("Weight column `", weights, "` not found in data.", call. = FALSE)
    }
    as.numeric(data[[weights]])
  }
  if (anyNA(w) || any(w < 0)) {
    stop("Weights must be non-negative and non-missing.", call. = FALSE)
  }

  arm_moments <- function(idx) {
    wi <- w[idx]
    keep <- wi > 0
    wi <- wi[keep]
    y <- cbind(ce_data$cost[idx][keep], ce_data$effect[idx][keep])
    k <- length(wi)
    if (k < 2L) stop("Each group needs at least two positively weighted rows.",
                     call. = FALSE)
    p <- wi / sum(wi)
    mu <- colSums(p * y)
    r <- sweep(y, 2, mu)
    V <- crossprod(r * p) * k / (k - 1)
    list(mu = mu, V = V)
  }

  trt <- ce_data$group != ref
  a1 <- arm_moments(which(trt))
  a0 <- arm_moments(which(!trt))
  theta <- a1$mu - a0$mu
  names(theta) <- c("delta_cost", "delta_effect")
  list(theta = theta, U = a1$V + a0$V, n = nrow(ce_data),
       treatment = as.character(unique(ce_data$group[trt])))
}

# Rubin's rules for one scalar with Barnard-Rubin degrees of freedom.
rubin_scalar <- function(est, ubar, b, m, dfcom, conf = 0.95) {
  tvar <- ubar + (1 + 1 / m) * b
  if (b <= 0) {
    df <- dfcom
    fmi <- 0
  } else {
    lambda <- (1 + 1 / m) * b / tvar
    df_old <- (m - 1) / lambda^2
    df_obs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
    df <- df_old * df_obs / (df_old + df_obs)
    r <- (1 + 1 / m) * b / ubar
    fmi <- (r + 2 / (df + 3)) / (r + 1)
  }
  se <- sqrt(tvar)
  q <- stats::qt(1 - (1 - conf) / 2, df = df)
  list(est = est, se = se, lower = est - q * se, upper = est + q * se,
       df = df, fmi = fmi)
}
