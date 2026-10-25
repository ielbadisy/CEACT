#' Bootstrap-Then-Impute Cost-Effectiveness Analysis
#'
#' Resamples the incomplete data within each group, imputes every bootstrap
#' sample with a user-supplied imputation function, and estimates the
#' incremental cost and effect on each completed sample (averaged over the
#' imputations of that sample). The point estimate is the mean of the bootstrap
#' estimates and intervals are percentile intervals. Bootstrapping before
#' imputing propagates imputation uncertainty into the bootstrap distribution,
#' so the result can be passed directly to [compute_nmb_ceac()],
#' [plot_ceplane()], and [plot_ceac()].
#'
#' @param formula A formula of the form `cost + effect ~ group`.
#' @param data A data frame with missing values in cost, effect, or
#'   covariates. The group variable must be complete.
#' @param ref Reference group label.
#' @param imputefun A function taking one data frame (a bootstrap sample) and
#'   returning its completed version: a data frame, a list of data frames, a
#'   `mimar_imputation`, or a `mids` object. With several completed data sets
#'   the bootstrap estimate is their mean. If the function adds a weight column
#'   (for example after matching each completed sample), name it in `weights`.
#' @param R Number of bootstrap replications.
#' @param weights Optional name of a case-weight column in each completed data
#'   set.
#' @param conf Confidence level of the percentile intervals.
#' @param ncores Number of cores used through [parallel::mclapply()]
#'   (Unix-alikes only; ignored on Windows).
#' @param seed Optional seed. Results are reproducible for a given `seed` and
#'   `ncores`.
#'
#' @return An object of class `c("miboot", "boot_icer")` with the same
#'   components as [boot_icer()] (`summary`, `boot_dist`, `observed`,
#'   `formula`, `ref`, `call`) plus `R`, `failed` (number of replications in
#'   which imputation or estimation failed and was dropped), and `m` (completed
#'   data sets per replication).
#'
#' @references
#' Brand J, van Buuren S, le Cessie S, van den Hout W. Combining multiple
#' imputation and bootstrap in the analysis of cost-effectiveness trial data.
#' Stat Med. 2019;38(2):210-220.
#'
#' Schomaker M, Heumann C. Bootstrap inference when using multiple imputation.
#' Stat Med. 2018;37(14):2252-2266.
#'
#' @examples
#' df <- simulate_ce_trial(n = 60, seed = 123)
#' df$cost[sample(nrow(df), 20)] <- NA
#' hotdeck <- function(d) {
#'   for (g in levels(d$group)) {
#'     i <- d$group == g
#'     miss <- i & is.na(d$cost)
#'     d$cost[miss] <- sample(d$cost[i & !is.na(d$cost)], sum(miss),
#'                            replace = TRUE)
#'   }
#'   d
#' }
#' res <- miboot(cost + effect ~ group, data = df, ref = "control",
#'               imputefun = hotdeck, R = 50, seed = 1)
#' summary(res)
#'
#' @export
miboot <- function(formula, data, ref, imputefun, R = 1000, weights = NULL,
                   conf = 0.95, ncores = 1L, seed = NULL) {
  if (!is.function(imputefun)) {
    stop("`imputefun` must be a function.", call. = FALSE)
  }
  data <- as.data.frame(data)
  ce_check <- parse_ce_formula(formula, data, ref = ref, require_group = TRUE,
                               na.omit = FALSE)
  if (anyNA(ce_check$group)) {
    stop("The group variable must not contain missing values.", call. = FALSE)
  }

  if (!is.null(seed)) {
    old_kind <- RNGkind()[1]
    RNGkind("L'Ecuyer-CMRG")
    on.exit(RNGkind(old_kind), add = TRUE)
    set.seed(seed)
  }

  groups <- ce_check$group
  strata <- split(seq_len(nrow(data)), groups)
  idx <- lapply(seq_len(R), function(r) {
    unlist(lapply(strata, function(s) s[sample.int(length(s), replace = TRUE)]),
           use.names = FALSE)
  })

  one_rep <- function(i) {
    tryCatch({
      imps <- as_completed_list(imputefun(data[i, , drop = FALSE]))
      th <- vapply(imps, function(d) ce_fit(formula, d, ref, weights)$theta,
                   numeric(2))
      c(rowMeans(matrix(th, nrow = 2)), length(imps))
    }, error = function(e) c(NA_real_, NA_real_, NA_real_))
  }

  use_fork <- ncores > 1L && .Platform$OS.type != "windows"
  reps <- if (use_fork) {
    parallel::mclapply(idx, one_rep, mc.cores = ncores, mc.set.seed = TRUE)
  } else {
    lapply(idx, one_rep)
  }
  reps <- do.call(rbind, reps)
  ok <- stats::complete.cases(reps)
  failed <- sum(!ok)
  if (sum(ok) < 2L) {
    stop("Fewer than two bootstrap replications succeeded.", call. = FALSE)
  }

  boot_dist <- cbind(reps[ok, 1], reps[ok, 2], reps[ok, 1] / reps[ok, 2])
  colnames(boot_dist) <- c("DeltaCost", "DeltaEffect", "ICER")
  observed <- c(DeltaCost = mean(boot_dist[, 1]),
                DeltaEffect = mean(boot_dist[, 2]))
  observed <- c(observed, ICER = unname(observed[1] / observed[2]))

  probs <- c((1 - conf) / 2, 1 - (1 - conf) / 2)
  ci <- lapply(1:3, function(j) stats::quantile(boot_dist[, j], probs,
                                                names = FALSE, na.rm = TRUE))
  summary_tbl <- data.frame(
    Metric = c("Delta Cost", "Delta Effect", "ICER"),
    Observed = round(observed, 3),
    BootstrapMean = round(colMeans(boot_dist, na.rm = TRUE), 3),
    StdError = round(apply(boot_dist, 2, stats::sd, na.rm = TRUE), 3),
    Bias = round(colMeans(boot_dist, na.rm = TRUE) - observed, 3),
    CI = vapply(ci, format_interval, character(1)),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      summary = summary_tbl,
      boot_dist = boot_dist,
      observed = observed,
      boot = NULL,
      formula = formula,
      ref = ref,
      R = R,
      failed = failed,
      m = unname(stats::median(reps[ok, 3])),
      call = match.call()
    ),
    class = c("miboot", "boot_icer")
  )
}
