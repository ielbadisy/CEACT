#' Bootstrap-Then-Impute Cost-Effectiveness Analysis
#'
#' Resamples the incomplete data within each group, imputes every bootstrap
#' sample with a user-supplied imputation function, and estimates the
#' incremental cost and effect on each completed sample (averaged over the
#' imputations of that sample). The observed estimate is the Rubin point
#' estimate (mean over imputations) from the original data completed once by
#' `imputefun`; intervals for the incremental cost and effect are percentile
#' intervals. The ICER is handled as in [booticer()]: observed ratio of mean
#' differences, no bootstrap mean or bias, and a percentile interval only when
#' all replicates lie in the NE or in the SW quadrant. Bootstrapping before
#' imputing propagates imputation uncertainty into the bootstrap distribution,
#' so the result can be passed directly to [computenmbceac()],
#' [plotceplane()], and [plotceac()].
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
#'   components as [booticer()] (`summary`, `boot_dist`, `observed`,
#'   `quadrants`, `note`, `formula`, `ref`, `call`) plus `R`, `failed` (number of replications in
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
#' df <- simulatecetrial(n = 60, seed = 123)
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

  # Observed estimate: Rubin point estimate (mean over imputations) from the
  # original data imputed once with `imputefun`.
  imps0 <- as_completed_list(imputefun(data))
  th0 <- vapply(imps0, function(d) ce_fit(formula, d, ref, weights)$theta,
                numeric(2))
  th0 <- rowMeans(matrix(th0, nrow = 2))
  observed <- c(DeltaCost = th0[1], DeltaEffect = th0[2],
                ICER = th0[1] / th0[2])

  probs <- c((1 - conf) / 2, 1 - (1 - conf) / 2)
  ci <- lapply(1:2, function(j) stats::quantile(boot_dist[, j], probs,
                                                names = FALSE))
  quadrants <- ce_quadrants(boot_dist[, 1], boot_dist[, 2])
  icer <- icer_interval(boot_dist[, 1], boot_dist[, 2], quadrants, conf)
  summary_tbl <- boot_summary_table(observed, boot_dist, ci[[1]], ci[[2]],
                                    icer$ci)

  structure(
    list(
      summary = .as_bt(summary_tbl),
      boot_dist = boot_dist,
      observed = observed,
      quadrants = quadrants,
      note = icer$note,
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
