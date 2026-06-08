#' Bootstrap Incremental Cost, Effect, ICER, and Net Benefit
#'
#' Performs non-parametric stratified bootstrap resampling for a two-arm
#' trial-based economic evaluation. Resampling is stratified by trial group to
#' preserve arm sizes.
#'
#' @param formula A formula of the form `cost + effect ~ group`.
#' @param data A data frame containing the variables in the formula.
#' @param ref Reference group label.
#' @param R Number of bootstrap replications.
#' @param ci.type Confidence interval type passed to [boot::boot.ci()].
#' @param na.omit Logical; whether to remove rows with missing values.
#'
#' @return An object of class `boot_icer` containing a summary table,
#'   bootstrap distribution, observed estimates, formula, reference group, and
#'   matched call.
#'
#' @examples
#' df <- simulate_ce_trial(n = 100, seed = 123)
#' res <- boot_icer(cost + effect ~ group, data = df, ref = "control", R = 200)
#' summary(res)
#'
#' @export
boot_icer <- function(formula, data, ref, R = 1000, ci.type = "bca",
                      na.omit = TRUE) {
  ce_data <- parse_ce_formula(formula, data, ref = ref, require_group = TRUE,
                              na.omit = na.omit)

  stat_func <- function(d, i) {
    boot_data <- d[i, , drop = FALSE]
    ce_deltas(boot_data, ref)
  }

  bt <- boot::boot(
    data = ce_data,
    statistic = stat_func,
    R = R,
    strata = ce_data$group
  )
  colnames(bt$t) <- c("DeltaCost", "DeltaEffect", "ICER")
  names(bt$t0) <- c("DeltaCost", "DeltaEffect", "ICER")

  ci_dc <- extract_boot_ci(bt, 1, type = ci.type)
  ci_de <- extract_boot_ci(bt, 2, type = ci.type)
  ci_icer <- extract_boot_ci(bt, 3, type = ci.type)

  summary_tbl <- data.frame(
    Metric = c("Delta Cost", "Delta Effect", "ICER"),
    Observed = round(bt$t0, 3),
    BootstrapMean = round(colMeans(bt$t, na.rm = TRUE), 3),
    StdError = round(apply(bt$t, 2, stats::sd, na.rm = TRUE), 3),
    Bias = round(colMeans(bt$t, na.rm = TRUE) - bt$t0, 3),
    CI = c(format_interval(ci_dc), format_interval(ci_de),
           format_interval(ci_icer)),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      summary = summary_tbl,
      boot_dist = bt$t,
      observed = bt$t0,
      boot = bt,
      formula = formula,
      ref = ref,
      call = match.call()
    ),
    class = "boot_icer"
  )
}

#' @export
summary.boot_icer <- function(object, ...) {
  if (!inherits(object, "boot_icer")) stop("Object must be of class 'boot_icer'")
  object$summary
}
