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
#' The ICER point estimate is the observed ratio of mean differences. The ICER
#' has no bootstrap mean, standard error or bias: a mean of ratios is unstable
#' when the incremental effect is near zero. Its interval is the percentile
#' interval of the bootstrap ratios, reported only when all replicates lie in
#' the NE or in the SW quadrant of the cost-effectiveness plane. Otherwise it is
#' `NA`, a note is attached, and uncertainty should be read from the CEAC or the
#' INMB ([computenmbceac()]). The share of replicates in each quadrant is
#' returned in `quadrants`.
#'
#' @return An object of class `boot_icer` containing a summary table
#'   (`summary`), the bootstrap distribution (`boot_dist`), the observed
#'   estimates (`observed`), the share of replicates per quadrant
#'   (`quadrants`), a `note` on the ICER interval (or `NULL`), the `boot`
#'   object, formula, reference group, and matched call.
#'
#' @examples
#' df <- simulatecetrial(n = 100, seed = 123)
#' res <- booticer(cost + effect ~ group, data = df, ref = "control", R = 200)
#' summary(res)
#'
#' @export
booticer <- function(formula, data, ref, R = 1000, ci.type = "bca",
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
  quadrants <- ce_quadrants(bt$t[, 1], bt$t[, 2])
  icer <- icer_interval(bt$t[, 1], bt$t[, 2], quadrants)

  summary_tbl <- boot_summary_table(bt$t0, bt$t, ci_dc, ci_de, icer$ci)

  structure(
    list(
      summary = .as_bt(summary_tbl),
      boot_dist = bt$t,
      observed = bt$t0,
      quadrants = quadrants,
      note = icer$note,
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
  if (!is.null(object$note)) message(object$note)
  object$summary
}

#' @export
print.boot_icer <- function(x, ...) {
  print(x$summary, ...)
  cat("\nShare of replicates by quadrant:\n")
  q <- x$quadrants
  if (!is.null(q)) {
    cat(paste0("  ", q$Quadrant, " (", q$Description, "): ",
               formatC(100 * q$Share, format = "f", digits = 1), "%"),
        sep = "\n")
  }
  if (!is.null(x$note)) cat("\nNote:", x$note, "\n")
  invisible(x)
}
