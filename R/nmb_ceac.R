#' Compute Net Monetary Benefit and CEAC Table
#'
#' Computes expected incremental net monetary benefit (INMB) and the probability
#' that treatment is cost-effective across willingness-to-pay thresholds. For a
#' two-arm trial, `INMB(k) = k * DeltaEffect - DeltaCost`.
#'
#' @param x Either a `booticer` object or a formula of the form
#'   `cost + effect ~ group`.
#' @param data Data frame required when `x` is a formula.
#' @param wtp_range Numeric vector of willingness-to-pay thresholds.
#' @param ref Reference group label required when `x` is a formula.
#' @param R Number of bootstrap replications used when `x` is a formula.
#' @param ... Additional arguments passed to [booticer()] for formula input.
#'
#' @return An object of class `"nmb_ceac"` with columns `WTP`, `ENMB`, and
#'   `Prob_CE`.
#'
#' @examples
#' df <- simulatecetrial(n = 100, seed = 123)
#' ceac_tbl <- computenmbceac(cost + effect ~ group, data = df,
#'                              ref = "control", R = 200,
#'                              wtp_range = seq(0, 50000, 5000))
#' head(ceac_tbl)
#'
#' @export
computenmbceac <- function(x, data = NULL, wtp_range = seq(0, 100000, 1000),
                             ref = NULL, R = 1000, ...) {
  if (inherits(x, "boot_icer")) {
    boot_dist <- x$boot_dist
    observed <- x$observed
    formula <- x$formula
  } else if (inherits(x, "formula")) {
    if (is.null(data)) stop("`data` must be supplied when `x` is a formula.")
    if (is.null(ref)) stop("`ref` must be supplied when `x` is a formula.")
    boot_obj <- booticer(x, data = data, ref = ref, R = R, ...)
    boot_dist <- boot_obj$boot_dist
    observed <- boot_obj$observed
    formula <- x
  } else {
    stop("`x` must be a `booticer` object or formula.", call. = FALSE)
  }

  delta_cost <- boot_dist[, 1]
  delta_effect <- boot_dist[, 2]
  observed_dc <- observed[1]
  observed_de <- observed[2]

  out <- data.frame(
    WTP = wtp_range,
    ENMB = wtp_range * observed_de - observed_dc,
    Prob_CE = vapply(
      wtp_range,
      function(k) mean(k * delta_effect - delta_cost > 0, na.rm = TRUE),
      numeric(1)
    )
  )
  attr(out, "formula") <- formula
  class(out) <- c("nmb_ceac", "basetable", "data.frame")
  out
}

#' @export
summary.nmb_ceac <- function(object, ...) {
  cat("Net Monetary Benefit and CEAC Summary\n")
  cat("Formula: ", deparse(attr(object, "formula")), "\n", sep = "")
  cat("WTP range: ", min(object$WTP), " to ", max(object$WTP), "\n\n", sep = "")
  print(utils::head(object, 10))
  invisible(object)
}
