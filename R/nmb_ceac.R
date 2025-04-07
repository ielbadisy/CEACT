#' Compute Net Monetary Benefit (NMB) and CEAC Table
#'
#' Computes the expected Net Monetary Benefit (ENMB) and the probability of cost-effectiveness
#' for a range of Willingness-To-Pay (WTP) thresholds. This function is useful for generating
#' Cost-Effectiveness Acceptability Curves (CEAC).
#'
#' @param formula A formula of the form `cost + effect ~ 1`, where:
#'   - `cost` is the numeric column for cost,
#'   - `effect` is the numeric column for effectiveness (e.g., QALYs).
#'   The right-hand side `~ 1` is ignored but required for consistency.
#' @param data A data frame containing the variables used in the formula.
#' @param wtp_range A numeric vector of WTP thresholds (e.g., `seq(0, 100000, 1000)`).
#'
#' @return An object of class `"nmb_ceac"`, which is a `data.frame` with columns:
#' \describe{
#'   \item{WTP}{Willingness-to-pay threshold}
#'   \item{ENMB}{Expected Net Monetary Benefit at each WTP}
#'   \item{Prob_CE}{Probability of being cost-effective at each WTP}
#' }
#' The object also contains the original formula as an attribute.
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   c = rnorm(100, 500, 100),
#'   e = rnorm(100, 0.6, 0.05)
#' )
#' ceac_tbl <- compute_nmb_ceac(c + e ~ 1, data = df, wtp_range = seq(0, 2000, 200))
#' summary(ceac_tbl)
#'
#' @export
compute_nmb_ceac <- function(formula, data, wtp_range = seq(0, 100000, 1000)) {
  terms_obj <- terms(formula)
  vars <- all.vars(terms_obj)

  if (length(vars) < 2) {
    stop("Formula must contain at least two variables on the left-hand side (e.g., cost + effect ~ 1)")
  }

  cost   <- vars[1]
  effect <- vars[2]

  cost_vals   <- data[[cost]]
  effect_vals <- data[[effect]]

  ENMB <- vapply(wtp_range, function(wtp) mean(wtp * effect_vals - cost_vals), numeric(1))
  Prob_CE <- vapply(wtp_range, function(wtp) mean((wtp * effect_vals - cost_vals) > 0), numeric(1))

  out <- data.frame(WTP = wtp_range, ENMB = ENMB, Prob_CE = Prob_CE)
  attr(out, "formula") <- formula
  class(out) <- c("nmb_ceac", "data.frame")
  return(out)
}


#' @export
summary.nmb_ceac <- function(object, ...) {
  cat("Net Monetary Benefit (NMB) Summary\n")
  cat("Formula: ", deparse(attr(object, "formula")), "\n")
  cat("WTP Range: ", min(object$WTP), "to", max(object$WTP), "\n\n")
  print(head(object, 10))
}

#*******************************************************************************

## example
#df <- data.frame(
  #c = rnorm(100, 500, 100),
  #e = rnorm(100, 0.6, 0.05)
#)
#ceac_tbl <- compute_nmb_ceac(c + e ~ 1, data = df, wtp_range = seq(0, 2000, 200))
#summary(ceac_tbl)

