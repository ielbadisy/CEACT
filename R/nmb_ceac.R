

## compute NMB
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

