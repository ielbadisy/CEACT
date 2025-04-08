#' Perform Deterministic Sensitivity Analysis on ICER or NMB
#'
#' This function varies a single parameter in the treatment group while keeping all others fixed,
#' and computes the resulting Incremental Cost-Effectiveness Ratio (ICER) or Net Monetary Benefit (NMB).
#'
#' @param formula A formula of the form `cost + effect ~ group`.
#' @param data A data frame.
#' @param param Name of the parameter to vary (e.g., "effect").
#' @param range A numeric vector of values for the parameter (e.g., seq(0.6, 0.8, 0.01)).
#' @param ref Character string specifying the reference group (e.g., "control").
#' @param metric Character string: either "ICER" or "NMB".
#' @param k Willingness-to-pay threshold (required for NMB).
#'
#' @return A data frame with the parameter values and resulting ICER or NMB.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   cost = c(rnorm(100, 500, 100), rnorm(100, 600, 120)),
#'   effect = c(rnorm(100, 0.6, 0.05), rnorm(100, 0.65, 0.06)),
#'   group = rep(c("control", "treatment"), each = 100)
#' )
#'
#' dsa_result <- dsa_icer(cost + effect ~ group, data = df, param = "effect",
#'                        range = seq(0.6, 0.7, 0.01), ref = "control", metric = "ICER")
dsa_icer <- function(formula, data, param, range, ref, metric = "ICER", k = 1000) {
  terms_obj <- terms(formula)
  vars <- all.vars(terms_obj)
  cost <- vars[1]; effect <- vars[2]; group <- vars[3]

  results <- data.frame()

  for (v in range) {
    df_mod <- data
    df_mod[df_mod[[group]] != ref, param] <- v

    ctrl <- df_mod[df_mod[[group]] == ref, ]
    trt <- df_mod[df_mod[[group]] != ref, ]

    delta_c <- mean(trt[[cost]]) - mean(ctrl[[cost]])
    delta_e <- mean(trt[[effect]]) - mean(ctrl[[effect]])

    val <- if (metric == "ICER") delta_c / delta_e else (k * delta_e - delta_c)
    results <- rbind(results, data.frame(Parameter = v, Value = val))
  }

  names(results)[2] <- metric
  attr(results, "metric") <- metric
  return(results)
}
