#' Observed Cost-Effectiveness Summary
#'
#' Computes observed cost-effectiveness outcomes between treatment and control groups,
#' including means, SDs, deltas, confidence intervals, p-values, and ICER.
#'
#' @param formula A formula like `cost + effect ~ group`, where the left-hand side contains
#' cost and effect variables, and the right-hand side is the grouping variable.
#' @param data A data frame containing the variables in the formula.
#' @param ref Character string specifying the reference (control) group label.
#' @param na.omit Logical indicating whether to omit missing values. Default is TRUE.
#'
#' @return A tidy data frame summarizing cost-effectiveness outcomes. ICER is returned as an attribute.
#'
#' @examples
#' data(acupuncture)
#' cea(cost + qaly ~ group, data = acupuncture, ref = "control")
#'
#' @export
cea <- function(formula, data, ref, na.omit = TRUE) {
  # Parse the formula
  terms_obj <- terms(formula)
  vars <- all.vars(terms_obj)
  if (length(vars) < 3) stop("Formula must be of the form: cost + effect ~ group")

  cost   <- vars[1]
  effect <- vars[2]
  group  <- vars[3]

  if (na.omit) data <- na.omit(data)

  data[[group]] <- as.factor(data[[group]])
  ctrl_data <- data[data[[group]] == ref, ]
  trt_data  <- data[data[[group]] != ref, ]

  # Means and SDs
  c_mean <- c(mean(ctrl_data[[cost]]), mean(trt_data[[cost]]))
  e_mean <- c(mean(ctrl_data[[effect]]), mean(trt_data[[effect]]))
  c_sd   <- c(sd(ctrl_data[[cost]]), sd(trt_data[[cost]]))
  e_sd   <- c(sd(ctrl_data[[effect]]), sd(trt_data[[effect]]))

  # Deltas and ICER
  delta_c <- c_mean[2] - c_mean[1]
  delta_e <- e_mean[2] - e_mean[1]
  ICER <- delta_c / delta_e

  # t-tests and CI
  t_cost <- t.test(trt_data[[cost]], ctrl_data[[cost]])
  t_eff  <- t.test(trt_data[[effect]], ctrl_data[[effect]])
  ci <- c(t_cost$conf.int, t_eff$conf.int)

  # Final Output
  out <- data.frame(
    Outcome = c("Mean Cost", "Mean Effect"),
    Control = c(paste0(round(c_mean[1], 2), " (sd ", round(c_sd[1], 2), ")"),
                paste0(round(e_mean[1], 2), " (sd ", round(e_sd[1], 2), ")")),
    Treatment = c(paste0(round(c_mean[2], 2), " (sd ", round(c_sd[2], 2), ")"),
                  paste0(round(e_mean[2], 2), " (sd ", round(e_sd[2], 2), ")")),
    Delta = round(c(delta_c, delta_e), 3),
    CI = c(paste0("[", round(ci[1], 2), ";", round(ci[2], 2), "]"),
           paste0("[", round(ci[3], 2), ";", round(ci[4], 2), "]")),
    p.value = round(c(t_cost$p.value, t_eff$p.value), 4)
  )

  attr(out, "ICER") <- round(ICER, 3)
  return(out)
}
