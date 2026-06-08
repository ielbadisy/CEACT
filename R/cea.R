#' Cost-Effectiveness Analysis Summary for a Two-Arm Trial
#'
#' Computes observed cost-effectiveness summaries comparing one treatment group
#' with one reference group. The incremental cost-effectiveness ratio is
#' `ICER = (mean(cost_treatment) - mean(cost_reference)) /
#' (mean(effect_treatment) - mean(effect_reference))`.
#'
#' @param formula A formula of the form `cost + effect ~ group`.
#' @param data A data frame containing the variables used in the formula.
#' @param ref Character string specifying the reference group.
#' @param na.omit Logical; whether to remove rows with missing values.
#'
#' @return An object of class `cea`, a data frame with group means, standard
#'   deviations, differences, confidence intervals, and p-values. Attributes
#'   include `ICER`, `delta_cost`, `delta_effect`, `formula`, `ref`, and `call`.
#'
#' @examples
#' df <- simulate_ce_trial(n = 100, seed = 123)
#' res <- cea(cost + effect ~ group, data = df, ref = "control")
#' summary(res)
#'
#' @export
cea <- function(formula, data, ref, na.omit = TRUE) {
  ce_data <- parse_ce_formula(formula, data, ref = ref, require_group = TRUE,
                              na.omit = na.omit)

  ctrl_data <- ce_data[ce_data$group == ref, , drop = FALSE]
  trt_data <- ce_data[ce_data$group != ref, , drop = FALSE]
  trt_name <- as.character(unique(trt_data$group))

  c_mean <- c(reference = mean(ctrl_data$cost), treatment = mean(trt_data$cost))
  e_mean <- c(reference = mean(ctrl_data$effect), treatment = mean(trt_data$effect))
  c_sd <- c(reference = stats::sd(ctrl_data$cost), treatment = stats::sd(trt_data$cost))
  e_sd <- c(reference = stats::sd(ctrl_data$effect), treatment = stats::sd(trt_data$effect))

  deltas <- ce_deltas(ce_data, ref)
  t_cost <- stats::t.test(trt_data$cost, ctrl_data$cost)
  t_eff <- stats::t.test(trt_data$effect, ctrl_data$effect)

  result_table <- data.frame(
    Outcome = c("Cost", "Effect"),
    Reference = c(
      paste0(round(c_mean[1], 3), " (SD ", round(c_sd[1], 3), ")"),
      paste0(round(e_mean[1], 3), " (SD ", round(e_sd[1], 3), ")")
    ),
    Treatment = c(
      paste0(round(c_mean[2], 3), " (SD ", round(c_sd[2], 3), ")"),
      paste0(round(e_mean[2], 3), " (SD ", round(e_sd[2], 3), ")")
    ),
    Difference = round(c(deltas["delta_cost"], deltas["delta_effect"]), 3),
    CI = c(format_interval(t_cost$conf.int), format_interval(t_eff$conf.int)),
    p.value = c(format_pval(t_cost$p.value), format_pval(t_eff$p.value)),
    stringsAsFactors = FALSE
  )

  structure(
    result_table,
    ICER = unname(deltas["ICER"]),
    delta_cost = unname(deltas["delta_cost"]),
    delta_effect = unname(deltas["delta_effect"]),
    treatment = trt_name,
    formula = formula,
    ref = ref,
    call = match.call(),
    class = c("cea", "data.frame")
  )
}

#' @export
summary.cea <- function(object, ...) {
  cat("Cost-Effectiveness Summary\n")
  cat("Formula: ", deparse(attr(object, "formula")), "\n", sep = "")
  cat("Reference group: ", attr(object, "ref"), "\n", sep = "")
  cat("Treatment group: ", attr(object, "treatment"), "\n", sep = "")
  cat("Incremental cost: ", round(attr(object, "delta_cost"), 3), "\n", sep = "")
  cat("Incremental effect: ", round(attr(object, "delta_effect"), 3), "\n", sep = "")
  cat("ICER: ", round(attr(object, "ICER"), 3), "\n\n", sep = "")
  print(as.data.frame(object))
  invisible(object)
}

#' @export
print.cea <- function(x, ...) {
  cat("Cost-Effectiveness Analysis Result\n")
  cat("Reference: ", attr(x, "ref"), "; treatment: ", attr(x, "treatment"), "\n",
      sep = "")
  cat("ICER: ", round(attr(x, "ICER"), 3), "\n", sep = "")
  invisible(x)
}
