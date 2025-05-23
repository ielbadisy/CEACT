#' Cost-Effectiveness Analysis Summary (Observed)
#'
#' Computes observed summary statistics for a cost-effectiveness analysis comparing two groups 
#' (typically control and treatment). This includes mean and standard deviation of cost and effect, 
#' differences (deltas), confidence intervals, p-values from t-tests, and the Incremental Cost-Effectiveness Ratio (ICER).
#'
#' @param formula A formula of the form `cost + effect ~ group`, where:
#'   - `cost` is the numeric column for cost,
#'   - `effect` is the numeric column for effectiveness or utility (e.g., QALYs),
#'   - `group` is a grouping variable with at least two levels.
#' @param data A data frame containing the variables used in the formula.
#' @param ref A character string specifying the reference group in the `group` variable (typically "control").
#' @param na.omit Logical; whether to remove rows with missing values. Default is `TRUE`.
#'
#' @return An object of class `cea`, which is a data frame with the following columns:
#' \describe{
#'   \item{Outcome}{"Mean Cost" or "Mean Effect"}
#'   \item{Control}{Mean and SD for the control group}
#'   \item{Treatment}{Mean and SD for the treatment group}
#'   \item{Delta}{Difference between treatment and control}
#'   \item{CI}{95% confidence interval for the difference}
#'   \item{p.value}{P-value from a t-test comparing groups}
#' }
#' The object also contains attributes for the ICER, formula, reference group, and matched call.
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   cost = c(rnorm(100, 500, 100), rnorm(100, 600, 120)),
#'   effect = c(rnorm(100, 0.6, 0.05), rnorm(100, 0.65, 0.06)),
#'   group = rep(c("control", "treatment"), each = 100)
#' )
#' res <- cea(cost + effect ~ group, data = df, ref = "control")
#' print(res)
#' summary(res)
#'
#' @export
cea <- function(formula, data, ref, na.omit = TRUE) {
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

  c_mean <- c(mean(ctrl_data[[cost]]), mean(trt_data[[cost]]))
  e_mean <- c(mean(ctrl_data[[effect]]), mean(trt_data[[effect]]))
  c_sd   <- c(sd(ctrl_data[[cost]]), sd(trt_data[[cost]]))
  e_sd   <- c(sd(ctrl_data[[effect]]), sd(trt_data[[effect]]))

  delta_c <- c_mean[2] - c_mean[1]
  delta_e <- e_mean[2] - e_mean[1]
  ICER <- delta_c / delta_e

  t_cost <- t.test(trt_data[[cost]], ctrl_data[[cost]])
  t_eff  <- t.test(trt_data[[effect]], ctrl_data[[effect]])
  ci <- c(t_cost$conf.int, t_eff$conf.int)

  pvals <- c(t_cost$p.value, t_eff$p.value)
  pvals_fmt <- ifelse(pvals < 0.001, "<0.001", formatC(pvals, format = "f", digits = 4))

  result_table <- data.frame(
    Outcome = c("Mean Cost", "Mean Effect"),
    Control = c(paste0(round(c_mean[1], 2), " (sd ", round(c_sd[1], 2), ")"),
                paste0(round(e_mean[1], 2), " (sd ", round(e_sd[1], 2), ")")),
    Treatment = c(paste0(round(c_mean[2], 2), " (sd ", round(c_sd[2], 2), ")"),
                  paste0(round(e_mean[2], 2), " (sd ", round(e_sd[2], 2), ")")),
    Delta = round(c(delta_c, delta_e), 3),
    CI = c(paste0("[", round(ci[1], 2), ";", round(ci[2], 2), "]"),
           paste0("[", round(ci[3], 2), ";", round(ci[4], 2), "]")),
    p.value = pvals_fmt,
    stringsAsFactors = FALSE
  )

  structure(
    result_table,
    ICER = round(ICER, 3),
    formula = formula,
    ref = ref,
    call = match.call(),
    class = "cea"
  )
}

#' @export
summary.cea <- function(object, ...) {
  cat("Cost-Effectiveness Summary\n")
  cat("Formula: ", deparse(attr(object, "formula")), "\n")
  cat("Reference Group: ", attr(object, "ref"), "\n")
  cat("ICER:", attr(object, "ICER"), "\n\n")

  df <- as.data.frame(unclass(object))  # ensure it is printed correctly
  print(df)
  invisible(object)
}


#' @export
print.cea <- function(x, ...) {
  cat("Cost-Effectiveness Analysis Result\n")
  cat("Use `summary()` for details.\n")
  invisible(x)
}

#*******************************************************************************

# simulate example data
#set.seed(123)
#df <- data.frame(
  #cost = c(rnorm(100, 500, 100), rnorm(100, 600, 120)),
  #effect = c(rnorm(100, 0.6, 0.05), rnorm(100, 0.65, 0.06)),
  #group = rep(c("control", "treatment"), each = 100)
#)

#res <- cea(cost + effect ~ group, data = df, ref = "control")

#print(res)
#summary(res)
