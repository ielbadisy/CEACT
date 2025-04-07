#' Bootstrap Estimation of Incremental Cost-Effectiveness Ratio (ICER)
#'
#' Performs non-parametric bootstrap resampling to estimate the distribution of 
#' the Incremental Cost-Effectiveness Ratio (ICER) between a treatment and control group.
#'
#' This function takes a formula of the form `cost + effect ~ group` and computes 
#' bootstrap replicates of incremental cost and effect differences, as well as their ratio (ICER).
#' Confidence intervals for each component are derived using the bias-corrected and accelerated (BCa) method.
#'
#' @param formula A formula of the form `cost + effect ~ group`, where `cost` and `effect` are numeric variables, 
#' and `group` is a factor variable indicating treatment assignment.
#' @param data A data frame containing the variables in the formula.
#' @param ref The reference group label in the `group` variable (typically "control").
#' @param R Number of bootstrap replications. Default is 1000.
#' @param ci.type Type of confidence interval to compute with `boot.ci()`. Default is `"bca"`.
#'
#' @return An object of class `boot_icer`, which contains:
#' \describe{
#'   \item{summary}{A data frame with estimates, standard errors, bias, and confidence intervals for Delta Cost, Delta Effect, and ICER.}
#'   \item{boot_dist}{A matrix of bootstrap replicates: one row per sample, with columns for Delta Cost, Delta Effect, and ICER.}
#'   \item{formula}{The original formula used.}
#'   \item{ref}{The reference group.}
#'   \item{call}{The matched call.}
#' }
#' 
#' The object supports a custom `summary()` method.
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   c = c(rnorm(100, 500, 100), rnorm(100, 600, 120)),
#'   e = c(rnorm(100, 0.6, 0.05), rnorm(100, 0.65, 0.06)),
#'   g = rep(c("control", "treatment"), each = 100)
#' )
#' res <- boot_icer(c + e ~ g, data = df, ref = "control", R = 500)
#' summary(res)
#'
#' @export
boot_icer <- function(formula, data, ref, R = 1000, ci.type = "bca") {
  terms_obj <- terms(formula)
  vars <- all.vars(terms_obj)
  if (length(vars) < 3) stop("Formula must be of the form: cost + effect ~ group")

  cost   <- vars[1]
  effect <- vars[2]
  group  <- vars[3]

  data <- data[, c(cost, effect, group)]
  colnames(data) <- c("cost", "effect", "group")

  stat_func <- function(d, i) {
    d <- d[i, ]
    delta_cost <- mean(d$cost[d$group != ref]) - mean(d$cost[d$group == ref])
    delta_effect <- mean(d$effect[d$group != ref]) - mean(d$effect[d$group == ref])
    ICER <- delta_cost / delta_effect
    c(delta_cost, delta_effect, ICER)
  }

  set.seed(1234)
  bt <- boot::boot(data, stat_func, R = R)

  ci_dc <- boot::boot.ci(bt, type = ci.type, index = 1)$bca[4:5]
  ci_de <- boot::boot.ci(bt, type = ci.type, index = 2)$bca[4:5]
  ci_icer <- boot::boot.ci(bt, type = ci.type, index = 3)$bca[4:5]

  summary_tbl <- data.frame(
    Metric = c("Delta Cost", "Delta Effect", "ICER"),
    Estimate = round(colMeans(bt$t), 3),
    Observed = round(bt$t0, 3),
    StdError = round(apply(bt$t, 2, sd), 3),
    Bias = round(colMeans(bt$t) - bt$t0, 3),
    CI = c(paste0("[", round(ci_dc[1], 3), ";", round(ci_dc[2], 3), "]"),
           paste0("[", round(ci_de[1], 3), ";", round(ci_de[2], 3), "]"),
           paste0("[", round(ci_icer[1], 3), ";", round(ci_icer[2], 3), "]"))
  )

  structure(
    list(
      summary = summary_tbl,
      boot_dist = bt$t,
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


#******************************************************************************

## example
#set.seed(123)
#df <- data.frame(
  #c = c(rnorm(100, 500, 100), rnorm(100, 600, 120)),
  #e = c(rnorm(100, 0.6, 0.05), rnorm(100, 0.65, 0.06)),
  #g = rep(c("control", "treatment"), each = 100)
#)
#res <- boot_icer(c + e ~ g, data = df, ref = "control", R = 500)

#summary(res)
