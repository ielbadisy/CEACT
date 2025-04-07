#' Plot Cost-Effectiveness Acceptability Curve (CEAC)
#'
#' This function plots the CEAC, which displays the probability that an intervention is cost-effective
#' compared to a reference, across a range of willingness-to-pay (WTP) thresholds. It can take as input
#' either a `boot_icer` object or a formula interface using cost and effect variables.
#'
#' @param x Either a result object from [boot_icer()], or a formula of the form `cost + effect ~ group`
#'   or `cost + effect ~ 1` (in this case, `group` is ignored and the comparison is made internally).
#' @param data A data frame, required if `x` is a formula.
#' @param wtp_range A numeric vector of WTP thresholds (default: `seq(0, 100000, 1000)`).
#' @param ... Additional arguments passed to `ggplot2::labs()` (e.g. `subtitle`).
#'
#' @return A `ggplot` object displaying the CEAC.
#'
#' @details
#' When using a formula, the cost and effect variables must appear on the left-hand side (e.g., `cost + effect ~ group`).
#' If `x` is a `boot_icer` object, the CEAC is based on the bootstrapped distribution of incremental cost and effect.
#'
#' @examples
#' # Example with boot_icer object
#' set.seed(123)
#' df <- data.frame(
#'   c = c(rnorm(100, 500, 100), rnorm(100, 200, 100)),
#'   e = c(rnorm(100, 0.6, 0.05), rnorm(100, 0.65, 0.06)),
#'   g = rep(c("control", "treatment"), each = 100)
#' )
#'
#' res <- boot_icer(c + e ~ g, data = df, ref = "control", R = 500)
#' plot_ceac(res, wtp_range = seq(0, 20000, 500))
#'
#' # Example with only cost and effect
#' plot_ceac(c + e ~ 1, data = df, wtp_range = seq(0, 20000, 500))
#'
#' @export
plot_ceac <- function(x, data = NULL, wtp_range = seq(0, 100000, 1000), ...) {
  if (inherits(x, "boot_icer")) {
    cost <- x$boot_dist[, 1]
    effect <- x$boot_dist[, 2]
  } else if (inherits(x, "formula")) {
    if (is.null(data)) stop("If using a formula, you must supply a data frame.")

    # Parse formula and extract cost + effect
    terms_obj <- terms(x)
    vars <- all.vars(terms_obj)
    if (length(vars) < 2) stop("Formula must be of the form: cost + effect ~ group or cost + effect ~ 1")

    cost <- eval(parse(text = vars[1]), envir = data)
    effect <- eval(parse(text = vars[2]), envir = data)
  } else {
    stop("x must be either a 'boot_icer' object or a formula.")
  }

  # Compute CEAC
  ceac_df <- data.frame()
  for (wtp in wtp_range) {
    inb <- wtp * effect - cost
    prob_ce <- mean(inb > 0)
    ceac_df <- rbind(ceac_df, data.frame(WTP = wtp, Prob_CE = prob_ce))
  }

  # Plot
  ggplot2::ggplot(ceac_df, ggplot2::aes(x = WTP, y = Prob_CE)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_point(color = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Cost-Effectiveness Acceptability Curve",
      subtitle = "Probability of being cost-effective across WTP thresholds",
      x = "Willingness-to-Pay (WTP)",
      y = "Probability Cost-Effective",
      ...
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
}



#*******************************************************************************

## Example
#df <- data.frame(
  #c = c(rnorm(100, 500, 1000), rnorm(100, 200, 420)),
  #e = c(rnorm(100, 0.6, 0.5), rnorm(100, 0.65, 0.06)),
  #g = rep(c("control", "treatment"), each = 100)
)

# 1. using boot_icer object
#res <- boot_icer(c + e ~ g, data = df, ref = "control", R = 500)
#plot_ceac(res)

# 2. using only cost and effect without group
#plot_ceac(c + e ~ 1, data = df)
