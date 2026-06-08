#' Plot Cost-Effectiveness Acceptability Curve
#'
#' Plots the probability that treatment is cost-effective across
#' willingness-to-pay thresholds.
#'
#' @param x A `boot_icer` object, `nmb_ceac` object, or formula of the form
#'   `cost + effect ~ group`.
#' @param data Data frame required when `x` is a formula.
#' @param wtp_range Numeric vector of willingness-to-pay thresholds.
#' @param ref Reference group label required when `x` is a formula.
#' @param R Number of bootstrap replications used when `x` is a formula.
#' @param ... Additional arguments passed to [ggplot2::labs()].
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' df <- simulate_ce_trial(n = 100, seed = 123)
#' res <- boot_icer(cost + effect ~ group, data = df, ref = "control", R = 200)
#' plot_ceac(res, wtp_range = seq(0, 50000, 5000))
plot_ceac <- function(x, data = NULL, wtp_range = seq(0, 100000, 1000),
                      ref = NULL, R = 1000, ...) {
  ceac_df <- if (inherits(x, "nmb_ceac")) {
    x
  } else {
    compute_nmb_ceac(x, data = data, wtp_range = wtp_range, ref = ref, R = R)
  }

  ggplot2::ggplot(ceac_df, ggplot2::aes(x = WTP, y = Prob_CE)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_point(color = "steelblue", size = 1.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Cost-Effectiveness Acceptability Curve",
      subtitle = "Probability that treatment is cost-effective",
      x = "Willingness-to-pay threshold",
      y = "Probability cost-effective",
      ...
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
}

utils::globalVariables(c("WTP", "Prob_CE"))
