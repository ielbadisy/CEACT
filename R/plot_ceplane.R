#' Plot Cost-Effectiveness Plane
#'
#' Visualizes bootstrap replicates of incremental cost and incremental effect.
#'
#' @param boot_icer_result A `boot_icer` object from [boot_icer()].
#' @param k Optional willingness-to-pay threshold shown as a straight line with
#'   slope `k`.
#' @param subtitle Optional subtitle text.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' df <- simulate_ce_trial(n = 100, seed = 123)
#' res <- boot_icer(cost + effect ~ group, data = df, ref = "control", R = 200)
#' plot_ceplane(res, k = 20000)
plot_ceplane <- function(boot_icer_result, k = NULL, subtitle = NULL) {
  if (!inherits(boot_icer_result, "boot_icer")) {
    stop("Input must be a result from boot_icer().", call. = FALSE)
  }

  plot_data <- data.frame(
    IncrementalCost = boot_icer_result$boot_dist[, 1],
    IncrementalEffect = boot_icer_result$boot_dist[, 2]
  )

  max_x <- max(abs(plot_data$IncrementalEffect), na.rm = TRUE) * 1.1
  max_y <- max(abs(plot_data$IncrementalCost), na.rm = TRUE) * 1.1
  if (!is.finite(max_x) || max_x == 0) max_x <- 1
  if (!is.finite(max_y) || max_y == 0) max_y <- 1

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = IncrementalEffect, y = IncrementalCost)
  ) +
    ggplot2::geom_point(alpha = 0.45, color = "black", size = 1.5) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")

  if (!is.null(k)) {
    p <- p + ggplot2::geom_abline(slope = k, intercept = 0, color = "red",
                                  linewidth = 0.8)
    subtitle <- if (is.null(subtitle)) paste0("WTP threshold: ", k) else subtitle
  }

  p +
    ggplot2::coord_cartesian(xlim = c(-max_x, max_x), ylim = c(-max_y, max_y)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Cost-Effectiveness Plane",
      subtitle = subtitle,
      x = "Incremental effect",
      y = "Incremental cost"
    )
}

utils::globalVariables(c("IncrementalEffect", "IncrementalCost"))
