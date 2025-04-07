#' Plot Cost-Effectiveness Plane
#'
#' This function visualizes the cost-effectiveness plane, showing incremental costs vs effects
#' from the bootstrap distribution.
#'
#' @name plot_ceplane
#' @param boot_icer_result A `boot_icer` object from [boot_icer()].
#' @param k Optional slope (willingness-to-pay threshold).
#' @param subtitle Optional subtitle text.
#'
#' @return A `ggplot` object displaying the cost-effectiveness plane.
#' @export
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   c = c(rnorm(100, 500, 100), rnorm(100, 600, 120)),
#'   e = c(rnorm(100, 0.6, 0.05), rnorm(100, 0.65, 0.06)),
#'   g = rep(c("control", "treatment"), each = 100)
#' )
#' res <- boot_icer(c + e ~ g, data = df, ref = "control", R = 300)
#' plot_ceplane(res, k = 1000)



utils::globalVariables(c("IncrementalEffect", "IncrementalCost")) 

plot_ceplane <- function(boot_icer_result, k = NULL, subtitle = NULL) {
  if (!"boot_dist" %in% names(boot_icer_result)) stop("Input must be result from boot_icer()")

  data <- data.frame(
    IncrementalCost = boot_icer_result$boot_dist[, 1],
    IncrementalEffect = boot_icer_result$boot_dist[, 2]
  )

  # define axis limits
  max_x <- max(abs(data$IncrementalEffect)) * 1.1
  max_y <- max(abs(data$IncrementalCost)) * 1.1

  x_breaks <- pretty(c(-max_x * 1.3, max_x * 1.3), n = 10)
  y_breaks <- pretty(c(-max_y * 1.3, max_y * 1.3), n = 10)

  # start plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = IncrementalEffect, y = IncrementalCost)) +
    ggplot2::geom_point(alpha = 0.5, color = "black") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")

  # add WTP line only if k is not NULL
  if (!is.null(k)) {
    p <- p + ggplot2::geom_abline(slope = k, intercept = 0, color = "red", size = 1)
    subtitle_text <- if (is.null(subtitle)) paste0("WTP Threshold: ", k) else subtitle
  } else {
    subtitle_text <- subtitle
  }

  # finalize plot with customization
  p +
    ggplot2::scale_x_continuous(limits = c(min(x_breaks), max(x_breaks)), breaks = x_breaks) +
    ggplot2::scale_y_continuous(limits = c(min(y_breaks), max(y_breaks)), breaks = y_breaks) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Cost-Effectiveness Plane",
      subtitle = subtitle_text,
      x = "Incremental Effect",
      y = "Incremental Cost"
    )
}
