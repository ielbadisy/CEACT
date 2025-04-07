
## plot the CE plane

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
