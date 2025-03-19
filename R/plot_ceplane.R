# -------------------------------
# Plot Cost-Effectiveness Plane (boot_icer output directly)
# -------------------------------
plot_ceplane <- function(boot_icer_result, k = 500, subtitle = NULL) {
  if (!"boot_dist" %in% names(boot_icer_result)) stop("Input must be result from boot_icer()")

  data <- data.frame(
    IncrementalCost = boot_icer_result$boot_dist[, 1],
    IncrementalEffect = boot_icer_result$boot_dist[, 2]
  )

  max_x <- max(abs(data$IncrementalEffect)) * 1.1
  max_y <- max(abs(data$IncrementalCost)) * 1.1

  ggplot2::ggplot(data, ggplot2::aes(x = IncrementalEffect, y = IncrementalCost)) +
    ggplot2::geom_point(alpha = 0.5, color = "black") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_abline(slope = k, intercept = 0, color = "red", size = 1) +
    ggplot2::scale_x_continuous(limits = c(-max_x, max_x)) +
    ggplot2::scale_y_continuous(limits = c(-max_y, max_y)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Cost-Effectiveness Plane",
                  subtitle = if (is.null(subtitle)) paste0("WTP Threshold: ", k) else subtitle,
                  x = "Incremental Effect",
                  y = "Incremental Cost")
}
