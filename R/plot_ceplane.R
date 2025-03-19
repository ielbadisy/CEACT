# -------------------------------
# Plot Cost-Effectiveness Plane (boot_icer output directly)
# -------------------------------
plot_ceplane <- function(boot_icer_result, k = 500, subtitle = NULL) {
  if (!"boot_dist" %in% names(boot_icer_result)) stop("Input must be result from boot_icer()")

  data <- data.frame(
    IncrementalCost = boot_icer_result$boot_dist[, 1],
    IncrementalEffect = boot_icer_result$boot_dist[, 2]
  )

  # Compute quadrant proportions
  data$Quadrant <- with(data, 
    ifelse(IncrementalCost >= 0 & IncrementalEffect >= 0, "Q1",
    ifelse(IncrementalCost >= 0 & IncrementalEffect < 0, "Q4",
    ifelse(IncrementalCost < 0 & IncrementalEffect >= 0, "Q2", "Q3"))))

  quad_table <- table(data$Quadrant)
  quad_prop <- round(prop.table(quad_table) * 100, 1)
  quad_labels <- setNames(paste0(names(quad_prop), ": ", quad_prop, "%"), names(quad_prop))

  max_x <- max(abs(data$IncrementalEffect)) * 1.1
  max_y <- max(abs(data$IncrementalCost)) * 1.1

  # Define axis breaks
  x_breaks <- pretty(c(-max_x * 1.3, max_x * 1.3), n = 10)
  y_breaks <- pretty(c(-max_y * 1.3, max_y * 1.3), n = 10)

  labels_df <- data.frame(
    x = c(max(x_breaks), min(x_breaks), min(x_breaks), max(x_breaks)),
    y = c(max(y_breaks), max(y_breaks), min(y_breaks), min(y_breaks)),
    label = c(
      ifelse("Q1" %in% names(quad_labels), quad_labels["Q1"], "Q1: 0%"),
      ifelse("Q2" %in% names(quad_labels), quad_labels["Q2"], "Q2: 0%"),
      ifelse("Q3" %in% names(quad_labels), quad_labels["Q3"], "Q3: 0%"),
      ifelse("Q4" %in% names(quad_labels), quad_labels["Q4"], "Q4: 0%")
    )
  )

  ggplot2::ggplot(data, ggplot2::aes(x = IncrementalEffect, y = IncrementalCost)) +
    ggplot2::geom_point(alpha = 0.5, color = "black") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_abline(slope = k, intercept = 0, color = "red", size = 1) +
    ggplot2::geom_text(data = labels_df, ggplot2::aes(x = x, y = y, label = label), 
                       inherit.aes = FALSE, color = "darkblue", size = 4.5) +
    ggplot2::scale_x_continuous(limits = c(min(x_breaks), max(x_breaks)), breaks = x_breaks) +
    ggplot2::scale_y_continuous(limits = c(min(y_breaks), max(y_breaks)), breaks = y_breaks) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Cost-Effectiveness Plane",
                  subtitle = if (is.null(subtitle)) paste0("WTP Threshold: ", k) else subtitle,
                  x = "Incremental Effect",
                  y = "Incremental Cost")
}
