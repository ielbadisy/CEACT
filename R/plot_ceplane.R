# -------------------------------
# Plot Cost-Effectiveness Plane (Improved with Full Quadrants)
# -------------------------------
plot_ceplane <- function(data, k = 500) {
  require(ggplot2)
  colnames(data)[1:2] <- c("IncrementalCost", "IncrementalEffect")
  
  max_x <- max(abs(data$IncrementalEffect)) * 1.1
  max_y <- max(abs(data$IncrementalCost)) * 1.1
  
  ggplot(data, aes(x = IncrementalEffect, y = IncrementalCost)) +
    geom_point(alpha = 0.5, color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_abline(slope = k, intercept = 0, color = "red", size = 1) +
    scale_x_continuous(limits = c(-max_x, max_x)) +
    scale_y_continuous(limits = c(-max_y, max_y)) +
    theme_classic() +
    labs(title = "Cost-Effectiveness Plane",
         x = "Incremental Effect",
         y = "Incremental Cost")
}
