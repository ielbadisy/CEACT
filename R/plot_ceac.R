# -------------------------------
# Plot CEAC Curve (boot_icer output directly)
# -------------------------------
plot_ceac <- function(boot_icer_result, wtp_range = seq(0, 100000, 1000)) {
  if (!"boot_dist" %in% names(boot_icer_result)) stop("Input must be result from boot_icer()")

  data <- data.frame(
    IncrementalCost = boot_icer_result$boot_dist[, 1],
    IncrementalEffect = boot_icer_result$boot_dist[, 2]
  )

  ceac_data <- data.frame()
  for (wtp in wtp_range) {
    inb <- wtp * data$IncrementalEffect - data$IncrementalCost
    prob_ce <- mean(inb > 0)
    ceac_data <- rbind(ceac_data, data.frame(WTP = wtp, Prob_CE = prob_ce))
  }
  ggplot2::ggplot(ceac_data, ggplot2::aes(x = WTP, y = Prob_CE)) +
    ggplot2::geom_line(color = "blue") +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Cost-Effectiveness Acceptability Curve",
                  x = "Willingness-to-Pay (WTP)",
                  y = "Probability Cost-Effective") +
    ggplot2::scale_y_continuous(limits = c(0, 1))
}
