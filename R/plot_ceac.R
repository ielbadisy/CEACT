
# -------------------------------
# Plot CEAC Curve
# -------------------------------
plot_ceac <- function(data, wtp_range = seq(0, 100000, 1000)) {
  require(ggplot2)
  colnames(data) <- c("IncrementalCost", "IncrementalEffect")
  
  ceac_data <- data.frame()
  for (wtp in wtp_range) {
    inb <- wtp * data$IncrementalEffect - data$IncrementalCost
    prob_ce <- mean(inb > 0)
    ceac_data <- rbind(ceac_data, data.frame(WTP = wtp, Prob_CE = prob_ce))
  }
  
  ggplot(ceac_data, aes(x = WTP, y = Prob_CE)) +
    geom_line(color = "blue") +
    geom_point() +
    theme_minimal() +
    labs(title = "Cost-Effectiveness Acceptability Curve",
         x = "Willingness-to-Pay (WTP)",
         y = "Probability Cost-Effective") +
    scale_y_continuous(limits = c(0, 1))
}
