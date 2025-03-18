# -------------------------------
# Compute NMB and CEAC Table
# -------------------------------
compute_nmb_ceac <- function(data, wtp_range = seq(0, 100000, 1000)) {
  nmb_table <- data.frame()
  for (wtp in wtp_range) {
    nmb <- mean(wtp * data$e - data$c)
    prob <- mean((wtp * data$e - data$c) > 0)
    nmb_table <- rbind(nmb_table, data.frame(WTP = wtp, ENMB = nmb, Prob_CE = prob))
  }
  return(nmb_table)
}
