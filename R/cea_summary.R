# -------------------------------
# Observed Cost-Effectiveness Summary
# -------------------------------
cea <- function(data, cost, effect, group, ref, na.omit = TRUE) {
  if (na.omit) data <- na.omit(data)
  
  data[[group]] <- as.factor(data[[group]])
  ctrl_data <- data[data[[group]] == ref, ]
  trt_data <- data[data[[group]] != ref, ]
  
  observ_c_mean <- c(mean(ctrl_data[[cost]]), mean(trt_data[[cost]]))
  observ_e_mean <- c(mean(ctrl_data[[effect]]), mean(trt_data[[effect]]))
  observ_c_sd <- c(sd(ctrl_data[[cost]]), sd(trt_data[[cost]]))
  observ_e_sd <- c(sd(ctrl_data[[effect]]), sd(trt_data[[effect]]))
  
  delta_c <- observ_c_mean[2] - observ_c_mean[1]
  delta_e <- observ_e_mean[2] - observ_e_mean[1]
  ICER <- delta_c / delta_e
  
  t_cost <- t.test(trt_data[[cost]], ctrl_data[[cost]])
  t_eff  <- t.test(trt_data[[effect]], ctrl_data[[effect]])
  
  ci <- c(t_cost$conf.int, t_eff$conf.int)
  
  out <- data.frame(
    Outcome = c("Mean Cost", "Mean Effect"),
    Control = c(paste0(round(observ_c_mean[1], 2), " (sd ", round(observ_c_sd[1],2), ")"),
                paste0(round(observ_e_mean[1], 2), " (sd ", round(observ_e_sd[1],2), ")")),
    Treatment = c(paste0(round(observ_c_mean[2], 2), " (sd ", round(observ_c_sd[2],2), ")"),
                  paste0(round(observ_e_mean[2], 2), " (sd ", round(observ_e_sd[2],2), ")")),
    Delta = round(c(delta_c, delta_e), 3),
    CI = c(paste0("[", round(ci[1],2), ";", round(ci[2],2), "]"),
           paste0("[", round(ci[3],2), ";", round(ci[4],2), "]")),
    p.value = round(c(t_cost$p.value, t_eff$p.value), 4)
  )
  
  attr(out, "ICER") <- round(ICER, 3)
  return(out)
}
