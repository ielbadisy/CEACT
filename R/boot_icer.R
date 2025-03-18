# -------------------------------
# Bootstrap ICER Function
# -------------------------------
boot_icer <- function(data, cost, effect, group, ref, R = 1000, ci.type = "bca") {
  require(boot)
  
  data <- data[, c(cost, effect, group)]
  colnames(data) <- c("cost", "effect", "group")
  
  stat_func <- function(d, i) {
    d <- d[i,]
    delta_cost <- mean(d$cost[d$group != ref]) - mean(d$cost[d$group == ref])
    delta_effect <- mean(d$effect[d$group != ref]) - mean(d$effect[d$group == ref])
    ICER <- delta_cost / delta_effect
    return(c(delta_cost, delta_effect, ICER))
  }
  
  set.seed(1234)
  bt <- boot(data, stat_func, R = R)
  
  ci_dc <- boot.ci(bt, type = ci.type, index = 1)$bca[4:5]
  ci_de <- boot.ci(bt, type = ci.type, index = 2)$bca[4:5]
  ci_icer <- boot.ci(bt, type = ci.type, index = 3)$bca[4:5]
  
  summary_tbl <- data.frame(
    Metric = c("Delta Cost", "Delta Effect", "ICER"),
    Estimate = round(colMeans(bt$t), 3),
    Observed = round(bt$t0, 3),
    StdError = round(apply(bt$t, 2, sd), 3),
    Bias = round(colMeans(bt$t) - bt$t0, 3),
    CI = c(paste0("[", round(ci_dc[1],3), ";", round(ci_dc[2],3), "]"),
           paste0("[", round(ci_de[1],3), ";", round(ci_de[2],3), "]"),
           paste0("[", round(ci_icer[1],3), ";", round(ci_icer[2],3), "]"))
  )
  
  return(list(summary = summary_tbl, boot_dist = bt$t))
}
