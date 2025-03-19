



# -------------------------------
# Bootstrap ICER Function (Formula Interface)
# -------------------------------
boot_icer <- function(formula, data, ref, R = 1000, ci.type = "bca") {
  terms_obj <- terms(formula)
  vars <- all.vars(terms_obj)
  if (length(vars) < 3) stop("Formula must be of the form: cost + effect ~ group")

  cost   <- vars[1]
  effect <- vars[2]
  group  <- vars[3]

  data <- data[, c(cost, effect, group)]
  colnames(data) <- c("cost", "effect", "group")

  require(boot)

  stat_func <- function(d, i) {
    d <- d[i, ]
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
    CI = c(paste0("[", round(ci_dc[1], 3), ";", round(ci_dc[2], 3), "]"),
           paste0("[", round(ci_de[1], 3), ";", round(ci_de[2], 3), "]"),
           paste0("[", round(ci_icer[1], 3), ";", round(ci_icer[2], 3), "]"))
  )

  return(list(summary = summary_tbl, boot_dist = bt$t))
}