#' Observed cost-effectiveness outcomes
#'
#' @param cost
#' @param effect
#' @param group
#' @param ref
#' @param data
#' @param na.omit
#'
#' @return
#' @export
#'
#' @examples
#' trt_grp <- data.frame(group = rep("treatment", 250), cost = rgamma(250, 1000), effect = runif(250, 1, 20))
#' control_grp <- data.frame(cost = rgamma(250, 10), effect = runif(250, 50, 100), group = rep("control", 250))
#' data <- rbind(trt_grp, control_grp)
#' ref <- 'control'
#'
cea <- function(cost, effect, group, ref, data, na.omit = TRUE) {

# ref == name of reference strategy/control group...


# must do an action on na : like omit.na !!


# warning : must have only two levels !


# warning if groups have the same length !


# split data by group

group_ctrl <- data[data$group == ref, ]
group_trt <-  data[data$group != ref, ]

# cost & effect observed mean and sd
observ_c_mean <-  round(c(mean(group_ctrl$cost), mean(group_trt$cost)),4)  # c(mean(data[data$group == ref, ]$cost), mean(data[data$group != ref, ]$cost)
observ_e_mean <- round(c(mean(group_ctrl$effect), mean(group_trt$effect)),4)
observ_c_sd <-  round(c(sd(group_ctrl$cost), sd(group_trt$cost)),4)
observ_e_sd <- round(c(sd(group_ctrl$effect), sd(group_trt$effect)),4)

# observed ICER
observ_delta_c <- observ_c_mean[2]-observ_c_mean[1]
observ_delta_e <- observ_e_mean[2]-observ_e_mean[1]
observ_icer <- round((observ_delta_c/observ_delta_e),3)

# compare the difference in means : t test
compar_c <- t.test(group_trt$cost, group_ctrl$cost)
compar_e <- t.test(group_trt$effect, group_ctrl$effect)

# percentile 95% ci and p-value
ci <-  round(c(compar_c$conf.int, compar_e$conf.int, use.names = FALSE), 3)

# observed cea outcomes
options(scipen=4)
observ_cea <- data.frame(
  Outcome = c("Mean Cost", "Mean Effect"),
  Control = c(paste0(observ_c_mean[1], ' (sd ',observ_c_sd[1],')'), paste0(observ_e_mean[1], ' (sd ',observ_e_sd[1],')')), # add (sd)
  Treatment = c(paste0(observ_c_mean[2], ' (sd ',observ_c_sd[2],')'), paste0(observ_e_mean[2], ' (sd ',observ_e_sd[2],')')), # ad (sd)
  Delta = round(c(observ_delta_c,  observ_delta_e),4),
  'percentile CI' = c(paste('[',ci[1],';', ci[2],']'), paste('[', ci[3],';', ci[4], ']')),
  p.value = round(c(compar_c$p.value, compar_e$p.value),4)
)

#class(observ_cea) <- "observ_cea"

# ajouter round_df() pour l'output !!!

observ_cea <- round_df(observ_cea, 3)
tangram::tangram(observ_cea, id="tbl2", caption="Observed cost-effectiveness outcomes", style="nejm",  as.character=TRUE, footnote = paste0('Observed ICER = ', observ_icer), digits = 1)
}

cea(cost, effect, group, ref, data)

