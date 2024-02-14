#' Acupuncture
#'
#'
#'Synthetic database generated from the outcomes published in the article by Witt CM et al., 2006.
#'The original study is a pragmatic randomized trial evaluating the effectiveness and costs of
#'acupuncture for chronic low back pain. Patients with chronic low back were allocated to an acupuncture or a no-acupuncture group.
#'Knowing that all patients had received a routine medical care in addition
# to acupuncture.

#' @name acupuncture
#' @docType data
#' @author Imad EL BADISY \email{elbadisyimad@@gmail.com}
#' @keywords acupuncture cos-effectiveness analysis
#' @usage data(acupunture)
#' @format A data frame with 2841 observations and 3 variables
#' cost : cost in dollars
#' qaly: quality-adjusted life
#' group: control group and treatment group (i.e acupunture)
#' @source Witt, Claudia M., et al. "Pragmatic randomized trial evaluating the clinical and economic effectiveness of acupuncture for chronic low back pain." American journal of epidemiology 164.5 (2006): 487-496.
#' @example
#' data(acupunture)
NULL

n_control <- 1390
control <- data.frame(cost = rnorm(n_control, mean = 782.36, sd = 1728.8),
                      qaly = rnorm(n_control, mean = 0.62, sd = 0.1),
                      group = rep("control", n_control) )

n_acupuncture <- 1451
acupuncture <- data.frame(cost = rnorm(n_acupuncture, mean = 1062.46, sd = 1539.74),
                          qaly = rnorm(n_acupuncture, mea = 0.65, sd = 0.1),
                          group = rep("acupuncture", n_acupuncture))

acupuncture <- rbind(control, acupuncture)
acupuncture$group <- as.factor(acupuncture$group)


