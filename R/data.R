#' Example Trial-Based Cost-Effectiveness Dataset
#'
#' A patient-level clinical-trial cost-effectiveness dataset with treatment
#' assignment, total cost, QALYs, and baseline covariates. The dataset is used
#' in teaching examples for trial-based economic evaluation and was described
#' as being provided by the Health Services Research Unit for the textbook
#' *Economic Evaluation in Clinical Trials*.
#'
#' @format A data frame with 500 rows and 10 variables:
#' \describe{
#'   \item{id}{Patient identifier.}
#'   \item{treat}{Treatment indicator, 1 for treatment and 0 for control.}
#'   \item{cost}{Total cost in US dollars.}
#'   \item{qaly}{Quality-adjusted life-years.}
#'   \item{dissev}{Disease severity, ranging approximately from 0.025 to 0.729.}
#'   \item{race}{Race indicator as supplied in the source dataset.}
#'   \item{blcost}{Baseline cost in US dollars.}
#'   \item{blqaly}{Baseline QALY.}
#'   \item{male}{Sex indicator, 1 for male and 0 for female.}
#'   \item{group}{Factor version of `treat`, with levels `control` and
#'     `treatment`.}
#' }
#'
#' @source Health Services Research Unit example dataset used in Glick HA,
#' Doshi JA, Sonnad SS, Polsky D. *Economic Evaluation in Clinical Trials*.
#' Oxford University Press. Also distributed in the `ceaR` package as
#' `clintrial_cea`.
"trial_cea"
