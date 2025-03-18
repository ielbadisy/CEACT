#' Bias-Corrected and Accelerated Bootstrap Confidence Interval
#'
#' This function calculates bias-corrected and accelerated (BCa) bootstrap
#' confidence intervals for a set of bootstrap estimates. The BCa method
#' provides a more accurate confidence interval than the basic percentile
#' method, especially for small sample sizes.
#'
#' @param theta A numeric vector of bootstrap estimates, typically the result
#'   of resampling and recalculating a statistic of interest many times.
#' @param conf.level A numeric value representing the confidence level for the
#'   interval. Default is 0.95 for a 95% confidence interval.
#'
#' @return A numeric vector with two elements: the lower and upper bounds of
#'   the BCa bootstrap confidence interval.
#' @examples
#' # Example usage (assuming `resampled_stats` is a vector of bootstrap estimates):
#' bca_conf_interval <- bca(resampled_stats, conf.level = 0.95)
#' print(bca_conf_interval)
#' @export
bca <- function(theta, conf.level = .95) {

  low <- (1 - conf.level) / 2 # lower bound
  high <- 1 - low # upper bound
  sims <- length(theta) # sample size

  # compute the constant
  z0 <- length(theta[theta <= mean(theta)]) / sims # inverse of phi * function(theta)
  zu <- qnorm(z0)
  I <- (sims - 1) * (mean(theta, na.rm = TRUE) - theta) # compute the influence point
  a <- (sum(I^3) / sum(I^2)^1.5) / 6
  lower.inv <- pnorm(z0 + (z0 + qnorm(low)) / (1 - a * (z0 + qnorm(low))))
  lower <- quantile(theta, lower.inv, names = FALSE)
  upper.inv <- pnorm(z0 + (z0 + qnorm(high)) / (1 - a * (z0 + qnorm(high))))
  upper <- quantile(theta, upper.inv, names = FALSE)

  return(c(lower, upper))
  }


#' Round Numeric Columns in a Data Frame
#'
#' Rounds all numeric columns in a data frame to a specified number of decimal
#' places. This function is useful for cleaning and preparing data for analysis
#' or presentation.
#'
#' @param x A data frame to be rounded. Each numeric column in this data frame
#'   will be rounded to the specified number of decimal places.
#' @param digits An integer specifying the number of decimal places to which
#'   numeric columns should be rounded.
#'
#' @return The data frame with numeric columns rounded to the specified number
#'   of decimal places.
#' @examples
#' # Example usage (assuming `df` is a data frame with numeric columns):
#' rounded_df <- round_df(df, digits = 2)
#' print(rounded_df)
#' @export
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == "numeric"
  x[numeric_columns] <- round(x[numeric_columns], digits)

  x
}

