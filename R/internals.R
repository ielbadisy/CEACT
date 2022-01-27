# bca CI =================================================

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


# round data frame =================================================

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == "numeric"

  x[numeric_columns] <- round(x[numeric_columns], digits)

  x
}

