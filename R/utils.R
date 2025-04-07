
#' @importFrom stats terms sd t.test
#' @importFrom utils head
NULL

# round data.frame output's
round_df <- function(x, digits = 3) {
  numeric_columns <- sapply(x, is.numeric)
  x[numeric_columns] <- round(x[numeric_columns], digits)
  x
}

## format pvalues
format_pval <- function(p, digits = 4) {
  ifelse(p < 0.001, "<0.001", round(p, digits))
}


