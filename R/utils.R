# -------------------------------
# Round DataFrame Utility
# -------------------------------
round_df <- function(x, digits = 3) {
  numeric_columns <- sapply(x, is.numeric)
  x[numeric_columns] <- round(x[numeric_columns], digits)
  x
}
