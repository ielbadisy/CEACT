plot_tornado <- function(df, metric = "ICER") {
  if (!metric %in% names(df)) stop(paste("Column", metric, "not found in data."))

  ggplot2::ggplot(df, ggplot2::aes(x = Value, y = !!as.name(metric))) +
    ggplot2::geom_line(color = "firebrick", linewidth = 1.2) +
    ggplot2::geom_point(color = "firebrick") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Tornado Plot -", metric),
      x = "Parameter Value",
      y = metric
    )
}
