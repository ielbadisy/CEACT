#' Plot Cost-Effectiveness Plane
#'
#' Generates a cost-effectiveness plane plot using ggplot2. This plot visualizes
#' incremental costs versus incremental effects, typically used in health
#' economics to assess the cost-effectiveness of healthcare interventions.
#' A budget line with a specified slope (k) is added to help identify interventions
#' below a certain cost-effectiveness threshold.
#'
#' @param data A data frame containing at least two columns: the first column
#'   for incremental effects and the second column for incremental costs. Additional
#'   columns are ignored.
#' @param k The slope of the budget line, representing the willingness-to-pay
#'   threshold for an additional unit of effect. Default is 500.
#' @param labels Optional vector of labels for the points plotted on the
#'   cost-effectiveness plane. Default is NULL.
#'
#' @return A ggplot object representing the cost-effectiveness plane with
#'   interventions plotted as points, a budget line (with slope k), and axes
#'   for incremental effects and costs. This plot object can be further modified
#'   or directly displayed using ggplot2 functions.
#'
#' @examples
#' # Assuming `acupuncture` is a data frame with incremental effects in the first
#' # column and incremental costs in the second column:
#' plot <- plot_ceplane(acupuncture, k = 500)
#' plot
#'
#' @import ggplot2
#' @export
plot_ceplane <- function(data, k = 500, labels = NULL) {

  pdata <- data
  pdata$ib <- k * pdata[1] -  pdata[2]
  # x and y limits
  xlim <- ceiling(max(pdata[,2]) * 1.2)
  ylim <- max(pdata[,1]) * 1.2

  # Main plot
  p <- ggplot2::ggplot(
    data = pdata,
    mapping = ggplot2::aes(x = pdata[,2], y = pdata[,1])) +
    ggplot2::geom_jitter(size = .5)  +
    ggplot2::xlab("Incremental effect") +
    ggplot2::ylab("Incremental costs") +
    ggplot2::scale_x_continuous(limits = c(-xlim, xlim),
                                breaks = pretty(seq(-xlim, xlim, length.out = 10),
                                                n = 10)) +
    ggplot2::scale_y_continuous(limits = c(-ylim, ylim),
                                breaks = pretty(seq(-ylim, ylim, length.out = 10),
                                                n = 10)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_abline(slope = k, linetype = "dashed", colour = "red") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    theme_classic()



  # Return
  return(p)
}
