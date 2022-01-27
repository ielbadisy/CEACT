

#================================
#k <- 500
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



#plot_ceplane(acupuncture)
