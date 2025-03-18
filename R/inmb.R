# compute NMB for a given value of K
d <- data.frame(c = runif(1000, 120, 2958), e = runif(1000, 120, 699))
d <- as.data.table(d)
#===========================


ib <- wtp * d$e - d$c # incremental benefit
ceac <- mean(ib>0) # probability of cost-effectiveness
eib <- mean(ib) # expected incremental benefit

#===============================
max.wtp <- 20
steps <- max.wtp/10

library(data.table)
library(ggplot2)

CEAC <- data.table()

for (wtp in seq(0, max.wtp, steps)) {
  prob <- d[, mean((wtp * e - c) > 0)]
  CEAC <- rbind(CEAC, data.table(wtp, prob))

}

CEAC

plot_ceac <- function(data){
pdata <- data
p <- ggplot(data=pdata, aes(x=wtp, y=prob, group=1)) +
  geom_line(color="red")+
  geom_point()+
  xlab("Treshold willingness to pay")+
  ylab("Probability of cost-effective")+
  scale_x_continuous(limits = c(0, max.wtp), breaks = c(seq(0,max.wtp, steps)))+
  scale_y_continuous(limits = c(0,1), breaks = c(seq(0,1, 0.1)))+
  geom_hline(yintercept = prob, linetype = "dashed", colour = "red") +
  theme_classic()
 return(p)
}


plot_ceac(CEAC)

