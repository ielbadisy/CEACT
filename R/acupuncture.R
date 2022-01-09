# Title :

# Description

# synthetic database generated from the outcomes published in the article by Witt CM et al., 2006.
# The original study is a pragmatic randomized trial evaluating the effectiveness and costs of
# acupuncture for chronic low back pain.
# patients with chronic low back were allocated to an acupuncture or a no-acupuncture group. Knowing that all patients had received a routin medical care in additon
# to acupuncture.



# Usage

data(acupuncture)

# Format
# a data frame of  including three measurement variables for  :

# cost :
# qaly : Qaly utility values
# group :


# Source

#
# source : Witt, Claudia M., et al. "Pragmatic randomized trial evaluating the clinical and economic effectiveness of acupuncture for chronic low back pain." American journal of epidemiology 164.5 (2006): 487-496.

n_control <- 1390
control <- data.frame(cost = rnorm(n_control, mean = 782.36, sd = 1728.8),
                      qaly = rnorm(n_control, mean = 0.62, sd = 0.1),
                      group = rep("control", n_control) )

n_acupuncture <- 1451
acupuncture <- data.frame(cost = rnorm(n_acupuncture, mean = 1062.46, sd = 1539.74),
                             qaly = rnorm(n_acupuncture, mea = 0.65, sd = 0.1),
                             group = rep("acupuncture", n_acupuncture))

acunpunture <- rbind(control, acupuncture)
acunpunture$group <- as.factor(acunpunture$group)


usethis::use_data(acupuncture)

