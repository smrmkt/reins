# load library
library('ggplot2')
library('rstan')

# load data
d <- read.delim('data/mantions.csv', header=T, sep=',')
d = na.omit(d)

# package data for stan
d.stan = list(N=nrow(d),
              d_d=d$distance,
              d_f=d$from,
              d_r=d$room,
              d_s=d$space,
              d_p=d$price)
basic_lm.fit<-stan(file="script/mcmc_basic_lm.stan",
                   data=d.stan,
                   iter=100,
                   chains=3)