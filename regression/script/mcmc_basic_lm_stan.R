# load library
library('ggplot2')
library('rstan')

# load data
d <- read.delim('data/mantions.csv', header=T, sep=',')
d = na.omit(d)
attach(d)

# package data for stan
X = t(rbind(distance, from, room, space))
Y = price
d.stan = list(N=nrow(X), M=ncol(X), X=X, Y=Y)
basic_lm.fit<-stan(file="script/mcmc_basic_lm.stan",
                   data=d.stan,
                   iter=100,
                   chains=3)
