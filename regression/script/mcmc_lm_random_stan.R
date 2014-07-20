# load library
library('ggplot2')
library('doParallel')
library('foreach')
library('rstan')

# load data
d <- read.delim('data/mantions.csv', header=T, sep=',')
d = na.omit(d)
attach(d)

# package data for stan
X  = t(rbind(distance, from, room, space))
Y = price
d.stan = list(N=nrow(X), M=ncol(X), X=X, Y=Y)

# test procesing
if (0) {
  model.fit<-stan(file="script/mcmc_lm_random.stan",
                     data=d.stan,
                     iter=40,
                     chains=2)
}

# parallel processing
N.chain = 3
cl = makeCluster(N.chain)
registerDoParallel(cl)
sflist = foreach(i=1:N.chain, .packages='rstan') %dopar% {
  stan(
    file='script/mcmc_lm_random.stan',
    data=data, iter=10000, thin=10,
    chains=1, chain_id=i, refresh=-1
  )
}
model.fits <- sflist2stanfit(sflist)
stopCluster(cl)

## parameter trace
traceplot(model.fit, ask=T)
## parameter estimate with percentile
print(model.fit, digits_summary=2)
## extract sampling parameter
la <- extract(model.fit, permuted = TRUE)
