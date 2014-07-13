# load library
library('rstan')

# load data
d <- read.delim('data/mantions.csv', header=T, sep=',')
d = na.omit(d)
attach(d)

# store elapsed time
time_matrix = array()
time_vector = array()
time_linear = array()

# matrix operation
X = t(rbind(distance, from, room, space))
Y = price
d.stan = list(N=nrow(X), M=ncol(X), X=X, Y=Y)
## matrix operation
fit.matrix = stan(file="script/mcmc_basic_lm_vector.stan",
                  data=d.stan,
                  iter=10,
                  chains=1)
for (i in 1:100) {
  time_matrix[i] = system.time(
    basic_lm.fit<-stan(fit=fit.matrix,
                       data=d.stan,
                       iter=1000,
                       chains=1,
                       thin=10,
                       warmup=100)  
  )
}
## vector operation with for loop
fit.vector<-stan(file="script/mcmc_basic_lm_vector_row.stan",
                 data=d.stan,
                 iter=10,
                 chains=1)
for (i in 1:100) {
  time_vector[i] = system.time(
    basic_lm.fit<-stan(fit=fit.vector,
                       data=d.stan,
                       iter=1000,
                       chains=1,
                       thin=10,
                       warmup=100)  
  )
}

# usual calculation using linear expression
d.stan = list(N=nrow(d),
              d_d=d$distance,
              d_f=d$from,
              d_r=d$room,
              d_s=d$space,
              d_p=d$price)
fit.linear<-stan(file="script/mcmc_basic_lm.stan",
                 data=d.stan,
                 iter=10,
                 chains=1)
for (i in 1:100) {
  time_linear[i] = system.time(
    basic_lm.fit<-stan(fit=fit.linear,
                       data=d.stan,
                       iter=1000,
                       chains=1,
                       thin=10,
                       warmup=100)  
  )
}
