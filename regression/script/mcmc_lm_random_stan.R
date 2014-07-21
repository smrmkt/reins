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
model.fit <- sflist2stanfit(sflist)
stopCluster(cl)

# save data
save.image("output/mcmc_lm_random_stan/result.Rdata")
## get summary
print(fit, digits_summary=3)
fit.summary <- data.frame(summary(fit)$summary)
write.table(fit.summary,
            file="output/mcmc_lm_random_stan/fit_summary.txt",
            sep="\t",
            quote=F,
            col.names=NA)
## get plot
pdf("output/fit_plot.pdf", width=600/72, height=600/72)
plot(fit)
dev.off()
## get traceplot
pdf("output/fit_traceplot.pdf", width=600/72, height=600/72)
traceplot(fit)
dev.off()
