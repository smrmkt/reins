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
Tr = train
S  = station
Y  = price
d.stan = list(N=nrow(X),
              N_T=length(unique(train)),
              N_S=length(unique(station)),
              M=ncol(X),
              X=X,
              T=Tr,
              S=S,
              Y=Y)

# test procesing
if (0) {
  model.fit<-stan(file="script/hierarchical_station_train.stan",
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
    data=d.stan, iter=2000, thin=3,
    chains=1, chain_id=i, refresh=-1
  )
}
model.fit <- sflist2stanfit(sflist)
stopCluster(cl)

# save data
save.image("output/hierarchical_station_train/result.Rdata")
## get summary
print(fit, digits_summary=3)
fit.summary <- data.frame(summary(model.fit)$summary)
write.table(fit.summary,
            file="output/hierarchical_station_train/fit_summary.txt",
            sep="\t",
            quote=F,
            col.names=NA)
## get plot
pdf("output/hierarchical_station_train/fit_plot.pdf", width=600/72, height=600/72)
plot(model.fit)
dev.off()
## get traceplot
pdf("output/hierarchical_station_train/fit_traceplot.pdf", width=600/72, height=600/72)
traceplot(model.fit)
dev.off()

# extract mcmc sample
la <- extract(model.fit, permuted = TRUE)
N.day <- nrow(d)
N.mcmc <- length(la$mu)
la$mu     #=> array
la$weight #=> matrix

