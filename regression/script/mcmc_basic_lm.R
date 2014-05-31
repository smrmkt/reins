# load library
library('ggplot2')
library('rjags')
library('R2WinBUGS')
library('reshape2')
library('dclone')

# load data
d <- read.delim('data/mantions.csv', header=T, sep=',')
d <- na.omit(d)

# convert to BUGS object
mcmc.list2bugs <- function(mcmc.list) {
  b1 <- mcmc.list[[1]]
  m1 <- as.matrix(b1)
  mall <- matrix(numeric(0), 0, ncol(m1))
  n.chains <- length(mcmc.list)
  for (i in 1:n.chains) {
    mall <- rbind(mall, as.matrix(mcmc.list[[i]]))
  }
  sims.array <- array(mall, dim = c(nrow(m1), n.chains, ncol(m1)))
  dimnames(sims.array) <- list(NULL, NULL, colnames(m1))
  mcpar <- attr(b1, "mcpar")
  as.bugs.array(
    sims.array = sims.array,
    model.file = NULL,
    program = NULL,
    DIC = FALSE,
    DICOutput = NULL,
    n.iter = mcpar[2],
    n.burnin = mcpar[1] - mcpar[3],
    n.thin = mcpar[3]
  )
}

# mcmc modeling
## model description
mantion_model <- function() {
  # main model
  for (i in 1:N) {
    PRICE[i] ~ dnorm(b+bD*DISTANCE[i]+bF*FROM[i]+bR*ROOM[i]+bS*SPACE[i], tau[1])
  }
  # prior distribution
  b  ~ dnorm(0.0, 1.0e-6);
  bD ~ dnorm(0.0, tau[2]);
  bF ~ dnorm(0.0, tau[3]);
  bR ~ dnorm(0.0, tau[3]);
  bS ~ dnorm(0.0, tau[4]);
  for (i in 1:N.tau) {
    tau[i] <- 1 / (sigma[i] * sigma[i])
    sigma[i] ~ dunif(0.0, 1.0e+6)
  }
}

# モデルの書き出し先
file.bugs <- file.path('script/mantion.bug')
R2WinBUGS::write.model(mantion_model, file.bugs)

# data for JAGS
list.data <- list(
  PRICE      = d$price,
  DISTANCE   = d$distance,
  FROM       = d$from,
  ROOM       = d$room,
  SPACE      = d$space,
  N          = nrow(d),
  N.tau      = 4
)

# initial value for JAGS
inits <- list(
  b     = rnorm(1, 39.22, 16.88),
  bD    = rnorm(1, 0, 100),  
  bF    = rnorm(1, 0, 100),
  bR    = rnorm(1, 0, 100),
  bS    = rnorm(1, 0, 100),
  sigma = runif(4, 0, 100)
)

# estimate parameter
params <- c('b', 'bD', 'bF', 'bR', 'bS', 'sigma')

# model definition
model <- jags.model(
  file    = file.bugs,
  data    = list.data,
  inits   = list(inits, inits, inits),
  n.chain = 3
)

# set MCMC burn-in cycles
update(model, 100)

# conduct MCMC silumation results
post.list <- coda.samples(
  model,
  params,
  thin = 3,
  n.iter = 1500
)
summary(post.list)
mcmc.list2bugs(post.list)

# graph
plot(post.list)

# visualize parameter
n.samples <- niter(post.list) * nchain(post.list)
b  <- c(sapply(match('b', varnames(post.list)), function(i) unlist(post.list[, i])))
bD <- c(sapply(match('bD', varnames(post.list)), function(i) unlist(post.list[, i])))
bF <- c(sapply(match('bF', varnames(post.list)), function(i) unlist(post.list[, i])))
bR <- c(sapply(match('bR', varnames(post.list)), function(i) unlist(post.list[, i])))
bS <- c(sapply(match('bS', varnames(post.list)), function(i) unlist(post.list[, i])))
#bs <- data.frame(b=b, bD=bD, bF=bF, bR=bR, bS=bS)
bs <- data.frame(bD=bD, bF=bF, bR=bR, bS=bS)
bs.melt <- melt(bs, id=c(), variable.name="param")
bs.qua.melt <- ddply(bs.melt, .(param), summarize,
                     median=median(value),
                     ymax=quantile(value,prob=0.975),
                     ymin=quantile(value,prob=0.025))
colnames(bs.qua.melt)[2] <- "value"
bs.melt <- data.frame(bs.melt, ymax=rep(0, n.samples), ymin=rep(0, n.samples))
p <- ggplot(bs.melt,aes(x=param, y=value, group=param, ymax=ymax, ymin=ymin, color=param))
p <- p + geom_violin(trim=F,fill="#5B423D",linetype="blank",alpha=I(1/3))
p <- p + geom_pointrange(data=bs.qua.melt,size=0.75)
p <- p + labs(x="", y="")
p <- p + theme(axis.text.x=element_text(size=14), axis.text.y=element_text(size=14))
plot(p)