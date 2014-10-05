# load library
library('ggplot2')
library('doParallel')
library('foreach')
library('rstan')
library('plyr')
library('dplyr')
library('reshape2')

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
    file='script/hierarchical_station_train.stan',
    data=d.stan, iter=2000, thin=3,
    chains=1, chain_id=i, refresh=-1
  )
}
model.fit = sflist2stanfit(sflist)
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

# extract mcmc sample for graph
la = extract(model.fit, permuted = TRUE)

# draw train distribution graph
## data preprocess
train_names = c('湘南新宿ライン宇須', '東海道本線', '南武線', '鶴見線', '横浜線', '根岸線',
                '横須賀線', '京浜東北・根岸線', '東急東横線', '京浜急行電鉄本線',
                '京浜急行電鉄逗子線','相模鉄道本線', '横浜市ブルーライン', '金沢シーサイドＬ',
                '横浜高速鉄道ＭＭ線', '横浜市グリーンＬ', '東海道・山陽新幹線',
                '東急目黒線', '東急田園都市線', '京王電鉄相模原線', '小田急電鉄多摩線',
                '京浜急行電鉄大師線', '小田急電鉄小田原線')
r_t = la$r_t
colnames(r_t) = train_names
r_t.melt <- melt(r_t, id = c(), value="param")
colnames(r_t.melt)[2] <- "train"
r_t.qua.melt <- ddply(r_t.melt, .(train), summarize,
                     median=median(value),
                     ymax=quantile(value, prob=0.975),
                     ymin=quantile(value, prob=0.025))
colnames(r_t.qua.melt)[2] <- "value"
r_t.melt = data.frame(r_t.melt, ymax=rep(0, nrow(r_t.melt)), ymin=rep(0, nrow(r_t.melt)))
## draw graph
p <- ggplot(r_t.melt, aes(x=reorder(train, value),
                    y=value, group=train, color=train, ymax=ymax, ymin=ymin))
p <- p + geom_violin(trim=F, fill="#5B423D", linetype="blank", alpha=I(1/3))
p <- p + geom_pointrange(data=r_t.qua.melt, size=0.20)
p <- p + coord_flip()
p <- p + labs(x="", y="固定効果 [万円/㎡]")
p <- p + theme_bw(base_family = "HiraKakuProN-W3")
p <- p + theme(axis.text.x=element_text(size=5),
               axis.title.x=element_text(size=5),
               axis.text.y=element_text(size=5),
               legend.position="none")
plot(p)
ggsave(file="output/hierarchical_station_train/train.png",
       plot=p, dpi=300, width=4, height=3)

# draw other parameter distribution graph
## data preprocess
bs = data.frame(la$b)
pnames = c('駅からの距離', '築年', '部屋数', '床面積')
colnames(bs) = pnames
bs.melt <- melt(bs, id = c(), value="params")
colnames(bs.melt)[1] <- "params"
bs.qua.melt <- ddply(bs.melt, .(params), summarize,
                      median=median(value),
                      ymax=quantile(value, prob=0.975),
                      ymin=quantile(value, prob=0.025))
colnames(bs.qua.melt)[2] <- "value"
bs.melt = data.frame(bs.melt, ymax=rep(0, nrow(bs.melt)), ymin=rep(0, nrow(bs.melt)))
bs.lm <- data.frame(params=pnames,
                    value=c(-4.23, 0.96, -2.61, 2.80), ymax=rep(0, 4), ymin=rep(0, 4))
## draw graph
p <- ggplot(bs.melt, aes(x=reorder(params, value),
                          y=value, group=params, color=params, ymax=ymax, ymin=ymin))
p <- p + geom_point(data=bs.lm, color="black", size=1.6, alpha=I(2/3))
p <- p + geom_violin(trim=F, fill="#5B423D", scale="width", linetype="blank", alpha=I(1/3))
p <- p + geom_pointrange(data=bs.qua.melt, size=0.40)
p <- p + coord_flip()
p <- p + labs(x="", y="")
p <- p + theme_bw(base_family = "HiraKakuProN-W3")
p <- p + theme(axis.text.x=element_text(size=8),
               axis.title.x=element_text(size=8),
               axis.text.y=element_text(size=8),
               legend.position="none")
plot(p)
ggsave(file="output/hierarchical_station_train/params.png",
       plot=p, dpi=300, width=4, height=3)

# draw station(price higher) distribution graph
station_names = c('武蔵小杉', 'みなとみらい', '新百合ケ丘', '横浜', '鹿島田',
                  '桜木町', '石川町', '新丸子', '元住吉', '京急川崎')
r_s = la$r_s[, c(109, 128, 124, 19, 89, 70, 79, 62, 47, 97)]
colnames(r_s) = station_names
r_s.melt <- melt(r_s, id = c(), value="param")
colnames(r_s.melt)[2] <- "station"
r_s.qua.melt <- ddply(r_s.melt, .(station), summarize,
                      median=median(value),
                      ymax=quantile(value, prob=0.975),
                      ymin=quantile(value, prob=0.025))
colnames(r_s.qua.melt)[2] <- "value"
r_s.melt = data.frame(r_s.melt, ymax=rep(0, nrow(r_s.melt)), ymin=rep(0, nrow(r_s.melt)))
## draw graph
p <- ggplot(r_s.melt, aes(x=reorder(station, value),
                          y=value, group=station, color=station, ymax=ymax, ymin=ymin))
p <- p + geom_violin(trim=F, fill="#5B423D", linetype="blank", alpha=I(1/3))
p <- p + geom_pointrange(data=r_s.qua.melt, size=0.30)
p <- p + coord_flip()
p <- p + labs(x="", y="固定効果 [万円/㎡]")
p <- p + theme_bw(base_family = "HiraKakuProN-W3")
p <- p + theme(axis.text.x=element_text(size=6),
               axis.title.x=element_text(size=6),
               axis.text.y=element_text(size=6),
               legend.position="none")
plot(p)
ggsave(file="output/hierarchical_station_train/station_h.png",
       plot=p, dpi=300, width=4, height=3)

# draw station(price lower) distribution graph
station_names = c('追浜', '浜川崎', '鶴見小野', '新子安', '子安',
                  '磯子', '下永谷', '小島新田', '新杉田', '鶴川')
r_s = la$r_s[, c(31, 74, 117, 63, 5, 27, 43, 102, 65, 37)]
colnames(r_s) = station_names
r_s.melt <- melt(r_s, id = c(), value="param")
colnames(r_s.melt)[2] <- "station"
r_s.qua.melt <- ddply(r_s.melt, .(station), summarize,
                      median=median(value),
                      ymax=quantile(value, prob=0.975),
                      ymin=quantile(value, prob=0.025))
colnames(r_s.qua.melt)[2] <- "value"
r_s.melt = data.frame(r_s.melt, ymax=rep(0, nrow(r_s.melt)), ymin=rep(0, nrow(r_s.melt)))
## draw graph
p <- ggplot(r_s.melt, aes(x=reorder(station, value),
                          y=value, group=station, color=station, ymax=ymax, ymin=ymin))
p <- p + geom_violin(trim=F, fill="#5B423D", linetype="blank", alpha=I(1/3))
p <- p + geom_pointrange(data=r_s.qua.melt, size=0.30)
p <- p + coord_flip()
p <- p + labs(x="", y="固定効果 [万円/㎡]")
p <- p + theme_bw(base_family = "HiraKakuProN-W3")
p <- p + theme(axis.text.x=element_text(size=6),
               axis.title.x=element_text(size=6),
               axis.text.y=element_text(size=6),
               legend.position="none")
plot(p)
ggsave(file="output/hierarchical_station_train/station_l.png",
       plot=p, dpi=300, width=4, height=3)
