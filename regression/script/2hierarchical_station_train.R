# load library
## util
#library('plyr')
library('dplyr')
library('reshape2')
library('pipeR')
## stan
library('doParallel')
library('foreach')
library('rstan')
## graph
library('ggplot2')
library('maptools')
library('gpclib')

################################################################################
# Stan simulation
################################################################################

# pre-simulation
################################################################################
# load data
d = read.delim('data/mantions.csv', header=T, sep=',')
d = na.omit(d)
attach(d)
st = read.delim('data/station_train.csv', header=F, sep=',')

# package data for stan
X  = t(rbind(distance, from, room, space))
ST = st
S  = station
Y  = price
d.stan = list(N=nrow(X),
              N_T=length(unique(train)),
              N_S=length(unique(station)),
              M=ncol(X),
              X=X,
              ST=ST,
              S=S,
              Y=Y)

# simulation
################################################################################
# test procesing
if (0) {
  model.fit<-stan(file="script/2hierarchical_station_train.stan",
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
    file='script/2hierarchical_station_train.stan',
    data=d.stan, iter=2000, thin=3,
    chains=1, chain_id=i, refresh=-1
  )
}
model.fit <- sflist2stanfit(sflist)
stopCluster(cl)

# post-simulation
################################################################################
# save data
save.image("output/2hierarchical_station_train/result.Rdata")
## get summary
print(model.fit, digits_summary=3)
fit.summary <- data.frame(summary(model.fit)$summary)
write.table(fit.summary,
            file="output/2hierarchical_station_train/fit_summary.txt",
            sep="\t",
            quote=F,
            col.names=NA)
## get plot
pdf("output/2hierarchical_station_train/fit_plot.pdf", width=600/72, height=600/72)
plot(model.fit)
dev.off()
## get traceplot
pdf("output/2hierarchical_station_train/fit_traceplot.pdf", width=600/72, height=600/72)
traceplot(model.fit)
dev.off()

# extract mcmc sample
la <- extract(model.fit, permuted = TRUE)
N.day <- nrow(d)
N.mcmc <- length(la$mu)
la$mu     #=> array
la$weight #=> matrix

################################################################################
# Draw graphs of Stan simulation result
################################################################################

# draw train distribution graph
################################################################################
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
ggsave(file="output/2hierarchical_station_train/train.png",
       plot=p, dpi=300, width=4, height=3)

# draw other parameter distribution graph
################################################################################
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
ggsave(file="output/2hierarchical_station_train/params.png",
       plot=p, dpi=300, width=4, height=3)

# draw station(price higher) full distribution graph
################################################################################
station_names = c('みなとみらい', '武蔵小杉', '横浜', '日本大通り', '馬車道', 
                  '元町・中華街', '新丸子', '元住吉', '石川町', '桜木町')
r_s = la$r_s[, c(128, 109, 19, 125, 87, 129, 62, 47, 79, 70)]
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
ggsave(file="output/2hierarchical_station_train/station_full_high.png",
       plot=p, dpi=300, width=4, height=3)

# draw station(price lower) full distribution graph
################################################################################
station_names = c('南部市場', '八景島', '海の公園柴口', '小島新田', '産業道路',
                  '追浜', '鳥浜', '若葉台', '幸浦', 'はるひ野')
r_s = la$r_s[, c(101, 49, 130, 102, 112, 31, 35, 83, 11, 92)]
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
ggsave(file="output/2hierarchical_station_train/station_full_low.png",
       plot=p, dpi=300, width=4, height=3)

# draw station(price higher) specific distribution graph
################################################################################
station_names = c('みなとみらい', '新百合ケ丘', '鹿島田', '石川町', '京急川崎', 
                  '新丸子', '元住吉', '武蔵中原', '桜木町', '上大岡')
as = la$as[, c(128, 124, 89, 79, 97, 62, 47, 108, 70, 40)]
colnames(as) = station_names
as.melt <- melt(as, id = c(), value="param")
colnames(as.melt)[2] <- "station"
as.qua.melt <- ddply(as.melt, .(station), summarize,
                      median=median(value),
                      ymax=quantile(value, prob=0.975),
                      ymin=quantile(value, prob=0.025))
colnames(as.qua.melt)[2] <- "value"
as.melt = data.frame(as.melt, ymax=rep(0, nrow(as.melt)), ymin=rep(0, nrow(as.melt)))
## draw graph
p <- ggplot(as.melt, aes(x=reorder(station, value),
                          y=value, group=station, color=station, ymax=ymax, ymin=ymin))
p <- p + geom_violin(trim=F, fill="#5B423D", linetype="blank", alpha=I(1/3))
p <- p + geom_pointrange(data=as.qua.melt, size=0.30)
p <- p + coord_flip()
p <- p + labs(x="", y="固定効果 [万円/㎡]")
p <- p + theme_bw(base_family = "HiraKakuProN-W3")
p <- p + theme(axis.text.x=element_text(size=6),
               axis.title.x=element_text(size=6),
               axis.text.y=element_text(size=6),
               legend.position="none")
plot(p)
ggsave(file="output/2hierarchical_station_train/station_specific_high.png",
       plot=p, dpi=300, width=4, height=3)

# draw station(price lower) specific distribution graph
################################################################################
station_names = c('浜川崎', '追浜', '磯子', '下永谷', '根岸',
                  '東白楽', '新小安', '新杉田', '久地', '北新横浜')
as = la$as[, c(74, 31, 27, 43, 18, 68, 63, 65, 1, 100)]
colnames(as) = station_names
as.melt <- melt(as, id = c(), value="param")
colnames(as.melt)[2] <- "station"
as.qua.melt <- ddply(as.melt, .(station), summarize,
                      median=median(value),
                      ymax=quantile(value, prob=0.975),
                      ymin=quantile(value, prob=0.025))
colnames(as.qua.melt)[2] <- "value"
as.melt = data.frame(as.melt, ymax=rep(0, nrow(as.melt)), ymin=rep(0, nrow(as.melt)))
## draw graph
p <- ggplot(as.melt, aes(x=reorder(station, value),
                          y=value, group=station, color=station, ymax=ymax, ymin=ymin))
p <- p + geom_violin(trim=F, fill="#5B423D", linetype="blank", alpha=I(1/3))
p <- p + geom_pointrange(data=as.qua.melt, size=0.30)
p <- p + coord_flip()
p <- p + labs(x="", y="固定効果 [万円/㎡]")
p <- p + theme_bw(base_family = "HiraKakuProN-W3")
p <- p + theme(axis.text.x=element_text(size=6),
               axis.title.x=element_text(size=6),
               axis.text.y=element_text(size=6),
               legend.position="none")
plot(p)
ggsave(file="output/2hierarchical_station_train/station_specific_low.png",
       plot=p, dpi=300, width=4, height=3)

################################################################################
# Draw geo graphs using maptools
################################################################################

# preprocessing
################################################################################
# load geo map data
#kanagawa = readShapePoly('data/mesh03-tky-14-shp/mesh03-tky-14.shp')
kanagawa <- readShapePoly("data/mesh05-jgd-14-shp/mesh05-jgd-14.shp")
gpclibPermit()
df = fortify(kanagawa)

# load mantion and location data
locations = read.csv('data/locations.tsv')
mantions = read.csv('data/mantions.csv')

# samples grouped by station
mantions.grouped = summarise(group_by(mantions, station), n())
colnames(mantions.grouped)[2] = 'n'

# draw geo map with station full effect
################################################################################
# acquire fixed effect
hmc_samples = melt(la$r_s)
colnames(hmc_samples)[2] = 'station'
hmc_samples.grouped = summarise(group_by(hmc_samples, station),
                                mean(value), sd(value))
colnames(hmc_samples.grouped)[2:3] = c('effect', 'effect_sd')

# join data
geo_data = merge(merge(locations, mantions.grouped), hmc_samples.grouped)

# plot
p = ggplot(df)
p = p + geom_polygon(
  aes(long, lat, group=group),
  colour='gray90', fill='gray93', size=0.1
)
p = p + xlim(c(139.40, 139.80)) + ylim(c(35.30, 35.65))
p = p + coord_equal()
p = p + geom_point(
  data=geo_data, alpha=0.5,
  aes(x=long, y=lat, colour=effect, size=n)
)
p = p + scale_color_gradientn(colours=c('blue', 'green', 'red'))
p = p + scale_size_continuous(range=c(1, 7))
p <- p + theme_bw(base_family = "HiraKakuProN-W3")
p <- p + theme(axis.text.x=element_text(size=5),
               axis.title.x=element_text(size=8),
               axis.text.y=element_text(size=5),
               axis.title.y=element_text(size=8),
               legend.title=element_text(size=5),
               legend.text=element_text(size=5))
p <- p + labs(x='緯度', y='経度', colour='駅の固定効果', size='物件数')
p <- p + theme(
  panel.background = element_rect(
    fill = "white", colour = "black",
    size= 0.2 , linetype = 1
  )
)
plot(p)
ggsave(file='output/2hierarchical_station_train/geo_mapping_full.png',
       plot=p, dpi=600, width=6, height=4)

# draw geo map with station specific effect
################################################################################
# acquire fixed effect
hmc_samples = melt(la$as)
colnames(hmc_samples)[2] = 'station'
hmc_samples.grouped = summarise(group_by(hmc_samples, station),
                                mean(value), sd(value))
colnames(hmc_samples.grouped)[2:3] = c('effect', 'effect_sd')

# join data
geo_data = merge(merge(locations, mantions.grouped), hmc_samples.grouped)

# plot
p = ggplot(df)
p = p + geom_polygon(
  aes(long, lat, group=group),
  colour='gray90', fill='gray93', size=0.1
)
p = p + xlim(c(139.40, 139.80)) + ylim(c(35.30, 35.65))
p = p + coord_equal()
p = p + geom_point(
  data=geo_data, alpha=0.5,
  aes(x=long, y=lat, colour=effect, size=n)
)
p = p + scale_color_gradientn(colours=c('blue', 'green', 'red'))
p = p + scale_size_continuous(range=c(1, 7))
p <- p + theme_bw(base_family = "HiraKakuProN-W3")
p <- p + theme(axis.text.x=element_text(size=5),
               axis.title.x=element_text(size=8),
               axis.text.y=element_text(size=5),
               axis.title.y=element_text(size=8),
               legend.title=element_text(size=5),
               legend.text=element_text(size=5))
p <- p + labs(x='緯度', y='経度', colour='駅の固定効果', size='物件数')
p <- p + theme(
  panel.background = element_rect(
    fill = "white", colour = "black",
    size= 0.2 , linetype = 1
  )
)
plot(p)
ggsave(file='output/2hierarchical_station_train/geo_mapping_specific.png',
       plot=p, dpi=600, width=6, height=4)