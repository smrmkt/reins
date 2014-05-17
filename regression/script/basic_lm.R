# load library
library('psych')
library('ggplot2')

# load data
d <- read.delim('data/mantions.csv', header=T, sep=',')

# descriptive stats.
pairs.panels(d[,14:18])
plot(d$from, d$price)
plot(d$distance, d$price)

# simple regresion
summary(lm(price~space+room+distance+from, data=d))

# regression coefficient
coefs <- c()
for (i in 1:5) {
  # lm
  d.part <- subset(d, distance==i)
  d.part.lm <- lm(price~space+room+from, data=d.part)
  coefs[i] <- summary(d.part.lm)$coefficients[4]
}

# plot
qcolors <- c("#720da8", "#f39800", "#009944", "#0068b7", "#e4007f")
get_graph <- function(graph, data, color) {
  graph <- graph + geom_point(data=data, shape=20, size=3, col=color)
  #return(graph + geom_smooth(data=data, col=color)) #局所多項式回帰の場合
  return(graph + geom_smooth(data=data, method="lm", col=color))
}
g <- ggplot(data=d, aes(x=from, y=price, fill=reorder(distance_raw, distance)))
for (i in 1:5) {
  g <- get_graph(g, subset(d, distance==i), qcolors[i])
}
g <- g + xlim(1960, 2014) + ylim(0, 120) + xlab('築年 [年]') + ylab('平方単価 [万円]')
g <- g + labs(fill='駅からの距離')
g <- g + theme_bw(base_family = "HiraKakuProN-W3")+
  theme(axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        legend.title=element_text(size=20),
        legend.text=element_text(size=16))
plot(g)
