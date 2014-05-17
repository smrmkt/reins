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
get_graph <- function(graph, data, color) {
  graph <- graph + geom_point(data=data, shape=20, size=3, col=color)
  return(graph + geom_smooth(data=data, method="lm", col=color))
}
g <- ggplot(data=subset(d, distance==1), aes(x=from, y=price))
for (i in 1:5) {
  g <- get_graph(g, subset(d, distance==i), i)
}
g <- g + xlim(1960, 2014) + ylim(0, 120) + xlab('築年 [年]') + ylab('平方単価 [万円]')
g <- g + theme_bw(base_family = "HiraKakuProN-W3")+
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16))
plot(g)
