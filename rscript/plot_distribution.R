library(gridExtra)
data <- read.csv("/Users/chaosun/Dropbox/beyondsome/osf_corpus/corpus_492ppl.csv", header = TRUE)

nonadj <- c("allowed", "few", "possible", "some", "sometimes" )
non.adj <- data[(data$Scale %in% nonadj), ]
non.adj <- ddply(non.adj, .(Scale, QID), summarise, Rating = mean(value))

plot.nonadj.corpus <- ggplot(non.adj, aes(x = Rating)) +
  geom_histogram(binwidth = .1)+ 
  labs(title = "Non-adjective scales", x="by-item mean rating", y ="Count")+
  scale_x_continuous(limits = c(1,7), breaks=c(1,2,3,4,5,6,7))+
  theme(panel.border = element_rect(colour = "grey90", fill=NA, size=1),
        panel.background = element_blank(),
        panel.grid.major = element_line(linetype = 'solid',colour = "grey90"), 
        panel.grid.minor = element_line(linetype = 'solid',colour = "grey90"))
plot.nonadj.corpus
skewness(non.adj$Rating) 


adj <- data[!(data$Scale %in% nonadj), ]

alladj <- ddply(adj, .(Scale, QID), summarise, Rating = mean(value))
plot.alladj <- ggplot(alladj, aes(x = Rating)) +
  geom_histogram(binwidth = .1)+ 
  labs(title = "Adjective scales", x="by-item mean rating", y ="Count")+
  scale_x_continuous(limits = c(1,7), breaks=c(1,2,3,4,5,6,7))+
  theme(panel.border = element_rect(colour = "grey90", fill=NA, size=1),
        panel.background = element_blank(),
        panel.grid.major = element_line(linetype = 'solid',colour = "grey90"), 
        panel.grid.minor = element_line(linetype = 'solid',colour = "grey90"))
plot.alladj
skewness(alladj$Rating) 
median(alladj$Rating)
mean(alladj$Rating)

pred <- ddply(adj[adj$predicative%in% c("1"),], .(Scale, QID), summarise, Rating = mean(value))
plot.pred <- ggplot(pred, aes(x = Rating)) +
  geom_histogram(binwidth = .1)+ 
  labs(title = "Adjective scales (predicative)", x="by-item mean rating", y ="Count")+
  scale_x_continuous(limits = c(1,7), breaks=c(1,2,3,4,5,6,7))+
  theme(panel.border = element_rect(colour = "grey90", fill=NA, size=1),
        panel.background = element_blank(),
        panel.grid.major = element_line(linetype = 'solid',colour = "grey90"), 
        panel.grid.minor = element_line(linetype = 'solid',colour = "grey90"))
plot.pred
skewness(pred$Rating) 
median(pred$Rating)
mean(pred$Rating)

non.pred <- ddply(adj[!(adj$predicative%in% c("1")),], .(Scale, QID), summarise, Rating = mean(value))
plot.non.pred <- ggplot(non.pred, aes(x = Rating)) +
  geom_histogram(binwidth = .1)+ 
  labs(title = "Adjective scales non-predicative", x="by-item mean rating", y ="Count")+
  scale_x_continuous(limits = c(1,7), breaks=c(1,2,3,4,5,6,7))+
  theme(panel.border = element_rect(colour = "grey90", fill=NA, size=1),
        panel.background = element_blank(),
        panel.grid.major = element_line(linetype = 'solid',colour = "grey90"), 
        panel.grid.minor = element_line(linetype = 'solid',colour = "grey90"))
plot.non.pred
skewness(non.pred$Rating) 
median(non.pred$Rating)
mean(non.pred$Rating)
shapiro.test(non.pred$Rating)

#### Sun et al. ####
inf <- read.csv("/Users/chaosun/Dropbox/My Experiments/Scalar Scale Offline Rating/scalar diversity paper data/scalar rating merge 230316.csv", header=TRUE)
inf <- inf[inf$Directness%in% "Direct",]
inf <- inf %>% 
  mutate(Rating_re = (Rating )/100)
byScalebyItem_Sun <- ddply(inf, .(Word,TrialList), summarise, Rating = mean(Rating_re))
colnames(byScalebyItem_Sun)[1] <- "Scale"
byScalebyItem_Sun<- byScalebyItem_Sun[byScalebyItem_Sun$Scale%in% unique(data$Scale),]


breaks <- pretty(range(byScalebyItem_Sun$Rating),
                 n = nclass.Sturges(byScalebyItem_Sun$Rating),
                 min.n = 1)

plot.nonadj.sun <- ggplot(byScalebyItem_Sun[byScalebyItem_Sun$Scale%in%nonadj,], aes(x = Rating)) +
  geom_histogram( breaks = breaks)+ 
  labs(title = "Non-adjective scales", x="by-item mean rating", y ="Count")+
  scale_x_continuous(limits = c(0,1))+
  theme(panel.border = element_rect(colour = "grey90", fill=NA, size=1),
        panel.background = element_blank(),
        panel.grid.major = element_line(linetype = 'solid',colour = "grey90"), 
        panel.grid.minor = element_line(linetype = 'solid',colour = "grey90"))
plot.nonadj.sun
skewness(byScalebyItem_Sun[byScalebyItem_Sun$Scale%in%nonadj,]$Rating)

plot.adj.sun <- ggplot(byScalebyItem_Sun[!(byScalebyItem_Sun$Scale%in%nonadj),], aes(x = Rating)) +
  geom_histogram( breaks = breaks)+ 
  labs(title = "Adjective scales_Sun", x="by-item mean rating", y ="Count")+
  scale_x_continuous(limits = c(0,1))+
  theme(panel.border = element_rect(colour = "grey90", fill=NA, size=1),
        panel.background = element_blank(),
        panel.grid.major = element_line(linetype = 'solid',colour = "grey90"), 
        panel.grid.minor = element_line(linetype = 'solid',colour = "grey90"))
plot.adj.sun
skewness(byScalebyItem_Sun[!(byScalebyItem_Sun$Scale%in%nonadj),]$Rating) 
median(byScalebyItem_Sun$Rating)

x <- grid.arrange(plot.nonadj.corpus,plot.nonadj.sun, ncol = 2)
ggsave(filename = "/Users/chaosun/Dropbox/beyondsome/osf_corpus/distribution_nonadj.jpeg", x, width = 6, height = 3, dpi = 300, units = "in", device='jpeg')

y <- grid.arrange(plot.alladj, plot.pred,plot.adj.sun, ncol = 3)
ggsave(filename = "/Users/chaosun/Dropbox/beyondsome/osf_corpus/distribution_adj.jpeg", y, width = 9, height = 3, dpi = 300, units = "in", device='jpeg')

