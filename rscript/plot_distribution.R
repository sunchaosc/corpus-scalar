library(gridExtra)
data <- read.csv("/Users/chaosun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Writings/beyondsome/osf_corpus/corpus_492ppl.csv", header = TRUE)

nonadj <- c("allowed", "few", "possible", "some", "sometimes" )
non.adj <- data[(data$Scale %in% nonadj), ]

adj <- data[!(data$Scale %in% nonadj), ]

alladj <- ddply(adj, .(Scale, QID), summarise, Rating = mean(value))
alladj$Scale <- factor(alladj$Scale)
levels <- levels(factor(alladj$Scale))

x <- ggplot(alladj, aes(y = Rating)) +
  facet_wrap(~Scale,nrow=4)+
  geom_dotplot( aes(x=1-0.2, y=Rating), binaxis='y', stackdir='center', dotsize=0.5, position=position_dodge(0.4))+
  stat_summary(aes(x=1+0.2, y=Rating), fun.data=mean_cl_boot,  geom="errorbar", width=0.05,position=position_dodge(0.4)) +
  stat_summary(aes(x=1+0.2, y=Rating), fun.y=mean,  geom="point", size=0.8, position=position_dodge(0.4))+
  scale_y_continuous(limits = c(1,7), breaks=c(1,2,3,4,5,6,7))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",
        legend.title=element_blank())
ggsave(filename = ".../adj dist.jpeg", x, width = 6, height = 9, dpi = 300, units = "in", device='jpeg')

plot.alladj <- ggplot(alladj, aes(x = Rating)) +
  geom_histogram(binwidth = .1)+ 
  labs(title = "Paraphrase task -- adjective items", x="by-item mean rating", y ="Count")+
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
  labs(title = "Paraphrase task -- predicative subset", x="by-item mean rating", y ="Count")+
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
