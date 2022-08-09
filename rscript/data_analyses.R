library(e1071) 
library(plyr)
library(ggplot2)
library(gridExtra)
data <- read.csv("/Users/chaosun/Dropbox/beyondsome/osf_corpus/corpus_492ppl.csv", header = TRUE)

# by scalar implicature rating
byScalebyItem <- ddply(data, .(Scale, QID), summarise, Rating = mean(value))
byScale <- ddply(byScalebyItem, .(Scale), summarise, CorpusRating = mean(Rating))

prev <- read.csv("/Users/chaosun/Dropbox/beyondsome/osf_corpus/previous_results.csv", header = TRUE)
combine <- merge(byScale, prev, by="Scale")
# kendall's tau
cor.test(combine$CorpusRating, combine$BobExp2, method="kendall") 
cor.test(combine$CorpusRating, combine$SunInf, method="kendall") 

# kendall's w
library(irr)
kendall(combine[,c("CorpusRating","BobExp2","SunInf")], TRUE)

# Levene
data$Rescale <- (data$value -1)/6
byScalebyItem <- ddply(data, .(Scale, QID), summarise, Rating = mean(Rescale))
byScale <- ddply(byScalebyItem, .(Scale), summarise, CorpusRating = mean(Rating))
combine <- merge(byScale, prev, by="Scale")

library(car)
l <- reshape(combine[, c("Scale","CorpusRating","BobExp2","SunInf")], direction = "long", 
             varying = list(names(combine[, c("Scale","CorpusRating","BobExp2","SunInf")])[c(2,3,4)]), 
             v.names = "Results", 
             #idvar = "Scale", 
             timevar = "Task", 
             times = c("CorpusRating","BobExp2","SunInf"))

l$Task <- factor(l$Task, levels = c("BobExp2","SunInf", "CorpusRating"))
leveneTest(Results ~ Task, data = l)
# boxplot
dodge_width <- .2
boxplot<- ggplot(l, aes(x=Task, y=Results, color=Task)) +
  geom_boxplot(aes(as.numeric(Task)- dodge_width, y=Results, color=Task), width=0.3)+
  geom_jitter(aes(as.numeric(Task)+ dodge_width, y=Results, color=Task), 
              position = position_jitter(width = .1)) +
  scale_x_continuous(breaks = c(1,2,3), labels = c("van Tiel et al.", "Sun et al.", "Paraphrase task"))+
  ylab("Implicature rates/ratings")+
  scale_color_discrete(name = "Task", labels = c("van Tiel et al.", "Sun et al.", "Paraphrase task"))+
  theme(axis.title.x = element_blank(),
        legend.position="none")
ggsave(filename = "/Users/chaosun/Dropbox/beyondsome/osf_corpus/boxplot.jpeg", boxplot, width = 5, height = 3, dpi = 300, units = "in", device='jpeg')

summary(combine$CorpusRating)
IQR(combine$CorpusRating)

#### regression ####
model <- lm(CorpusRating ~ Cloze +  Gramma + Freq + LSA + Distance + Boundedness + So +But, data = combine)
summary(model)
library(sjPlot)
tab_model(model, show.se = TRUE, show.ci = FALSE, show.r2 = TRUE,
          #string.est = "\u03B2", 
          string.se = "SE",
          digits.p = 2,
          pred.labels = c("(Intercept)", "Association strength", "Grammatical class", "Word frequencies", "Semantic relatedness", 
                          "Semantic distance", "Boundedness", "Enrichability", "Homogeneity"),
          dv.labels = c("Corpus-based paraphrase task")
)

#### barplot ####
l <- l[order(l$Task, -l$Results),]
l$Scale <- factor(l$Scale, levels = l$Scale[order(l$Task, l$Results)[1:28]])
l$Task <- factor(l$Task, levels = c("BobExp2","SunInf", "CorpusRating"))
barplot <- ggplot(l, aes(x=Scale, y=Results, fill=Task)) +
  geom_bar(stat="identity", width = 0.6, position=position_dodge())+ 
  labs(x="", y ="Mean implicature rates/ratings")+
  scale_y_continuous(limits = c(0, 1),expand = c(0, 0))+
  scale_fill_discrete(name = "Task", labels = c("van Tiel et al.", "Sun et al.", "Paraphrase task"))+
  coord_flip()+ 
  theme(
    legend.position="bottom",
    legend.title=element_blank(),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.border = element_rect(colour = "grey90", fill=NA, size=1), #draws nothing, and assigns no space
    panel.grid.major = element_line(linetype = 'solid',colour = "grey90"), 
    panel.grid.minor = element_line(linetype = 'solid',colour = "grey90"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
ggsave(filename = "/Users/chaosun/Dropbox/beyondsome/osf_corpus/barplot.jpeg", barplot, width = 5, height = 6, dpi = 300, units = "in", device='jpeg')
nrow(byScalebyItem[byScalebyItem$Rating<5 & byScalebyItem$Rating>3,])/nrow(byScalebyItem)
