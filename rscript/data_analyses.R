library(e1071) 
library(plyr)
library(ggplot2)
library(gridExtra)
data <- read.csv(".../corpus_492ppl.csv", header = TRUE)
prev <- read.csv(".../previous_results.csv", header = TRUE)

data$Rescale <- (data$value -1)/6
byScalebyItem <- ddply(data, .(Scale, QID), summarise, Rating = mean(Rescale))
byScale <- ddply(byScalebyItem, .(Scale), summarise, CorpusRating = mean(Rating))
combine <- merge(byScale, prev, by="Scale")

# kendall's tau
cor.test(combine$CorpusRating, combine$BobExp2, method="kendall") 
cor.test(combine$CorpusRating, combine$SunInf, method="kendall") 

# kendall's w
library(irr)
kendall(combine[,c("CorpusRating","BobExp2","SunInf")], TRUE)

# Levene
library(car)
l <- reshape(combine[, c("Scale","CorpusRating","BobExp2","SunInf")], direction = "long", 
             varying = list(names(combine[, c("Scale","CorpusRating","BobExp2","SunInf")])[c(2,3,4)]), 
             v.names = "Results", 
             #idvar = "Scale", 
             timevar = "Task", 
             times = c("CorpusRating","BobExp2","SunInf"))

l$Task <- factor(l$Task, levels = c("BobExp2","SunInf", "CorpusRating"))
leveneTest(Results ~ Task, data = l[l$Task!="SunInf",])
leveneTest(Results ~ Task, data = l[l$Task!="BobExp2",])

#### regression ####

library(sjPlot)

combine$Gramma <- factor(combine$Gramma)
combine$Boundedness <- factor(combine$Boundedness)

m1<- lm(CorpusRating ~ Cloze +  Gramma + Freq + LSA + Distance + Boundedness + So + But, 
        data = combine)
m2 <- lm(BobExp2 ~ Cloze + Gramma + Freq + LSA + Distance + Boundedness + So +But, 
         data = combine)
m3 <- lm(SunInf ~ Cloze + Gramma + Freq + LSA + Distance + Boundedness + So +But, 
         data = combine)
tab_model(m1, m2, m3, show.se = TRUE, show.ci = FALSE, show.r2 = TRUE,
          string.est = "\u03B2", 
          string.se = "SE",
          digits.p = 2,
          pred.labels = c("(Intercept)", "Association strength", "Grammatical class", 
                          "Word frequencies", "Semantic relatedness", "Semantic distance",
                          "Boundedness", "Enrichability", "Homogeneity"),
          dv.labels = c( "Paraphrase task", "van Tiel et al.", "Sun et al.")
)

#### barplot ####
l <- l[order(l$Task, -l$Results),]
l$Scale <- factor(l$Scale, levels = l$Scale[order(l$Task, l$Results)[1:28]])
l$Task <- factor(l$Task, levels = c("BobExp2","SunInf", "CorpusRating"))

# ggplot(l, aes(x=Scale, y=Results)) +
#   geom_point(aes(color = Task, shape=Task))+
#   coord_flip()

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
ggsave(filename = ".../barplot.jpeg", barplot, width = 5, height = 6, dpi = 300, units = "in", device='jpeg')
nrow(byScalebyItem[byScalebyItem$Rating<5 & byScalebyItem$Rating>3,])/nrow(byScalebyItem)
