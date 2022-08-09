library(reshape2)
library(tidyr)
library(plyr)

data <- read.csv("/Users/chaosun/Dropbox/beyondsome/osf_corpus/raw_transpose_590ppl.csv", header=TRUE, na.strings=c("","NA"))
exp <- melt(data, id.var = 'QID', variable.name = "Subject")
nlevels(factor(exp$Subject)) #590ppl
exp <- separate(exp, QID, c("QID", "Group", "Scale"),"_")
exp <- exp[complete.cases(exp),] # remove empty submission 
nlevels(factor(exp$Subject)) #528ppl   
bysubject_total_trial <- ddply(exp, .(Subject), summarise, total_trial = length(QID))
incomplete <- bysubject_total_trial[bysubject_total_trial$total_trial<28,]$Subject
length(incomplete) #28ppl incomplete submissions
exp <- exp[!(exp$Subject %in% incomplete),] #500 valid submissions

demogr <- data[c(1:3),]
demogr <- melt(demogr, id.var = 'QID', variable.name = 'Subject')
demogr <- demogr[(demogr$Subject%in% exp$Subject),]

summary(factor(demogr[demogr$QID=="Language",]$value))
non.native <- unique(demogr[demogr$value%in%c("Chinese", "Russian","Slovak", "Spanish", "Tamil","Thai", NA),]$Subject)

demogr$value <- gsub('Thirty Six', '36', demogr$value )
mean(as.numeric(demogr[demogr$QID=="Age",]$value),na.rm=TRUE)
sd(as.numeric(demogr[demogr$QID=="Age",]$value),na.rm=TRUE)
summary(factor(demogr[demogr$QID=="Gender",]$value),na.rm=TRUE) #male 1

exp <- exp[!(exp$Subject %in% non.native),] #8ppl
nlevels(factor(exp$Subject)) #492ppl in total
exp$Scale <- gsub('10 funny', 'funny', exp$Scale)
exp[exp$Scale=="funny",]$Group <- gsub('Q', 'Q10', exp[exp$Scale=="funny",]$Group)

item <- read.csv("/Users/chaosun/Dropbox/beyondsome/osf_corpus/items.csv", header=TRUE)
exp <- merge(exp, item, by=c("Scale", "QID"))
write.csv(exp,file="/Users/chaosun/Dropbox/beyondsome/osf_corpus/corpus_492ppl.csv",row.names = FALSE)

