
library(ggplot2)
library(plyr)



qplot(Year, Duff, fill=factor(Age), data=ASF, geom="boxplot", position="dodge")+theme_bw()

##qplot(Year, Litter, fill=factor(Age), data=ASF, geom="boxplot", position="dodge")+theme_bw()

##qplot(Year, Fuelbed, fill=factor(Age), data=ASF, geom="boxplot", position="dodge")+theme_bw()


duff.avg <-ddply (ASF, c(“Year”, “Age”), summarize, mean(Duff), sd(Duff))

#create the barplot componentv
avg.plot<-qplot(Duff, duff.avg, fill=factor(Age), data=duff.avg, geom="bar", position="dodge")

#add error bars
avg.plot+geom_errorbar(aes(ymax=duff.avg+duff.sd, ymin=duff.avg-duff.sd), position="dodge")+theme_bw()