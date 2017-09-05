library(sciplot)
library(agricolae)
library(ggplot2)
library(lattice)
library(AICcmodavg)

setwd("~/Dropbox/PhD Program/FieldSeason_2013/2013_Datasheets/FuelsSites_2013/Fuel_Output_Sheets/SFuels_R")
CFL <- read.csv("~/Dropbox/PhD Program/FieldSeason_2013/2013_Datasheets/FuelsSites_2013/Fuel_Output_Sheets/SFuels_R/CFL.csv")
CC <- read.csv("~/Dropbox/PhD Program/FieldSeason_2013/2013_Datasheets/FuelsSites_2013/Fuel_Output_Sheets/SFuels_R/CC.csv")
#attach(CFL)
#CFL <- CFL[CFL$Species=="PIEN",]

#Convert kg/m2 to MG/ha
CFL$AFCL <- CFL$AFCL*10
CFL$LCW <- CFL$LCW*10
CFL$DCW <- CFL$DCW*10

#Create categories for Live and Dead based on Status groupings
CFL$LiDe <- CFL$Status
levels(CFL$LiDe) <- c(levels(CFL$LiDe), "Dead")
CFL[CFL$Status %in% c("Gr", "Ye", "Nd", "Tw", "Br", "Sn", "De"), "LiDe"] <- "Dead"

#--------------------------------------------------
#Aggregate all plots to site level
CFL.sum <- aggregate(cbind(CBD, AFCL, LCW ,DCW,BA_Li,BA_Gr, BA_Ye,BA_Nd,BA_Tw,BA_Br,BA_Sn,BA_De) ~ Site + Year + Age + Plot, data=CFL, FUN=sum)
CFL.mean <- aggregate(cbind(CBD, AFCL, LCW ,DCW,BA_Li,BA_Gr, BA_Ye,BA_Nd,BA_Tw,BA_Br,BA_Sn,BA_De) ~  Year + Site +Age, data=CFL, FUN=mean)
CC.site <- aggregate(cbind(CanopyOpen,CanopyCover) ~ Year +Age, data=CC, FUN=min)
CFL.plot <- aggregate(cbind(LiFol,DeFol,Li.1hr,De.1hr,Li.10hr,De.10hr,Li.100hr,De.100hr,CBH,LCW,DCW,AFCL,CBD,BA_Li,BA_Gr,
                            BA_Ye,	BA_Nd,	BA_Tw,	BA_Br,	BA_Sn,	BA_De) ~ Site + Year + Age +Plot, data=CFL, FUN=sum)

CFL.site <- aggregate(cbind(LiFol,DeFol,Li.1hr,De.1hr,Li.10hr,De.10hr,Li.100hr,De.100hr,CBH,LCW,DCW,AFCL,CBD,BA_Li,BA_Gr,
                            BA_Ye,BA_Nd,BA_Tw,BA_Br,BA_Sn,BA_De) ~ Site + Year + Age, data=CFL.plot, FUN=mean)
ACFL.sum <- aggregate(cbind(AFCL) ~ Site + Year + Age + Plot + LiDe, data=CFL, FUN=sum)
#--------------------------------------------------
CFL.cpy <- CFL

CFL.basum <- CFL.sum[, c(1,2,5:16)]
CFL.bamean <- CFL.mean[, c(2,3:15)]
CFL.bcpy <- CFL[, c(1,2,21,22,23,24,25,26,27,28)]
CFL.bap <- CFL.plot[, c(2,3,17,18,19,20,21,22,23,24)]
CFL.bas <- CFL.site[, c(2,3,17,18,19,20,21,22,23,24)]

#Basal area class for Gr, Ye, and Nd to represent total area of trees with > 50% needle retention
CFL.basum$BA_l50pct.nd <- CFL.basum$BA_Gr + CFL.basum$BA_Ye + CFL.basum$BA_Nd
CFL.bamean$BA_l50pct.nd <- CFL.bamean$BA_Gr + CFL.bamean$BA_Ye + CFL.bamean$BA_Nd
CFL.bcpy$BA_l50pct.nd <- CFL.bcpy$BA_Gr + CFL.bcpy$BA_Ye + CFL.bcpy$BA_Nd
CFL.bas$BA_l50pct.nd <- CFL.bas$BA_Gr + CFL.bas$BA_Ye + CFL.bas$BA_Nd
CFL.bap$BA_l50pct.nd <- CFL.bap$BA_Gr + CFL.bap$BA_Ye + CFL.bap$BA_Nd


#Basal area for class Br and Sn to represent total area of trees with <50% 1-hr fuels
CFL.basum$BA_l50pct.tw <- CFL.basum$BA_Br + CFL.basum$BA_Sn
CFL.bamean$BA_l50pct.tw <- CFL.bamean$BA_Br + CFL.bamean$BA_Sn
CFL.bcpy$BA_l50pct.tw <- CFL.bcpy$BA_Br + CFL.bcpy$BA_Sn
CFL.bas$BA_l50pct.tw <- CFL.bas$BA_Br + CFL.bas$BA_Sn
CFL.bap$BA_l50pct.tw <- CFL.bap$BA_Br + CFL.bap$BA_Sn

#Attach the new calculations to the dataframe
CFL.sum$BA_l50pct.nd <- CFL.basum$BA_l50pct.nd
CFL.sum$BA_l50pct.tw <- CFL.basum$BA_l50pct.tw
CFL.mean$BA_l50pct.nd <- CFL.bamean$BA_l50pct.nd
CFL.mean$BA_l50pct.tw <- CFL.bamean$BA_l50pct.tw
CFL.cpy$BA_l50pct.nd <- CFL.bcpy$BA_l50pct.nd
CFL.cpy$BA_l50pct.tw <- CFL.bcpy$BA_l50pct.tw
CFL.plot$BA_l50pct.nd <- CFL.bap$BA_l50pct.nd
CFL.plot$BA_l50pct.tw <- CFL.bap$BA_l50pct.tw
CFL.site$BA_l50pct.nd <- CFL.bas$BA_l50pct.nd
CFL.site$BA_l50pct.tw <- CFL.bas$BA_l50pct.tw

#Combine Age and Year to preform Tukey HSD
CFL.sum$SB.Age <- with(CFL.sum, paste0(Year, Age))
CFL.mean$SB.Age <- with(CFL.mean, paste0(Year, Age))
CFL.cpy$SB.Age <- with(CFL.cpy, paste0(Year, Age))
CFL.plot$SB.Age <- with(CFL.plot, paste0(Year, Age))
CFL.site$SB.Age <- with(CFL.site, paste0(Year, Age))
CC$SB.Age <- with(CC, paste0(Year, Age))


CFL.s1 <- CFL.plot
CFL.s1$Plot.size<-CFL.s1$Species<-CFL.s1$Status <-NULL
CFL.s2 <- CFL.sum
CFL.s2$Year <- CFL.s2$Age <- NULL

#Create ANOVA models and post-hoc Tukey HSD.
#Response variable will be looped, but the explanatory variable is a merged Year and Age varaible (i.e., Old1960, Young1990, etc.)  

#Produces a Word Document output of a full ANOVA summary and the Tukey HSD groups meaningful for the graphs.
#sink("ANOVA_Tukey.doc")
#for(i in 1:(ncol(CFL.s)-1)){
#  columns <- names(CFL.s[i])
#  aov.out <- aov(CFL.s[,i] ~ SB.Age, data = CFL.s)
#  aov.sum <- summary(aov(CFL.s[,i] ~ SB.Age, data = CFL.s))
#  tukey.out <- HSD.test(aov(CFL.s[,i] ~ SB.Age, data = CFL.s), "SB.Age")$groups
#  print(columns)
#  print(aov.sum)
#  print(tukey.out)
#}
#sink()

#titles <- c("LiFol","DeFol","Li.1hr","De.1hr","Li.10hr","De.10hr","Li.100hr","De.100hr","CBH","LCW","DCW","AFCL","CBD","BA_Li","BA_Gr","BA_Ye","BA_Nd","BA_Tw","BA_Br","BA_Sn","BA_De","BA_l50pct.nd", "BA_l50pct.tw")
tukey1<- NULL
aov1 <- NULL
aov.sum1 <- NULL
aicc <- NULL
CV1 <- NULL
for(i in c("LiFol","DeFol","Li.1hr","De.1hr","Li.10hr","De.10hr","Li.100hr","De.100hr")) 
        aov1[[i]] <- aov(CFL.s1[,i] ~ SB.Age + Site/Plot, data = CFL.s1)
for(i in c("LiFol","DeFol","Li.1hr","De.1hr","Li.10hr","De.10hr","Li.100hr","De.100hr","CBH"))
        tukey1[[i]] <- HSD.test(aov1[[i]], "SB.Age")
for(i in c("LiFol","DeFol","Li.1hr","De.1hr","Li.10hr","De.10hr","Li.100hr","De.100hr","CBH"))
        aov.sum1[[i]] <- summary(aov1[[i]])
for(i in c("LiFol","DeFol","Li.1hr","De.1hr","Li.10hr","De.10hr","Li.100hr","De.100hr","CBH"))
        CV1[[i]] <- tukey1[[i]]$statistics[,2]

  
tukey2<- NULL
aov2 <- NULL
aov.sum2 <- NULL
aicc2 <- NULL
CV2 <- NULL
for(i in c("CBD", "AFCL","LCW","DCW","BA_Li","BA_Gr","BA_Ye","BA_Nd","BA_Tw",
           "BA_Br","BA_Sn","BA_De","BA_l50pct.nd","BA_l50pct.tw")) aov2[[i]] <- aov(CFL.s2[,i] ~ SB.Age+ Site/Plot, data = CFL.s2)
for(i in c("CBD", "AFCL","LCW","DCW","BA_Li","BA_Gr","BA_Ye","BA_Nd","BA_Tw",
           "BA_Br","BA_Sn","BA_De","BA_l50pct.nd","BA_l50pct.tw")) tukey2[[i]] <- HSD.test(aov2[[i]], "SB.Age")
for(i in c("CBD", "AFCL","LCW","DCW","BA_Li","BA_Gr","BA_Ye","BA_Nd","BA_Tw",
           "BA_Br","BA_Sn","BA_De","BA_l50pct.nd","BA_l50pct.tw")) aov.sum2[[i]] <- summary(aov2[[i]])

CV1 <-NULL
for(i in c( "EndemicOld", "EndemicYoung","1940Old", "1960Old", "1990Old", "2000Old", "2010Old", "1990Young", "2000Young")){
  q<- subset(CFL.s2, SB.Age== i)
  t <- aov(AFCL~Site, data=q)
  CV1[[i]] <- cv.model(t)
}
CV2 <-NULL
SE2 <- NULL
for(i in c( "EndemicOld", "EndemicYoung","1940Old", "1960Old", "1990Old", "2000Old", "2010Old", "1990Young", "2000Young")){
  q<- subset(CFL.s1, SB.Age== i)
  t <- aov(CBD~Site, data=q)
  CV2[[i]] <- cv.model(t)
  SE2[[i]] <- summary(t)$coefficients[, 2]
}

CV2010 <- NULL
for(i in c("CBD", "AFCL","LCW","DCW","BA_Li","BA_Gr","BA_Ye","BA_Nd","BA_Tw",
           "BA_Br","BA_Sn","BA_De","BA_l50pct.nd","BA_l50pct.tw")){
  q1 <- subset(CFL.s2, SB.Age== "2010Young")
  CV <- function(mean, sd){
    (sd/mean)*100 }
  m <- mean(q1[,i])
  sd <- sd(q1[,i])
  CV2010[[i]] <- CV(mean = m, sd=sd)
}


  #aicc2[[i]] <- AICc(aov1[[i]])
  
cc.aov <- aov(CanopyOpen~SB.Age, data=CC)
cc.hsd <- HSD.test(cc.aov, "SB.Age")
cc.aicc <- AICc(cc.aov)




#--------------------------------------------------
#Create bargraphs with standard error bars for all live and dead crown fuelloads
pdf(file="Live_Dead_CFL_Spruce.pdf", width = 10, height = 9)
par(mfrow = c(4,3),#mar = c(3, 3.5, 1, 0.5),mgp = c(2, 1, 0),oma = c(1, 0, 1, 0))
    mar=c(4, 4,0.5, 0.5)+0.2)
op <- par(family = "Times")
bg.LiFol = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=LiFol,family = "Times",
                                      lc=FALSE, err.width = .03,  ylab = "Live Foliage (MG/ha)",
                                      legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$LiFol,groups[order(groups$trt),])
text(x = bg.LiFol$xvals[c(1,3,5:15)], y=bg.LiFol$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(a) F(9,32) = 23.41, p = < 0.0001", 1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.DeFol = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=DeFol,family = "Times",
                                      lc=FALSE, err.width = .03,  ylab = "Dead Foliage (MG/ha)",
                                      legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$DeFol,groups[order(groups$trt),])
text(x = bg.DeFol$xvals[c(1,3,5:12)], y=bg.DeFol$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(b) F(9,32) = 2.25, p = 0.0447", 1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.CBH = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=CBH,family = "Times",
                                    lc=FALSE, err.width = .03,  ylab = "CBH (m)",
                                    legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$CBH,groups[order(groups$trt),])
text(x = bg.CBH$xvals[c(1,3,5:12)], y=bg.CBH$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(c) F(9,32) = 5.36, p = < 0.0001", 1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.Li.1hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=Li.1hr,family = "Times",
                                       lc=FALSE, err.width = .03,  ylab = "Live 1hr (MG/ha)",
                                       legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$Li.1hr,groups[order(groups$trt),])
text(x = bg.Li.1hr$xvals[c(1,3,5:12)], y=bg.Li.1hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(d) F(9,32) = 28, p = < 0.0001",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.De.1hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=De.1hr,family = "Times",
                                       lc=FALSE, err.width = .03, ylab = "Dead 1hr (MG/ha)",
                                       legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$De.1hr,groups[order(groups$trt),])
text(x = bg.De.1hr$xvals[c(1,3,5:12)], y=bg.De.1hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(e) F(9,32) = 0.47, p = 0.89", 1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.CBD = with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=CBD,family = "Times",
                                   lc=FALSE, err.width = .03, ylab = "CBD (kg/m2)",
                                   legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey2$CBD,groups[order(groups$trt),])
text(x = bg.CBD$xvals[c(1,3,5:12)], y=bg.CBD$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(f) F(9,32) = 5.92, p = < 0.0001",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.Li.10hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=Li.10hr,family = "Times",
                                        lc=FALSE, err.width = .03,  ylab = "Live 10hr (MG/ha)",
                                        legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$Li.10hr,groups[order(groups$trt),])
text(x = bg.Li.10hr$xvals[c(1,3,5:12)], y=bg.Li.10hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(g) F(9,32) = 31.61, p = < 0.0001",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.De.10hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=De.10hr,family = "Times",
                                        lc=FALSE, err.width = .03, ylab = "Dead 10hr (MG/ha)",
                                        legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$De.10hr,groups[order(groups$trt),])
text(x = bg.De.10hr$xvals[c(1,3,5:12)], y=bg.De.10hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(h) F(9,32) = 1.88, p = 0.0924",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.AFCL = with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=AFCL,family = "Times",
                                    lc=FALSE, err.width = .03,  ylab = "ACFL (MG/ha)",
                                    legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$AFCL,groups[order(groups$trt),])
text(x = bg.AFCL$xvals[c(1,3,5:12)], y=bg.AFCL$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(i) F(9,32) = 24.01, p = < 0.0001",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.Li.100hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=Li.100hr,family = "Times",
                                         lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Live 100hr (MG/ha)",
                                         legend=T, x.leg=1.3,y.leg = 2.2, ncol =2,cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=T, col= c("dark gray", "white")))
x <- with(tukey1$Li.100hr,groups[order(groups$trt),])
text(x = bg.Li.100hr$xvals[c(1,3,5:12)], y=bg.Li.100hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(j) F(9,32) = 57.59, p = < 0.0001",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.De.100hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=De.100hr,family = "Times",
                                         lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Dead 100hr (MG/ha)",
                                         legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=T, col= c("dark gray", "white")))
x <- with(tukey1$De.100hr,groups[order(groups$trt),])
text(x = bg.De.100hr$xvals[c(1,3,5:12)], y=bg.De.100hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(k) F(9,32) = 1.13, p = 0.37", 1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.CC = with(CC, bargraph.CI(x.factor=Year, group=Age, response=CanopyOpen,family = "Times",
                             lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Canopy Openness (%)",
                             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=T, col= c("dark gray", "white")))
x <- with(cc.hsd,groups[order(groups$trt),])
text(x = bg.CC$xvals[c(1,3,5:12)], y=bg.CC$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#mtext("(l) F(9,32) = 2.53, p = 0.045",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])
par(op)
dev.off()

#--------------------------------------------------
#pdf(file="CrownWeight.pdf", width = 8, height = 9)
#par(mfrow = c(4,2))
#op <- par(family = "Times")
#par(mfrow = c(3,2))
#bg.lcw = with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=LCW,family = "Times",
#             lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "LCW (kg/m2)",
#             legend=F, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1, col= c("dark gray", "white")))
#x <- with(tukey2$LCW,groups[order(groups$trt),])
#text(x = bg.lcw$xvals[c(1,3,5:12)], y=bg.lcw$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")#

#bg.dcw = with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=DCW,family = "Times",
#             lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "DCW (kg/m2)",
#             legend=F, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1, col= c("dark gray", "white")))
#x <- with(tukey2$DCW,groups[order(groups$trt),])
#text(x = bg.dcw$xvals[c(1,3,5:12)], y=bg.dcw$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
#par(op)
#dev.off()
#(4, 4,0.5, 0.5--------------------------------------------------
#Plot the basal areas

#Export to reformat 
#write.table(CFL.stk, file = "CFLstk_unformatted.csv", sep = ",")
#Import formatted file
#CFL.restk <- read.csv("~/Dropbox/PhD Program/FieldSeason_2013/2013_Datasheets/FuelsSites_2013/Fuel_Output_Sheets/SFuels_R/CFL.stk.csv")

pdf(file="BasalArea_Spruce.pdf", width = 5, height = 7)
par(mfrow = c(4,1),
    oma = c(1,1,1,1)+0.1,
    mar = c(3,4,1,1))
op <- par(family = "Times")
#Plot of live basal area
bg.li <- with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=BA_Li,axisnames=F,
                                   lc=FALSE, err.width = .03, ylab = "Basal Area (m2/ha)",ylim = c(0,250),
                                   x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1,family = "Times", col= c("dark gray", "white")))
x <- with(tukey1$BA_Li,groups[order(groups$trt),])
text(x = bg.li$xvals[c(1,3,5:12)], y=bg.li$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(a) Live", 2, adj=0, las=1, padj = -7.5,cex = 0.75)
mtext("F(9,32) = 7.4, p = < 0.0001", 2, adj=-0.1, las=1, padj = -7,cex = 0.55)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.li$xvals[c(1,3,5:12)])
#Plot combining the basal area class for Gr, Ye, and Nd to represent total area of trees with greater than 50% needle retention
bg.g50pct.nd <- with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=BA_l50pct.nd,ylim = c(0,60),axisnames=F,
                                          lc=FALSE, err.width = .03,  ylab = "Basal Area (m2/ha)",
                                          x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1,family = "Times", col= c("dark gray", "white")))
x <- with(tukey1$BA_l50pct.nd,groups[order(groups$trt),])
text(x = bg.g50pct.nd$xvals[c(1,3,5:12)], y=bg.g50pct.nd$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(b) > 50% needle retention",  2, adj=0, las=1, padj = -7.5,cex = 0.75)
mtext("F(9,32) = 5.55, p = < 0.0001", 2, adj=-0.1, las=1, padj = -7,cex = 0.55)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.li$xvals[c(1,3,5:12)])

#Plot of twig basal area- < 50% red needles retained in the canopy; >50% 1-hr twigs remain but are devoid of needles 
bg.tw <- with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=BA_Tw, ylim = c(0,150),axisnames=F,
                                   lc=FALSE, err.width = .03,  ylab = "Basal Area (m2/ha)",
                                   x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1,family = "Times", col= c("dark gray", "white")))
x <- with(tukey1$BA_Tw,groups[order(groups$trt),])
text(x = bg.tw$xvals[c(1,3,5:12)], y=bg.tw$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(c) < 50% needles, > 50%",  2, adj=0, las=1, padj = -7.5,cex = 0.75)
mtext("1-hr; F(9,32) = 4.62, p = < 0.0001", 2, adj=-0.1, las=1, padj = -7,cex = 0.55)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.li$xvals[c(1,3,5:12)])

#Plot of branch basal area- < 50% 1-hr fuels
bg.br <- with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=BA_l50pct.tw, ylim = c(0,125),axisnames=T,
                                   lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Basal Area (m2/ha)",
                                   x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1,family = "Times", col= c("dark gray", "white")))
x <- with(tukey1$BA_l50pct.tw,groups[order(groups$trt),])
text(x = bg.br$xvals[c(1,3,5:12)], y=bg.br$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(d) < 50% 1-hr ", 2, adj=0, las=1, padj = -7.5,cex = 0.75)
mtext("F(9,32) = 6.01, p = < 0.0001", 2, adj=-0.1, las=1, padj = -7,cex = 0.55)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.li$xvals[c(1,3,5:12)])  
par(op)
dev.off()
