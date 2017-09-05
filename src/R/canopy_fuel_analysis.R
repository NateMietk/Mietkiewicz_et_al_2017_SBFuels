library(sciplot)
library(data.table)
library(agricolae)
library(ggplot2)
library(tidyverse)

cfl <- fread("data/CFL.csv") %>%
  mutate(AFCL = AFCL*10,
         LCW = LCW*10,
         DCW = DCW*10,
         LiDe = ifelse(Status == "Li", "Live", "Dead"))
cc <- fread("data/CC.csv") %>%
  group_by(Year, Age) %>%
  summarise(CanopyOpen = min(CanopyOpen)) %>%
  mutate(sb_age = paste0(Year, Age))

source('src/R/NestAnovaPerm.txt')

# Aggregate canopy base height per plot/site
cfl_cbh <- cfl %>%
  group_by(Site, Year, Age, Plot) %>%
  summarise(CBH = mean(CBH))

# Basal area sums and means per plot/site
ba_cfl <- cfl %>%
  filter(S.F == "S") %>%
  group_by(Site, Year, Age, Plot) %>%
  select(CBD, AFCL, LCW, DCW, BA_Li, BA_Gr, BA_Ye, BA_Nd, BA_Tw, BA_Br, BA_Sn, BA_De) %>%
  summarise_all(c("sum", "mean"))

# Canopy fuel sum per plot/site for standing trees only
pfuel <- cfl %>%
  filter(S.F == "S") %>%
  mutate(AFCL = AFCL*10,
         LCW = LCW*10,
         DCW = DCW*10,
         LiDe = ifelse(Status == "Li", "Live", "Dead")) %>%
  filter(S.F == "S" & Plot != "0") %>%
  group_by(Site, Year, Age, Plot) %>%
  select(LiFol, DeFol, Li.1hr, De.1hr, Li.10hr, De.10hr, Li.100hr, De.100hr) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  mutate(sb_age = paste0(Year, Age))

# Canopy fuel sum per site for standing trees only
sfuel <- pfuel %>%
  group_by(Site, Year, Age) %>%
  select(LiFol, DeFol, Li.1hr, De.1hr, Li.10hr, De.10hr, Li.100hr, De.100hr) %>%
  summarise_all(mean) %>%
  mutate(sb_age = paste0(Year, Age))

# Available crown fuel load per plot/site for standing trees only
acfl <- cfl %>%
  filter(S.F == "S") %>%
  group_by(Site, Year, Age, Plot, LiDe) %>%
  summarise(AFCL = sum(AFCL)) %>%
  mutate(sb_age = paste0(Year, Age))

#--------------------------------------------------
#Basal area class for Gr, Ye, and Nd to represent total area of trees with > 50% needle retention
cfl_basum <- all_cfl %>%
  select(Site, Year, Age, Plot, BA_Li_sum, BA_Gr_sum, BA_Ye_sum, BA_Nd_sum, BA_Tw_sum, BA_Br_sum, BA_Sn_sum, BA_De_sum) %>%
  mutate(ba_g50pct_nd = BA_Gr_sum + BA_Ye_sum + BA_Nd_sum,
         ba_l50pct_tw = BA_Br_sum + BA_Sn_sum,
         sb_age = paste0(Year, Age))
CFL_bamean <- all_cfl %>%
  select(Site, Year, Age, Plot, BA_Li_mean, BA_Gr_mean, BA_Ye_mean, BA_Nd_mean, BA_Tw_mean, BA_Br_mean, BA_Sn_mean, BA_De_mean) %>%
  mutate(ba_g50pct_nd = BA_Gr_mean + BA_Ye_mean + BA_Nd_mean,
         ba_l50pct_tw = BA_Br_mean + BA_Sn_mean,
         sb_age = paste0(Year, Age))


#Create ANOVA models and post-hoc Tukey HSD.
#Response variable will be looped, but the explanatory variable is a merged Year and Age varaible (i.e., Old1960, Young1990, etc.)  

aov.models = pfuel %>%
  select(LiFol, DeFol, Li.1hr, De.1hr, Li.10hr, De.10hr, Li.100hr, De.100hr, Site, Plot, sb_age) %>%
  map(~ aov(.x ~ sb_age + Site/Plot, data = pfuel))

aov.models = mtcars[ , -grep("cyl", names(mtcars))] %>%
  map(~ aov(.x ~ mtcars$cyl))

for(i in c("LiFol","DeFol","Li.1hr","De.1hr","Li.10hr","De.10hr","Li.100hr","De.100hr")) 
  aov1[[i]] <- aov(pfuel[,i] ~ sb_age + Site/Plot, data = pfuel)
for(i in c("LiFol","DeFol","Li.1hr","De.1hr","Li.10hr","De.10hr","Li.100hr","De.100hr"))
  tukey1[[i]] <- HSD.test(aov1[[i]], "sb_age",alpha=0.1)
for(i in c("LiFol","DeFol","Li.1hr","De.1hr","Li.10hr","De.10hr","Li.100hr","De.100hr"))
  TukeyHSD1[[i]] <- TukeyHSD(aov1[[i]], "sb_age", ordered=T)
for(i in c("LiFol","DeFol","Li.1hr","De.1hr","Li.10hr","De.10hr","Li.100hr","De.100hr")) 
  aov.sum1[[i]] <- summary(aov1[[i]])
for(i in c("LiFol","DeFol","Li.1hr","De.1hr","Li.10hr","De.10hr","Li.100hr","De.100hr")) 
  SW1[[i]]<- shapiro.test(residuals(aov1[[i]]))


tukey2<- NULL
TukeyHSD2 <- NULL
aov2 <- NULL
aov.sum2 <- NULL
SW2 <- NULL
for(i in c("CBD", "AFCL","LCW","DCW","BA_Li","BA_Gr","BA_Ye","BA_Nd","BA_Tw",
           "BA_Br","BA_Sn","BA_De","BA_g50pct.nd", "BA_l50pct.tw")) aov2[[i]] <- aov(CFL.s2[,i] ~ SB.Age + Site/Plot, data = CFL.s2)
for(i in c("CBD", "AFCL","LCW","DCW","BA_Li","BA_Gr","BA_Ye","BA_Nd","BA_Tw",
           "BA_Br","BA_Sn","BA_De","BA_g50pct.nd", "BA_l50pct.tw")) tukey2[[i]] <- HSD.test(aov2[[i]], "SB.Age")
for(i in c("CBD", "AFCL","LCW","DCW","BA_Li","BA_Gr","BA_Ye","BA_Nd","BA_Tw",
           "BA_Br","BA_Sn","BA_De","BA_g50pct.nd", "BA_l50pct.tw")) TukeyHSD2[[i]] <- TukeyHSD(aov2[[i]], "SB.Age", ordered=T)
for(i in c("CBD", "AFCL","LCW","DCW","BA_Li","BA_Gr","BA_Ye","BA_Nd","BA_Tw",
           "BA_Br","BA_Sn","BA_De","BA_g50pct.nd", "BA_l50pct.tw")) aov.sum2[[i]] <- summary(aov2[[i]])
for(i in c("CBD", "AFCL","LCW","DCW","BA_Li","BA_Gr","BA_Ye","BA_Nd","BA_Tw",
           "BA_Br","BA_Sn","BA_De","BA_g50pct.nd", "BA_l50pct.tw")) SW2[[i]]<- shapiro.test(residuals(aov2[[i]]))


co.var <- function(x) ( 100*sd(x)/mean(x) )
p<-apply(CFL.s2[,c("CBD", "AFCL","LCW","DCW","BA_Li","BA_Gr","BA_Ye","BA_Nd",
              "BA_Tw","BA_Br","BA_Sn","BA_De","BA_g50pct.nd", "BA_l50pct.tw")],1,co.var)
CV1 <-NULL
for(i in c( "EndemicOld", "EndemicYoung","1940Old", "1960Old", "1990Old", "2000Old", "2010Old", "1990Young", "2000Young")){
  q<- subset(CFL.s2, SB.Age== i)
  t <- aov(AFCL~Site, data=q)
  CV2[[i]] <- cv.model(t)
  SE2[[i]] <- sd(q$AFCL)/sqrt(length(q$AFCL))
}

CV2 <-NULL
SE2 <- NULL
for(i in c( "EndemicOld", "EndemicYoung","1940Old", "1960Old", "1990Old", "2000Old", "2010Old", "1990Young", "2000Young")){
  q<- subset(CFL.s2, SB.Age== i)
  t <- aov(CBD~Site, data=q)
  CV2[[i]] <- cv.model(t)
  SE2[[i]] <- sd(q$CBD)/sqrt(length(q$CBD))
}

plot(x, CV2,
     ylim=range(c(CV2-SE2, CV2+SE2)),
     pch=19, xlab="Measurements", ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, CV2-SE2, x, CV2+SE2, length=0.05, angle=90, code=3)

CV2010 <- NULL
for(i in c("CBD", "AFCL","LCW","DCW","BA_Li","BA_Gr","BA_Ye","BA_Nd","BA_Tw",
           "BA_Br","BA_Sn","BA_De","BA_g50pct.nd","BA_l50pct.tw")){
  q1 <- subset(CFL.s2, SB.Age== "2010Young")
  CV <- function(mean, sd){
    (sd/mean)*100 }
  m <- mean(q1[,i])
  sd <- sd(q1[,i])
  CV2010[[i]] <- CV(mean = m, sd=sd)
}


aov.CBH <- aov(CBH ~SB.Age + Site/Plot, data = CFL.cpy)
CBH.aoc.sum <- summary(aov.CBH)
tukey.CBH <- HSD.test(aov.CBH, "SB.Age")
TukeyHSD.cbh <- TukeyHSD(aov.CBH, "SB.Age")

cc.aov <- aov(CanopyOpen~SB.Age, data=CC)
cc.hsd <- HSD.test(cc.aov, "SB.Age")
cc.aicc <- AICc(cc.aov)

#--------------------------------------------------
#Create bargraphs with standard error bars for all live and dead crown fuelloads
pdf(file="Live_Dead_CFL.pdf", width = 10, height = 9)
par(mfrow = c(4,3),#mar = c(3, 3.5, 1, 0.5),mgp = c(2, 1, 0),oma = c(1, 0, 1, 0))
    mgp = c(2, 1, 0),
    oma = c(1, 1, 1, 2),
    mar=c(4, 4,0.5, 0))
op <- par(family = "Times")
bg.LiFol = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=LiFol,family = "Times",
             lc=FALSE, err.width = .03,  ylab = "Live Foliage (MG/ha)",ylim=c(0,40),
             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$LiFol,groups[order(groups$trt),])
text(x = bg.LiFol$xvals[c(1,3,5:15)], y=bg.LiFol$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(a) F(9,33) = 30.51, p = < 0.0001", 1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.DeFol = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=DeFol,family = "Times",
             lc=FALSE, err.width = .03,  ylab = "Dead Foliage (MG/ha)",ylim=c(0,2),
             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
#x <- with(tukey1$DeFol,groups[order(groups$trt),])
#text(x = bg.DeFol$xvals[c(1,3,5:12)], y=bg.DeFol$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(b) F(9,33) = 1.97, p = 0.0763", 1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.CBH = with(CFL.cpy, bargraph.CI(x.factor=Year, group=Age, response=CBH,family = "Times",
                                    lc=FALSE, err.width = .03,  ylab = "CBH (m)",ylim=c(0,2),
                                    legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey.CBH,groups[order(groups$trt),])
text(x = bg.CBH$xvals[c(1,3,5:12)], y=bg.CBH$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(c) F(9,33) = 4.51, p = < 0.0001", 1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.Li.1hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=Li.1hr,family = "Times",
             lc=FALSE, err.width = .03,  ylab = "Live 1hr (MG/ha)",ylim=c(0,25),
             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$Li.1hr,groups[order(groups$trt),])
text(x = bg.Li.1hr$xvals[c(1,3,5:12)], y=bg.Li.1hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(d) F(9,33) = 33.08, p = < 0.0001",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.De.1hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=De.1hr,family = "Times",
             lc=FALSE, err.width = .03, ylab = "Dead 1hr (MG/ha)",ylim=c(0,2),
             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$De.1hr,groups[order(groups$trt),])
text(x = bg.De.1hr$xvals[c(1,3,5:12)], y=bg.De.1hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(e) F(9,33) = 2.94, p = 0.0113", 1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.CBD = with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=CBD,family = "Times",
                                   lc=FALSE, err.width = .03, ylab = "CBD (kg/m2)",ylim=c(0,0.6),
                                   legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey2$CBD,groups[order(groups$trt),])
text(x = bg.CBD$xvals[c(1,3,5:12)], y=bg.CBD$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(f) F(9,33) = 10.96, p = < 0.0001",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.Li.10hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=Li.10hr,family = "Times",
             lc=FALSE, err.width = .03,  ylab = "Live 10hr (MG/ha)",ylim=c(0,35),
             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$Li.10hr,groups[order(groups$trt),])
text(x = bg.Li.10hr$xvals[c(1,3,5:12)], y=bg.Li.10hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(g) F(9,33) = 40.17, p = < 0.0001",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.De.10hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=De.10hr,family = "Times",
             lc=FALSE, err.width = .03, ylab = "Dead 10hr (MG/ha)",ylim=c(0,5),
             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$De.10hr,groups[order(groups$trt),])
text(x = bg.De.10hr$xvals[c(1,3,5:12)], y=bg.De.10hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(h) F(9,33) = 3.53, p = 0.0037",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.AFCL = with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=AFCL,family = "Times",
                                    lc=FALSE, err.width = .03,  ylab = "ACFL (MG/ha)",ylim=c(0,70),
                                    legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey2$AFCL,groups[order(groups$trt),])
text(x = bg.AFCL$xvals[c(1,3,5:12)], y=bg.AFCL$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(i) F(9,33) = 31.10, p = < 0.0001",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.Li.100hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=Li.100hr,family = "Times",
             lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Live 100hr (MG/ha)",ylim=c(0,40),
             legend=T, x.leg=1.3,y.leg = 2.2, ncol =2,cex.leg=1.2, cex.names=1, cex.lab = 1.2,axisnames=T, col= c("dark gray", "white")))
x <- with(tukey1$Li.100hr,groups[order(groups$trt),])
text(x = bg.Li.100hr$xvals[c(1,3,5:12)], y=bg.Li.100hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(j) F(9,33) = 46.53, p = < 0.0001",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.De.100hr = with(CFL.plot, bargraph.CI(x.factor=Year, group=Age, response=De.100hr,family = "Times",
             lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Dead 100hr (MG/ha)",ylim=c(0,4),
             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1, cex.lab = 1.2,axisnames=T, col= c("dark gray", "white")))
#x <- with(tukey1$De.100hr,groups[order(groups$trt),])
#text(x = bg.De.100hr$xvals[c(1,3,5:12)], y=bg.De.100hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(k) F(9,33) = 2.06, p = 0.0634", 1, adj=0.1, las=1, padj = -18,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.LiFol$xvals[c(1,3,5:12)])

bg.CC = with(CC, bargraph.CI(x.factor=Year, group=Age, response=CanopyOpen,family = "Times",
            lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Canopy Openness (%)",ylim = c(0,25),
            legend=T, x.leg=14.5, cex.leg=1, cex.names=1, cex.lab = 1.2,axisnames=T, col= c("dark gray", "white")))
x <- with(cc.hsd,groups[order(groups$trt),])
text(x = bg.CC$xvals[c(1,3,5:12)], y=bg.CC$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(l) F(9,33) = 2.53, p = 0.045",  1, adj=0.1, las=1, padj = -18,cex = 0.75)
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

pdf(file="BasalArea.pdf", width = 5, height = 7)
par(mfrow = c(4,1),
    mgp = c(2, 1, 0),
    oma = c(1,1,1,1)+0.2,
    mar = c(3,4,1,1))
op <- par(family = "Times")
  #Plot of live basal area
  bg.li <- with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=BA_Li,axisnames=F,
                                                 lc=FALSE, err.width = .03, ylab = "Basal Area (m2/ha)",ylim = c(0,250),
                                                 x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1,family = "Times", col= c("dark gray", "white")))
  x <- with(tukey2$BA_Li,groups[order(groups$trt),])
  text(x = bg.li$xvals[c(1,3,5:12)], y=bg.li$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
  mtext("(a) Live", 2, adj=0, las=1, padj = -7.5,cex = 0.75)
  mtext("F(9,33) = 7.61, p = < 0.0001", 2, adj=-0.1, las=1, padj = -7,cex = 0.55)
  box(lty = 'solid', col = 'black')
  Axis(side=1, labels=F, at=bg.li$xvals[c(1,3,5:12)])
  #Plot combining the basal area class for Gr, Ye, and Nd to represent total area of trees with greater than 50% needle retention
  bg.g50pct.nd <- with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=BA_g50pct.nd,ylim = c(0,60),axisnames=F,
                                            lc=FALSE, err.width = .03,  ylab = "Basal Area (m2/ha)",
                                            x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1,family = "Times", col= c("dark gray", "white")))
  x <- with(tukey2$BA_g50pct.nd,groups[order(groups$trt),])
  text(x = bg.g50pct.nd$xvals[c(1,3,5:12)], y=bg.g50pct.nd$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
  mtext("(b) > 50% needle retention",  2, adj=0, las=1, padj = -7.5,cex = 0.75)
  mtext("F(9,33) = 7.07, p = < 0.0001", 2, adj=-0.1, las=1, padj = -7,cex = 0.55)
  box(lty = 'solid', col = 'black')
  Axis(side=1, labels=F, at=bg.li$xvals[c(1,3,5:12)])
  
  #Plot of twig basal area- < 50% red needles retained in the canopy; >50% 1-hr twigs remain but are devoid of needles 
  bg.tw <- with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=BA_Tw, ylim = c(0,150),axisnames=F,
                                     lc=FALSE, err.width = .03,  ylab = "Basal Area (m2/ha)",
                                     x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1,family = "Times", col= c("dark gray", "white")))
  x <- with(tukey2$BA_Tw,groups[order(groups$trt),])
  text(x = bg.tw$xvals[c(1,3,5:12)], y=bg.tw$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
  mtext("(c) < 50% needles, > 50% 1-hr",  2, adj=0, las=1, padj = -7.5,cex = 0.75)
  mtext("F(9,33) = 3.92, p = 0.0018", 2, adj=-0.1, las=1, padj = -7,cex = 0.55)
  box(lty = 'solid', col = 'black')
  Axis(side=1, labels=F, at=bg.li$xvals[c(1,3,5:12)])
  
  #Plot of branch basal area- < 50% 1-hr fuels
  bg.br <- with(CFL.sum, bargraph.CI(x.factor=Year, group=Age, response=BA_l50pct.tw, ylim = c(0,125),axisnames=T,
                                     lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Basal Area (m2/ha)",
                                     legend=T, x.leg=15.3, cex.leg=1, cex.names=1, cex.lab = 1,family = "Times", col= c("dark gray", "white")))
  x <- with(tukey2$BA_l50pct.tw,groups[order(groups$trt),])
  text(x = bg.br$xvals[c(1,3,5:12)], y=bg.br$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
  mtext("(d) < 50% 1-hr ", 2, adj=0, las=1, padj = -7.5,cex = 0.75)
  mtext("F(9,33) = 9.57, p = < 0.0001", 2, adj=-0.1, las=1, padj = -7,cex = 0.55)
  box(lty = 'solid', col = 'black')
  Axis(side=1, labels=F, at=bg.li$xvals[c(1,3,5:12)])  
par(op)
dev.off()
