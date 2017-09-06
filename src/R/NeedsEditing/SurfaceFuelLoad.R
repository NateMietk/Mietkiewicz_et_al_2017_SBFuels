library(sciplot)
library(agricolae)

SFL <- read.csv("data/SFL.csv")
MP <- read.csv("data/MicroPlots.csv")

par(mfrow = c(3,2))

MP$BulkDensity <- MP$BulkDensity*10
MicroP <- aggregate(cbind(BulkDensity) ~ Year +Age + Site + Plot +Veg.Cat, data=MP, FUN=mean)

SFL$SB.Age <- with(SFL, paste0(Year, Age))
MicroP$SB.Age <- with(MicroP, paste0(Year, Age))

microp.shrub <- subset(MicroP, Veg.Cat=='Shrub')
microp.grass.herb <- subset(MicroP, Veg.Cat=='Grass/Herb')


tukey1<- NULL
aov1 <- NULL
aov.sum1 <- NULL
for(i in c("n1", "n10", "n100", "dsq100", "fl1", "fl10", "fl100", "nS1000", "dsqS1000",
           "flS1000", "nR1000", "dsqR1000","flR1000")) aov1[[i]] <- aov(SFL[,i] ~ SB.Age + Site/Plot, data = SFL)
for(i in c("n1", "n10", "n100", "dsq100", "fl1", "fl10", "fl100", "nS1000", "dsqS1000",
           "flS1000", "nR1000", "dsqR1000","flR1000")) tukey1[[i]] <- HSD.test(aov1[[i]], "SB.Age")
for(i in c("n1", "n10", "n100", "dsq100", "fl1", "fl10", "fl100", "nS1000", "dsqS1000",
           "flS1000", "nR1000", "dsqR1000","flR1000")) aov.sum1[[i]] <- summary(aov1[[i]])

aov.micoplot.shrub <- aov(BulkDensity ~ SB.Age + Site/Plot, data = microp.shrub)
tukey.microplot.shrub <- HSD.test(aov.micoplot.shrub, "SB.Age")

aov.micoplot.grass.herb <- aov(BulkDensity ~ SB.Age + Site/Plot, data = microp.grass.herb)
tukey.microplot.grass.herb<- HSD.test(aov.micoplot.grass.herb, "SB.Age")


SDLFL <- read.csv("~/Dropbox/PhD Program/Dissertation_Research/2_Fuels/Fuel_Output_Sheets/SFuels_R/SDLFL.csv")
attach(SDLFL)

SDLFL$SB.Age <- with(SDLFL, paste0(Year, Age))
SDLFL$Duff <- SDLFL$Duff*100
SDLFL$Litter <- SDLFL$Litter*100
SDLFL$Fuelbed <- SDLFL$Fuelbed*100

tukey2<- NULL
aov2 <- NULL
aov.sum2 <- NULL
for(i in c("Duff", "Litter", "Fuelbed")) aov2[[i]] <- aov(SDLFL[,i] ~ SB.Age + Site/Plot, data = SDLFL)
for(i in c("Duff", "Litter", "Fuelbed")) tukey2[[i]] <- HSD.test(aov2[[i]], "SB.Age")
for(i in c("Duff", "Litter", "Fuelbed")) aov.sum2[[i]] <- summary(aov2[[i]])

# Down Fuel Load abundances (Total Count per site)
#NOTE: Because the year 2010 only had one site, I replicated the collected data to produce a Std Error of zero
# while retaining the true values.  This allowed me to plot the year/age category properly.  You will generate warnings -ignore.
pdf(file="Surface_1-100hr_Fuelload.pdf", width = 8, height = 7)
op <- par(family = "Times")
par(mfrow = c(3,2),
    oma= c(0.5,0.5,0.5,0.5),
    mar=c(4, 4.2,0.5, 1)+0.1)
bg.fl1 = with(SFL, bargraph.CI(x.factor=Year, group=Age, response=fl1,col= c("dark gray", "white"),
                              lc=FALSE, err.width = .03, ylab = "1-hr Fuel Load (Mg/ha)",ylim=c(0,4.5),
                              legend=F, x.leg=3.3, cex.leg=1.2,  cex.names=1.2, cex.lab = 1.2, axisnames=FALSE))
x <- with(tukey1$fl1,groups[order(groups$trt),])
text(x = bg.fl1$xvals[c(1,3,5:12)], y=bg.fl1$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(a) F(9, 32) = 2.83, p = 0.0131", 1,adj=0.05, las=1, padj = -19,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.fl1$xvals[c(1,3,5:12)])

bg.flS1000 = with(SFL, bargraph.CI(x.factor=Year, group=Age, response=flS1000,col= c("dark gray", "white"),
                                   lc=FALSE, err.width = .03,  ylab = "Sound 1000-hr Fuels (Mg/ha)",ylim=c(0,200),
                                   legend=T, x.leg=14, cex.leg=1.2,  cex.names=1.2, cex.lab = 1.2, axisnames=F))
x <- with(tukey1$flS1000,groups[order(groups$trt),])
text(x = bg.flS1000$xvals[c(1,3,5:12)], y=bg.flS1000$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(b) F(9,32) = 2.74, p = 0.0156", 1,adj=0.05, las=1, padj = -19,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.fl1$xvals[c(1,3,5:12)])

bg.fl10 = with(SFL, bargraph.CI(x.factor=Year, group=Age, response=fl10,col= c("dark gray", "white"),
                              lc=FALSE, err.width = .03,   ylab = "10-hr Fuels (Mg/ha)",ylim=c(0,20),
                              legend=F, x.leg=3.3, cex.leg=1.2,  cex.names=1.2, cex.lab = 1.2, axisnames=FALSE))
#x <- with(tukey1$fl10,groups[order(groups$trt),])
#text(x = bg.fl10$xvals[c(1,3,5:12)], y=bg.fl10$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(c) F(9,32) = 1.68, p = 0.131", 1,adj=0.05, las=1, padj = -19,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.fl1$xvals[c(1,3,5:12)])


bg.flR1000 = with(SFL, bargraph.CI(x.factor=Year, group=Age, response=flR1000,col= c("dark gray", "white"),
                                   lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Rotten 1000-hr Fuels (Mg/ha)",ylim=c(0,225),
                                   legend=F, x.leg=3.3, cex.leg=1.2,  cex.names=1.2, cex.lab = 1.2))
x <- with(tukey1$flR1000,groups[order(groups$trt),])
text(x = bg.flR1000$xvals[c(1,3,5:12)], y=bg.flR1000$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(d) F(9,32) = 4.71, p = 0.0004", 1,adj=0.2, las=1, padj = -19,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.fl1$xvals[c(1,3,5:12)])

bg.fl100 = with(SFL, bargraph.CI(x.factor=Year, group=Age, response=fl100,col= c("dark gray", "white"),
                              lc=FALSE, err.width = .03,  xlab="Year of SB Outbreak",ylab = "100-hr Fuels (Mg/ha)",ylim=c(0,50),
                              legend=F, x.leg=14, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2))
#x <- with(tukey1$fl100,groups[order(groups$trt),])
#text(x = bg.fl100$xvals[c(1,3,5:12)], y=bg.fl100$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(e) F(9,32) = 1.57, p = 0.1633", 1,adj=0.05, las=1, padj = -19,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.fl1$xvals[c(1,3,5:12)])

#plot(0,type='n',axes=FALSE,ann=FALSE)
#legend("top", c("Old","Young"), xpd = TRUE, horiz = TRUE, bty = 'n', inset = c(0,0),
#       col=c('black','black'), fill = c("dark gray", "white"), cex = 1.2)
par(op)
dev.off()

# Microplot fuel abundances (Total Count per site)
pdf(file="Microplot_Fuelload.pdf", width = 8, height = 7)
par(mfrow = c(3,2),
    oma= c(0.5,0.5,0.5,0.5),
    mar=c(4, 4.2,0.5, 1)+0.1)
op <- par(family = "Times")
bg.duff = with(SDLFL, bargraph.CI(x.factor=Year, group=Age, response=Duff,col= c("dark gray", "white"),
                                  lc=FALSE, err.width = .03,  ylab = "Duff (cm)",ylim=c(0,15),
                                  legend=F, x.leg=3.3,cex.leg=1.2,  cex.names=1.2, cex.lab = 1.2,axisnames=FALSE))
x <- with(tukey2$Duff,groups[order(groups$trt),])
text(x = bg.duff$xvals[c(1,3,5:12)], y=bg.duff$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(a) F(9,32) = 4.03, p = 0.0016", 1,adj=0.05, las=1, padj = -19,cex = 0.75)
Axis(side=1, labels=F, at=bg.duff$xvals[c(1,3,5:12)])
box(lty = 'solid', col = 'black')

bg.grass.herb = with(microp.grass.herb, bargraph.CI(x.factor=Year, group=Age, response=BulkDensity,col= c("dark gray", "white"),ylim=c(0,1),
                                                    lc=FALSE, err.width = .03, ylab = "Grass/Herb Biomass (Mg/ha)",
                                                    legend=T, x.leg=14, cex.leg=1.2,  cex.names=1.2, cex.lab = 1.2))
x <- with(tukey.microplot.grass.herb,groups[order(groups$trt),])
text(x = bg.grass.herb$xvals[c(1,3,5:12)], y=bg.grass.herb$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(b) F(9,32) = 4.9, p = < 0.0001", 1,adj=0.05, las=1, padj = -19,cex = 0.75)
Axis(side=1, labels=F, at=bg.duff$xvals[c(1,3,5:12)])
box(lty = 'solid', col = 'black')

bg.litter = with(SDLFL, bargraph.CI(x.factor=Year, group=Age, response=Litter,col= c("dark gray", "white"),
                                    lc=FALSE, err.width = .03, ylab = "Litter (cm)",ylim=c(0,10),
                                    legend=F, x.leg=3.3, cex.leg=1.2,  cex.names=1.2, cex.lab = 1.2,axisnames=FALSE))
x <- with(tukey2$Litter,groups[order(groups$trt),])
text(x = bg.litter$xvals[c(1,3,5:12)], y=bg.litter$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(c) F(9,32) = 3.95, p = 0.0018",1,adj=0.05, las=1, padj = -19,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.litter$xvals[c(1,3,5:12)])

bg.shrub = with(microp.shrub, bargraph.CI(x.factor=Year, group=Age, response=BulkDensity,col= c("dark gray", "white"),ylim=c(0,2),
                                          lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Shrub Biomass (Mg/ha)",
                                          legend=F, x.leg=3.3, cex.leg=1.2,  cex.names=1.2, cex.lab = 1.2))
x <- with(tukey.microplot.shrub,groups[order(groups$trt),])
text(x = bg.shrub$xvals[c(1,3,5:12)], y=bg.shrub$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(d) F(9,32) = 2.95, p = 0.0119",1,adj=0.05, las=1, padj = -19,cex = 0.75)
Axis(side=1, labels=F, at=bg.duff$xvals[c(1,3,5:12)])
box(lty = 'solid', col = 'black')

bg.fuelbed = with(SDLFL, bargraph.CI(x.factor=Year, group=Age, response=Fuelbed,col= c("dark gray", "white"),ylim=c(0,40),
                                     lc=FALSE, err.width = .03, xlab="Year of SB outbreak", ylab = "Fuelbed (cm)",
                                     legend=F, x.leg=1.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2))
x <- with(tukey2$Fuelbed,groups[order(groups$trt),])
text(x = bg.fuelbed$xvals[c(1,3,5:12)], y=bg.fuelbed$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(e) F(9,32) = 7.29, p = < 0.0001", 1,adj=0.05, las=1, padj = -19,cex = 0.75)
box(lty = 'solid', col = 'black')
axis(side=1, labels=F, at=bg.fuelbed$xvals[c(1,3,5:12)])

#plot(0,type='n',axes=FALSE,ann=FALSE)
#legend("top", c("Old","Young"), xpd = TRUE, horiz = TRUE, bty = 'n', inset = c(0,0),
#       col=c('black','black'), fill = c("dark gray", "white"), cex = 1.2)

par(op)
dev.off()
detach(SDLFL)