library(sciplot)
library(agricolae)


par(mfrow = c(3,1))

setwd("data/SFuels_R")

# Down Fuel Load abundances (Total Count per site)
pdf(file="Surface_Fuelbed_load.pdf", width = 5, height = 7)
par(mfrow = c(3,1),
    mar=c(4, 4,0.5, 4)+0.1)
op <- par(family = "Times")
bg.duff = with(SDLFL, bargraph.CI(x.factor=Year, group=Age, response=Duff,col= c("dark gray", "white"),
                                    lc=FALSE, err.width = .03,  ylab = "Duff (cm)",ylim=c(0,15),
                                    legend=F, x.leg=3.3,cex.leg=1.2,  cex.names=1.2, cex.lab = 1.2,axisnames=FALSE))
x <- with(tukey1$Duff,groups[order(groups$trt),])
text(x = bg.duff$xvals[c(1,3,5:12)], y=bg.duff$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(a)", 4, adj=1.5, las=1, padj = -6)
Axis(side=1, labels=F, at=bg.duff$xvals[c(1,3,5:12)])
box(lty = 'solid', col = 'black')

bg.litter = with(SDLFL, bargraph.CI(x.factor=Year, group=Age, response=Litter,col= c("dark gray", "white"),
                                     lc=FALSE, err.width = .03, ylab = "Litter (cm)",ylim=c(0,10),
                                     legend=F, x.leg=3.3, cex.leg=1.2,  cex.names=1.2, cex.lab = 1.2,axisnames=FALSE))
x <- with(tukey1$Litter,groups[order(groups$trt),])
text(x = bg.litter$xvals[c(1,3,5:12)], y=bg.litter$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(b)", 4, adj=1.5, las=1, padj = -6)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.litter$xvals[c(1,3,5:12)])

bg.fuelbed = with(SDLFL, bargraph.CI(x.factor=Year, group=Age, response=Fuelbed,col= c("dark gray", "white"),ylim=c(0,40),
                                      lc=FALSE, err.width = .03, xlab="Year of SB outbreak", ylab = "Fuelbed (cm)",
                                      legend=T, x.leg=1.3, cex.leg=1.2, cex.names=1.2, cex.lab = 1.2))
x <- with(tukey1$Fuelbed,groups[order(groups$trt),])
text(x = bg.fuelbed$xvals[c(1,3,5:12)], y=bg.fuelbed$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(c)", 4, adj=1.5, las=1, padj = -6)
box(lty = 'solid', col = 'black')
axis(side=1, labels=F, at=bg.fuelbed$xvals[c(1,3,5:12)])

par(op)
dev.off()
