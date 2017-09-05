library(sciplot); library(agricolae); library(ggplot2); library(lattice); library(plyr)

Saps <- read.csv("~/Dropbox/PhD Program/Dissertation_Research/2_Fuels/Fuel_Output_Sheets/SFuels_R/Sapling.csv")
attach(Saps)

sap.sum <- aggregate(cbind(foliage, f1hr, f10hr, f100hr, f1000hr) ~ site + year + age + plot, data=Saps, FUN=sum)

##Go through each row and determine if a value is zero
rowsub <- Saps[apply(Saps[12],1,function(z) !any(z==0)),] 

sap.hght <- (aggregate(height~ site+year+age+plot, data= rowsub, FUN=mean))
sap.sum$SB.Age <- with(sap.sum, paste0(year, age))
sap.hght$SB.Age <- with(sap.hght, paste0(year, age))

sap.freq <- count(Saps, c('site', 'year','age','plot'))
sap.freq$freq.perha <-  (sap.freq$freq/5)*(10000)
sap.freq$SB.Age <- with(sap.freq, paste0(year, age))

aov.freq <- aov(freq.perha ~ SB.Age + site/plot, data = sap.freq)
tukey.freq <- HSD.test(aov.freq, "SB.Age",alpha=0.1)


tukey1<- NULL
aov1 <- NULL
aov.sum1 <- NULL

for(i in c("foliage", "f1hr", "f10hr", "f100hr", "f1000hr")) 
  aov1[[i]] <- aov(sap.sum[,i] ~ SB.Age + site/plot, data = sap.sum)
for(i in c("foliage", "f1hr", "f10hr", "f100hr", "f1000hr"))
  tukey1[[i]] <- HSD.test(aov1[[i]], "SB.Age",alpha=0.1)
for(i in c("foliage", "f1hr", "f10hr", "f100hr", "f1000hr")) 
  aov.sum1[[i]] <- summary(aov1[[i]])


aov2 <- aov(height ~ SB.Age + site/plot, data = sap.hght)
tukey2<- HSD.test(aov2, "SB.Age",alpha=0.1)


# Plotting theme (ggplot2) ####
theme_cust<-theme(
  plot.title = element_text(hjust = 0.5,lineheight=.8,size=11,face="bold"),
  legend.position="right",
  legend.text = element_text(colour="black", size = 11),
  legend.title = element_text(size=10,face="bold"),
  legend.title.align=0.5,
  plot.margin = unit(c(1,0.75,1,0),"mm"), #top, right, bottom, left
  legend.background = element_rect(colour = "black"),
  axis.text.x= element_text(family= "serif"),
  axis.text.y= element_text(family= "serif"),
  axis.text=element_text(size=9),
  axis.title=element_text(size=10,face="bold"),
  axis.text         = element_text(size = 9),
  axis.ticks        = element_line(colour = "black"),
  legend.key        = element_rect(colour = "#FFFFFF"),
  panel.background  = element_rect(fill = "white", colour = NA),
  panel.border      = element_rect(fill = NA, colour = "black")
)

# Plots -------------------------------------------------------------------
ggplot_fun <- function(data, x, y, s, ymn, ymx, xlb, ylb, tit){
  p <- ggplot(data, aes_string(x=x, y=y, fill = s))
  p <- p + geom_boxplot(show.legend = T,position=position_dodge(.75))+
      scale_fill_manual(values=c("dark gray", "white"))
  p <- p + theme_cust
  p <- p + labs(y = ifelse(ylb == 0, "", "Sapling height (cm)"),
                x= (ifelse(xlb == 0, "", "Year of spruce beetle outbreak")),
                title = tit ) 
}
p1 <- ggplot_fun(sap.hght, "year", "height", "age", a, b, "1" ,"1", "1985-2015")

pdf(file="Sapling_FuelLoad.pdf", width = 10, height = 7)
par(mfrow = c(2,3),#mar = c(3, 3.5, 1, 0.5),mgp = c(2, 1, 0),oma = c(1, 0, 1, 0))
    mgp = c(2, 1, 0),
    oma = c(1, 1, 1, 2),
    mar=c(4, 4,0.5, 0))
op <- par(family = "Times")
bg.SapFol = with(sap.sum, bargraph.CI(x.factor=year, group=age, response=foliage,family = "Times",
             lc=FALSE, err.width = .03,  ylab = "Foliage fuels (MG/ha)",ylim=c(0,7),
             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$foliage,groups[order(groups$trt),])
text(x = bg.SapFol$xvals[c(1,3,5:15)], y=bg.SapFol$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(a) F(9,32) = 6.11, p = < 0.0001", 1, adj=0.1, las=1, padj = -32,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.SapFol$xvals[c(1,3,5:12)])

bg.Sapf1hr = with(sap.sum, bargraph.CI(x.factor=year, group=age, response=f1hr,family = "Times",
             lc=FALSE, err.width = .03,  ylab = "1 hr fuels (MG/ha)",ylim=c(0,3),
             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$f1hr,groups[order(groups$trt),])
text(x = bg.Sapf1hr$xvals[c(1,3,5:15)], y=bg.Sapf1hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(b) F(9,32) = 6.44, p = < 0.0001", 1, adj=0.1, las=1, padj = -32,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.Sapf1hr$xvals[c(1,3,5:12)])

bg.Sapf10hr = with(sap.sum, bargraph.CI(x.factor=year, group=age, response=f10hr,family = "Times",
             lc=FALSE, err.width = .03,  ylab = "10 hr fuels (MG/ha)",ylim=c(0,3),
             legend=T, x.leg=13, cex.leg=1.2, cex.names=1, cex.lab = 1.2,axisnames=F, col= c("dark gray", "white")))
x <- with(tukey1$f10hr,groups[order(groups$trt),])
text(x = bg.Sapf10hr$xvals[c(1,3,5:15)], y=bg.Sapf10hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(c) F(9,32) = 5.91, p = < 0.0001", 1, adj=0.1, las=1, padj = -32,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.Sapf10hr$xvals[c(1,3,5:12)])

bg.Sapf100hr = with(sap.sum, bargraph.CI(x.factor=year, group=age, response=f100hr,family = "Times",
             lc=FALSE, err.width = .03,  ylab = "100 hr fuels (MG/ha)",ylim=c(0,4),xlab= "\nSpruce beetle outbreak",
             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1, cex.lab = 1.2,axisnames=T, col= c("dark gray", "white")))
x <- with(tukey1$f10hr,groups[order(groups$trt),])
text(x = bg.Sapf100hr$xvals[c(1,3,5:15)], y=bg.Sapf100hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(d) F(9,32) = 5.48, p = < 0.0001", 1, adj=0.1, las=1, padj = -32,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.Sapf100hr$xvals[c(1,3,5:12)])

bg.Sapf1000hr = with(sap.sum, bargraph.CI(x.factor=year, group=age, response=f1000hr,family = "Times",
             lc=FALSE, err.width = .03,  ylab = "1000 hr fuels (MG/ha)",ylim=c(0,0.5),xlab= "\nSpruce beetle outbreak",
             legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1, cex.lab = 1.2,axisnames=T, col= c("dark gray", "white")))
x <- with(tukey1$f1000hr,groups[order(groups$trt),])
text(x = bg.Sapf1000hr$xvals[c(1,3,5:15)], y=bg.Sapf1000hr$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(e) F(9,32) = 2.37, p = 0.0081", 1, adj=0.1, las=1, padj = -32,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.Sapf1000hr$xvals[c(1,3,5:12)])

bg.freq = with(sap.freq, bargraph.CI(x.factor=year, group=age, response=freq.perha,family = "Times",
                                          lc=FALSE, err.width = .03,  ylab = "Sapling density (saplings/ha)",ylim=c(0,50000),xlab= "\nSpruce beetle outbreak",
                                          legend=F, x.leg=3.3, cex.leg=1.2, cex.names=1, cex.lab = 1.2,axisnames=T, col= c("dark gray", "white")))
x <- with(tukey.freq,groups[order(groups$trt),])
text(x = bg.freq$xvals[c(1,3,5:15)], y=bg.freq$CI[c(2,6,10,12,14,16,18,20,22,24)], labels = as.character(x$M),  cex = 1, pos = 3, xpd = T, col="black",family = "Times")
mtext("(f) F(9,32) = 64.95, p < 0.0001", 1, adj=0.1, las=1, padj = -32,cex = 0.75)
box(lty = 'solid', col = 'black')
Axis(side=1, labels=F, at=bg.freq$xvals[c(1,3,5:12)])


# Calculating the range so that the panels are comparable
#bp = boxplot(height~interaction(age,year), data=sap.hght,outline=F,xaxt='n',
#             at= c(1,2,3,4,5,6,7,8,9,10,11,12),ylab = "Sapling height (cm)", xlab= "\nSpruce beetle outbreak",cex.lab = 1.2,
#             names=NA, ylim = c(0,350), col = c("dark gray", "dark gray", "dark gray", "white", "dark gray", "white", "dark gray", "white", "dark gray", "white", "dark gray", "white"))
#labels = c("1940", "1960", "1990", "2000", "2010", "Endemic")
#axis(1,at=c(1.5,3.5,5.5,7.5,9.5, 11.5),labels=labels, tck=-0.02) # boxplot of factor level distributions
##legend(x=9, y=350, legend=c("Old","Young"), 
##       fill=c("dark gray", "white"), bty = "n")
#x <- with(tukey2,groups[order(groups$trt),])
#mtext("(f) F(9,32) = 7.54, p = < 0.0001", 1, adj=0.1, las=1, padj = -32,cex = 0.75)
#text(x=c(1,2,3,4,5,6,7,8,9,10,11,12), y=10+bp$stats[5,1:12], labels=x$M)
dev.off()

