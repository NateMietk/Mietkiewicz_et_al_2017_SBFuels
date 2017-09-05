attach(CBD)
library(ggplot2)
library(openxlsx)
library(lattice)

setwd("/Users/NateM/Dropbox/PhD Program/Dissertation_Research/2_Fuels/Fuel_Output_Sheets/CanopyFuels/")

CBD <- read.xlsx("CBD.xlsx", sheet = "All Sites")
CBD.P <- read.xlsx("CBD.xlsx", sheet = "All Sites Spruce")


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

pdf(file="CBD_SpruceFir.pdf", width = 8, height = 10)
par(mfrow = c(2,1),
    mar=c(4, 4,0.5, 0.5))
op <- par(family = "Times")
#Plots for CBD with Spruce and sub-alpine fir
old.cbd <- plot(range(c(CBD[,2],CBD[,3],CBD[,4],CBD[,5],CBD[,6],CBD[,7])), range(CBD[,1]), type='n', 
            ylim=c(0,40), xlim = c(0,0.3),
            xlab ="",
     ylab = "Canopy Height (m)")
lines(CBD[,2], CBD[,1], type='l', lty = 2, col = "black", lwd = 1.5)
lines(CBD[,3], CBD[,1], type='l', lty =6, col = "dark gray", lwd = 1.5)
lines(CBD[,4], CBD[,1], type='l', lty =5, col = "brown", lwd = 1.5)
lines(CBD[,5], CBD[,1], type='l', lty =3, col = "red", lwd = 1.5)
lines(CBD[,6], CBD[,1], type='l', lty =4, col = "green", lwd = 1.5)
lines(CBD[,7], CBD[,1], type='l', lty =1, col = "dark green", lwd = 1.5)
text(x=0.04 , y=40, label = "(a) Old spruce stands", cex = 1)
#abline(v=0.01,lty=2)

young.cbd <- plot(range(c(CBD[,8],CBD[,9],CBD[,10],CBD[,11])), range(CBD[,1]), type='n', 
     ylim=c(0,40), xlim = c(0,0.3),
     xlab ="Canopy bulk density (kg/m2)",
     ylab = "Canopy Height (m)")
lines(CBD[,8], CBD[,1], type='l', lty = 5, col = "brown", lwd = 1.5)
lines(CBD[,9], CBD[,1], type='l', lty =3, col = "red", lwd = 1.5)
lines(CBD[,10], CBD[,1], type='l', lty =4, col = "green", lwd = 1.5)
lines(CBD[,11], CBD[,1], type='l', lty =1, col = "dark green", lwd = 1.5)
legend(0.25, 40, c(1940, 1960, 1990, 2000, 
                   2010, "Endemic"),
       lty = c(2,6,5,3,4,1),
       lwd = c(1.5,1.5,1.5,1.5,1.5,1.5),
       col= c("black", "dark gray", "brown", "red", "green", "dark green"),
       bty = "n")
text(x=0.04 , y=40, label = "(b) Young spruce stands")
#abline(v=0.01,lty=2)

par(op)
dev.off()
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

pdf(file="CBD_Spruce.pdf", width = 8, height = 10)
par(mfrow = c(2,1))
op <- par(family = "Times")
#Plots for just spruce
old.cbd.p <- plot(range(c(CBD.P[,2],CBD.P[,3],CBD.P[,4],CBD.P[,5],CBD.P[,6],CBD.P[,7])), range(CBD.P[,1]), type='n', 
                ylim=c(0,40), xlim = c(0,0.2),
                xlab = "Canopy Bulk Density (kg/m2)",
                ylab = "Canopy Height (m)")
lines(CBD.P[,2], CBD.P[,1], type='l', lty = 2, col = "black", lwd = 1.5)
lines(CBD.P[,3], CBD.P[,1], type='l', lty =6, col = "dark gray", lwd = 1.5)
lines(CBD.P[,4], CBD.P[,1], type='l', lty =5, col = "brown", lwd = 1.5)
lines(CBD.P[,5], CBD.P[,1], type='l', lty =3, col = "red", lwd = 1.5)
lines(CBD.P[,6], CBD.P[,1], type='l', lty =4, col = "green", lwd = 1.5)
lines(CBD.P[,7], CBD.P[,1], type='l', lty =1, col = "dark green", lwd = 1.5)
text(x=0.04 , y=40, label = "(a) Old spruce stands")
#abline(v=0.01,lty=2)

young.cbd.p <- plot(range(c(CBD.P[,8],CBD.P[,9],CBD.P[,10],CBD.P[,11])), range(CBD.P[,1]), type='n', 
                  ylim=c(0,40), xlim = c(0,0.2),
                  xlab = "Canopy Bulk Density (kg/m2)",
                  ylab = "Canopy Height (m)")
lines(CBD.P[,8], CBD.P[,1], type='l', lty = 5, col = "brown", lwd = 1.5)
lines(CBD.P[,9], CBD.P[,1], type='l', lty =3, col = "red", lwd = 1.5)
lines(CBD.P[,10], CBD.P[,1], type='l', lty =4, col = "green", lwd = 1.5)
lines(CBD.P[,11], CBD.P[,1], type='l', lty =1, col = "dark green", lwd = 1.5)
legend(0.16, 40, c( 1990, 2000, 
                   2010, "Endemic"),
       lty = c(5,3,4,1),
       lwd = c(1.5,1.5,1.5,1.5),
       col= c( "brown", "red", "green", "dark green"),
       bty = "n")
text(x=0.04 , y=40, label = "(b) Young spruce stands")
#abline(v=0.01,lty=2)

par(op)
dev.off()
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


