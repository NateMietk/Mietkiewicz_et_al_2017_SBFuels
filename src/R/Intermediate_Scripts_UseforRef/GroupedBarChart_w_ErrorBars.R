libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales', 'psych')
#lapply(libs, require, character.only = T)
attach(SDLFL)
par(mfrow = c(1,1))

#------------------
#Define Varaibles

var1 <- Duff
var2 <- n10
var3 <- n100


d.mean <- tapply( X = var1, INDEX = list(Age, Year), FUN=mean)
d.matrix <- matrix(c(d.mean[1,],d.mean[2,]),nrow= 2, ncol = 6,byrow=TRUE, dimnames = list(c("Old", "Young"),c("1940", "1960", "1990", "2000", "2010", "Endemic")))
d.matrix
#d.matrix[is.na(d.matrix)] <- 0

d.std <- tapply( X = var1, INDEX = list(Age,Year), FUN=sd)
se.d <- (d.std/sqrt(length(var1)))
se.matrix <- matrix(c(se.d[1,],se.d[2,]),nrow= 2, ncol = 6,byrow=TRUE, dimnames = list(c("Old", "Young"),c("1940", "1960", "1990", "2000", "2010", "Endemic")))
#se.matrix[is.na(se.matrix)] <- 0

barx <- barplot(d.matrix,
                beside=TRUE,
                col=c("gray","black"), 
                border = "black",
                ylim=c(0,0.1), 
                axis.lty=1, 
                legend.text = rownames(d.mean), 
                args.legend = list(x = "topleft", bty="n"),
                xlab="Year", 
                ylab="var1")
### HSD results for the Old Stands
text(barx,1, label=c("c"," ","ab"," ", "abc", "" ,"bc"," ","a"," ","bc"," "),cex=1,pos=3)
err3 <- error.bars(barx,d.matrix,se.matrix)