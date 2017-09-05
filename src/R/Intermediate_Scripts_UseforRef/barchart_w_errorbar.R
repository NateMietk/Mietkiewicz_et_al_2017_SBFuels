attach(asf)
par(mfrow = c(1,3))


d.mean <- tapply( X = Duff, INDEX = list(Year), FUN=mean)
d.std <- tapply( X = Duff, INDEX = list(Year), FUN=sd)

l.mean <- tapply( X = Litter, INDEX = list(Year), FUN=mean)
l.std <- tapply( X = Litter, INDEX = list(Year), FUN=sd)

f.mean <- tapply( X = Fuelbed, INDEX = list(Year), FUN=mean)
f.std <- tapply( X = Fuelbed, INDEX = list(Year), FUN=sd)

### function to calculate the standard error of the mean

barx <- barplot(d.mean,ylim=c(0,15), col="gray", axis.lty=1, xlab="Year", ylab="Duff (cm)")
##text(barx,1, label=c("c","ab", "abc","bc","a","bc"),cex=1,pos=3)
err1 <- error.bar(barx,d.mean, (d.std/sqrt(length(Duff))))



barx <- barplot(l.mean,ylim=c(0,15), col="gray", axis.lty=1, xlab="Year", ylab="Litter (cm)")
err1 <- error.bar(barx,l.mean, (l.std/sqrt(length(Litter))))

barx <- barplot(f.mean,ylim=c(0,15), col="gray", axis.lty=1, xlab="Year", ylab="Fuelbed (cm)")
err1 <- error.bar(barx,f.mean, (f.std/sqrt(length(Fuelbed))))
