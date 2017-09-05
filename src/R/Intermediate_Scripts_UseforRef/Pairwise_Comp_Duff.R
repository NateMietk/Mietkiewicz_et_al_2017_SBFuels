library(agricolae)
attach(SDLFL)


#####################################################
#####################################################
#####################################################
##DUFF
#####################################################
#####################################################
#####################################################

# Means of the treatment groups (years)
d.mean <- tapply( X = Duff, INDEX = list(Year), FUN=mean)
d.std <- tapply( X = Duff, INDEX = list(Year), FUN=sd)

# Linear Model for the TukeyHSD post-hoc test
mod.lm <- lm(Duff ~ Year)
summary(mod.lm)

# Produce an omnibus ANOVA test
mod.anova <- anova(mod.lm)
summary(mod.anova)

boxplot(Duff~Year)
# Pairwise comparisons between the treatment groups (years)
# p.adj: the p-value adjustment method used to control for the family-wise Type I 
# error rate across the comparisons; one of "none", "bonferroni", "holm", "hochberg",
# "hommel", "BH", or "BY"

# p.adj = "non"
pairwise.t.test(Duff, Year, p.adj= "none")

# p.adj = "bonferroni"
pairwise.t.test(Duff, Year, p.adj = "bonferroni")

# p.adj = "holm"
pairwise.t.test(Duff, Year, p.adj = "holm")

# TukeyHSD method for the 95% confidence interval
out <- HSD.test(mod.lm,"Year", alpha = 0.05, group=TRUE,console=TRUE,main="Duff (cm)")


Xlab.d = "Year of Outbreak"     # x label
Ylab.d = "Duff (cm)"             # y label

#Setting the layout for the final graphic output
#par(mfrow=c(1,1),mar=c(7,7,2,2), oma=c(3,3,1,0))
#quartz(h=7.5, w=10)
par(mfrow=c(1,1),cex=1)
duff.bar <- bar.group(out$groups,
          ylim=c(0, max(out$means[1,])+0.5), 
          xlim=c(out$Treatments),
          col="gray",
          border="black",
          las=1, 
          main = "Pairwise Comparisons using the TukeyHSD Function", 
          xlab = Xlab.d, 
          ylab= Ylab.d)
legend("topright",                               # Add a legend to the plot  
       legend=c("Old Stand"),             # Text for the legend  
       fill=c("gray"))                  # Fill for boxes of the legend  

#axis()
# add an x-axis
#abline(h=0, col="black", lty = 1, lwd = 2)
#Final Graphic
out<-HSD.test(mod.lm,"Year", group=FALSE)
duff.err <- bar.err(out$means,
        variation="SE",
        ylim=c(0, 13),
        main = "Duff depths within old spruce/fir stands", 
        xlab = Xlab.d, 
        ylab= Ylab.d)
##  Get character width and height in user coordinates.
chw <- par()$cxy[ 1 ]  ##  character width
chh <- par()$cxy[ 2 ]  ##  character height

text(x= c("0.75","1.90","3.1","4.3","5.5","6.7"), 
     y=d.mean/2, 
     label=c("c","ab","abc", "bc","a","bc"))
legend("topright",                               # Add a legend to the plot  
       legend=c("Old Stand"),             # Text for the legend  
       fill=c("gray"))                  # Fill for boxes of the legend  
means<-out$means

#####################################################
#####################################################
#####################################################
##LITTER
#####################################################
#####################################################
#####################################################

# Means of the treatment groups (years)
l.mean <- tapply( X = Litter, INDEX = list(Year), FUN=mean)
l.std <- tapply( X = Litter, INDEX = list(Year), FUN=sd)

# Linear Model for the TukeyHSD post-hoc test
mod.lm <- lm(Litter ~ Year)
summary(mod.lm)

# Produce an omnibus ANOVA test
mod.anova <- anova(mod.lm)
summary(mod.anova)

boxplot(Litter~Year)
# Pairwise comparisons between the treatment groups (years)
# p.adj: the p-value adjustment method used to control for the family-wise Type I 
# error rate across the comparisons; one of "none", "bonferroni", "holm", "hochberg",
# "hommel", "BH", or "BY"

# p.adj = "non"
pairwise.t.test(Litter, Year, p.adj= "none")

# p.adj = "bonferroni"
pairwise.t.test(Litter, Year, p.adj = "bonferroni")

# p.adj = "holm"
pairwise.t.test(Litter, Year, p.adj = "holm")

# TukeyHSD method for the 95% confidence interval
out <- HSD.test(mod.lm,"Year", alpha = 0.05, group=TRUE,console=TRUE,main="Litter (cm)")


Xlab.l = "Year of Outbreak"     # x label
Ylab.l = "Litter (cm)"             # y label

#Setting the layout for the final graphic output
#par(mfrow=c(1,1),mar=c(7,7,2,2), oma=c(3,3,1,0))
#quartz(h=7.5, w=10)
par(mfrow=c(1,1),cex=1)
bar.group(out$groups,
          ylim=c(0, max(out$means[1,])+0.5), 
          xlim=c(out$Treatments),
          col="gray",
          border="black",
          las=1, 
          main = "Pairwise Comparisons using the TukeyHSD Function", 
          xlab = Xlab.l, 
          ylab= Ylab.l)
legend("topright",                               # Add a legend to the plot  
       legend=c("Old Stand"),             # Text for the legend  
       fill=c("gray"))                  # Fill for boxes of the legend  

#axis()
# add an x-axis
#abline(h=0, col="black", lty = 1, lwd = 2)
#Final Graphic
out<-HSD.test(mod.lm,"Year", group=FALSE)
bar.err(out$means,
        variation="SE",
        ylim=c(0, 5),
        main = "Litter depths within old spruce/fir stands", 
        xlab = Xlab.l, 
        ylab= Ylab.l)
##  Get character width and height in user coordinates.
chw <- par()$cxy[ 1 ]  ##  character width
chh <- par()$cxy[ 2 ]  ##  character height

text(x= c("0.75","1.90","3.1","4.3","5.5","6.7"), 
     y=d.mean/2, 
     label=c("b","a","ab", "ab","ab","ab"))
legend("topright",                               # Add a legend to the plot  
       legend=c("Old Stand"),             # Text for the legend  
       fill=c("gray"))                  # Fill for boxes of the legend  
means<-out$means


#####################################################
#####################################################
#####################################################
##FUELBED
#####################################################
#####################################################
#####################################################

# Means of the treatment groups (years)
f.mean <- tapply( X = Fuelbed, INDEX = list(Year), FUN=mean)
f.std <- tapply( X = Fuelbed, INDEX = list(Year), FUN=sd)

# Linear Model for the TukeyHSD post-hoc test
mod.lm <- lm(Fuelbed ~ Year)
summary(mod.lm)

# Produce an omnibus ANOVA test
mod.anova <- anova(mod.lm)
summary(mod.anova)

boxplot(Fuelbed~Year)
# Pairwise comparisons between the treatment groups (years)
# p.adj: the p-value adjustment method used to control for the family-wise Type I 
# error rate across the comparisons; one of "none", "bonferroni", "holm", "hochberg",
# "hommel", "BH", or "BY"

# p.adj = "non"
pairwise.t.test(Fuelbed, Year, p.adj= "none")

# p.adj = "bonferroni"
pairwise.t.test(Fuelbed, Year, p.adj = "bonferroni")

# p.adj = "holm"
pairwise.t.test(Fuelbed, Year, p.adj = "holm")

# TukeyHSD method for the 95% confidence interval
out <- HSD.test(mod.lm,"Year", alpha = 0.05, group=TRUE,console=TRUE,main="Fuelbed (cm)")


Xlab.f = "Year of Outbreak"     # x label
Ylab.f = "Fuelbed (cm)"         # y label

#Setting the layout for the final graphic output
#par(mfrow=c(1,1),mar=c(7,7,2,2), oma=c(3,3,1,0))
#quartz(h=7.5, w=10)
par(mfrow=c(1,1),cex=1)
bar.group(out$groups,
          ylim=c(0, 1), 
          xlim=c(out$Treatments),
          col="gray",
          border="black",
          las=1, 
          main = "Pairwise Comparisons using the TukeyHSD Function", 
          xlab = Xlab.f, 
          ylab= Ylab.f)
legend("topright",                               # Add a legend to the plot  
       legend=c("Old Stand"),             # Text for the legend  
       fill=c("gray"))                  # Fill for boxes of the legend  

#axis()
# add an x-axis
#abline(h=0, col="black", lty = 1, lwd = 2)
#Final Graphic
out<-HSD.test(mod.lm,"Year", group=FALSE)
bar.err(out$means,
        variation="SE",
        ylim=c(0, 18),
        main = "Fuelbed depths within old spruce/fir stands", 
        xlab = Xlab.f, 
        ylab= Ylab.f)
##  Get character width and height in user coordinates.
chw <- par()$cxy[ 1 ]  ##  character width
chh <- par()$cxy[ 2 ]  ##  character height

text(x= c("0.75","1.90","3.1","4.3","5.5","6.7"), 
     y=d.mean/2, 
     label=c("a","a","a", "a","a","a"))
legend("topright",                               # Add a legend to the plot  
       legend=c("Old Stand"),             # Text for the legend  
       fill=c("gray"))                  # Fill for boxes of the legend  
means<-out$means