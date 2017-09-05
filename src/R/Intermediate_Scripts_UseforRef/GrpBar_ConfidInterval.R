library(sciplot)
library(agricolae)

attach(SDLFL)
par(mfrow = c(3,1))

mod.dy <- lm(Duff ~ Year)
mod.da <- lm(Duff ~ Age)
HSD.dy <- HSD.test(mod.dy,"Year", alpha = 0.05, group=TRUE,console=TRUE,main="Duff (cm) compared by Age")
HSD.da <- HSD.test(mod.da,"Age", alpha = 0.05, group=TRUE,console=TRUE,main="Duff (cm) compared by Year")

# Plot grouped bar charts that average the variable based on the factor and group.  Also will add the confidence interval based on 
# the standard error from the plot level measures
bpd = with(SDLFL, bargraph.CI(x.factor=Year, group=Age, response=Duff,
                          lc=FALSE, xlab="Year", ylab = "Duff (cm)",
                          legend=TRUE, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1,
                          ci.fun=function(x) {c(mean(x) - 1.96*se(x), mean(x) + 1.96*se(x))}))

bpl = with(SDLFL, bargraph.CI(x.factor=Year, group=Age, response=Litter,
                              lc=FALSE, xlab="Year", ylab = "Litter (cm)",
                              legend=F, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1,
                              ci.fun=function(x) {c(mean(x) - 1.96*se(x), mean(x) + 1.96*se(x))}))

bpf = with(SDLFL, bargraph.CI(x.factor=Year, group=Age, response=Fuelbed,
                              lc=FALSE, xlab="Year", ylab = "Fuelbed (cm)",
                              legend=F, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1,
                              ci.fun=function(x) {c(mean(x) - 1.96*se(x), mean(x) + 1.96*se(x))}))