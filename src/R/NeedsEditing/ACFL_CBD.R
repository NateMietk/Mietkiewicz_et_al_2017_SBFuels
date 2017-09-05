library(ggplot2); library(lattice); library(sciplot)

ACFL <- read.csv("~/Dropbox/PhD Program/FieldSeason_2013/2013_Datasheets/FuelsSites_2013/Fuel_Output_Sheets/SFuels_R/CFL.csv")

#Convert kg/m2 to MG/ha
ACFL$AFCL <- ACFL$AFCL*10

#Create categories for Live and Dead based on Status groupings
ACFL$LiDe <- ACFL$Status
levels(ACFL$LiDe)[match("Li",levels(ACFL$LiDe))] <- "Live"
levels(ACFL$LiDe) <- c(levels(ACFL$LiDe), "Dead")
ACFL[ACFL$Status %in% c("Gr", "Ye", "Nd", "Tw", "Br", "Sn", "De"), "LiDe"] <- "Dead"


ACFL.sum <- aggregate(cbind(AFCL, CBD) ~ Site + Year + Age + Plot + LiDe, data=ACFL, FUN=sum)
ACFL.sum.species <- aggregate(cbind(AFCL, CBD) ~ Site + Year + Age + Plot + Species+ LiDe, data=ACFL, FUN=sum)

ACFL.spruce <- subset(ACFL.sum.species, Species=='PIEN')
ACFL.young <- subset(ACFL.sum, Age=='Young')
ACFL.old <-subset(ACFL.sum, Age=='Old')

pdf(file="ACFL_CBD.pdf", width = 8, height = 9)
op <- par(family = "Times")
par(mfrow = c(2,1))
ACFL.y <- with(ACFL.young, bargraph.CI(x.factor=Year, group=LiDe, response=AFCL,family = "Times",
                                      lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Avaliable canopy fuel load (MG/ha) in young stands",
                                      legend=F, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1, col= c("dark gray", "white")))
ACFL.o <- with(ACFL.old, bargraph.CI(x.factor=Year, group=LiDe, response=AFCL,family = "Times",
                                     lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Avaliable canopy fuel load (MG/ha) in old stands",
                                       legend=T, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1, col= c("dark gray", "white")))
CBD.y = with(ACFL.young, bargraph.CI(x.factor=Year, group=LiDe, response=CBD,family = "Times",
                                     lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "CBD (kg/m2) in young stands",
                                     legend=F, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1, col= c("dark gray", "white")))
CBD.o = with(ACFL.old, bargraph.CI(x.factor=Year, group=LiDe, response=CBD,family = "Times",
                                   lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "CBD (kg/m2) in old stands",
                                   legend=F, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1, col= c("dark gray", "white")))
par(op)
dev.off()

pdf(file="ACFL_CBD_42.pdf", width = 8, height = 9)
op <- par(family = "Times")
par(mfrow = c(4,2))
ACFL.y <- with(ACFL.young, bargraph.CI(x.factor=Year, group=LiDe, response=AFCL,family = "Times",
                                       lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Avaliable canopy fuel load (MG/ha)",
                                       main  = "Young Stands", legend=T, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1, col= c("dark gray", "white")))
ACFL.o <- with(ACFL.old, bargraph.CI(x.factor=Year, group=LiDe, response=AFCL,family = "Times",
                                     lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "Avaliable canopy fuel load (MG/ha)",
                                     main  = "Old Stands",legend=F, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1, col= c("dark gray", "white")))
CBD.y = with(ACFL.young, bargraph.CI(x.factor=Year, group=LiDe, response=CBD,family = "Times",
                                     lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "CBD (kg/m2)",
                                     legend=F, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1, col= c("dark gray", "white")))
CBD.o = with(ACFL.old, bargraph.CI(x.factor=Year, group=LiDe, response=CBD,family = "Times",
                                   lc=FALSE, err.width = .03, xlab="Year of SB Outbreak", ylab = "CBD (kg/m2)",
                                   legend=F, x.leg=3.3, cex.leg=1, cex.names=1, cex.lab = 1, col= c("dark gray", "white")))
par(op)
dev.off()

cairo_pdf(file="ACFL.pdf", width = 10, height = 6)
q <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),panel.background = element_rect(fill = 'white', colour = 'black'))
t <-theme(plot.title=element_text(family="Times New Roman"), axis.title.x=element_text(family="Times New Roman"),axis.title.y=element_text(family="Times New Roman"), axis.text.x=element_text(family="Times New Roman"),
          axis.text.y=element_text(family="Times New Roman"), legend.text=element_text(family="Times New Roman"),
          strip.text= element_text(family="Times New Roman"),legend.title=element_blank())
ggplot(ACFL.sum, aes(x = Age, y = AFCL, fill = LiDe)) + t + q +
  labs(x="Stand age",y="Available Crown Fuel")+
  geom_bar(stat = 'identity', position = 'stack') + 
  facet_grid(~ Year)

dev.off()

