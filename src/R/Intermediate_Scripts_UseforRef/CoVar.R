

# Libaries and directories ------------------------------------------------
library(openxlsx); library(sciplot); library(agricolae); library(ggplot2); library(lattice); library(AICcmodavg); library(ez)
library(doBy);library(ggplot2);library(plyr);library(reshape2);library(htmlTable);library(gridExtra)
library(ftsa);library(sciplot); library(tidyr); library(devtools); library(ggplot2); library(grid); library(dplyr)

setwd("/Users/NateM/Dropbox/PhD Program/Dissertation_Research/2_Fuels/Fuel_Output_Sheets/CanopyFuels")
#acfl.lsms <- read.xlsx("CFL_pertree.xlsx", sheet = "LS and MS")
#acfl.us <- read.xlsx("CFL_pertree.xlsx", sheet = "US")
#acfl.vert <- read.xlsx("CFL_pertree.xlsx", sheet = "VertCol")
acfl.site <- read.xlsx("CFL_pertree.xlsx", sheet = "ACFL-1mwsp")
site.info <- read.xlsx("CFL_pertree.xlsx", sheet = "Sitein")

# Lower stratum --------------------------------------------------
require(plyr)
acfl.site.ls <- subset(acfl.site, VertCol > 0 & VertCol <= 5)
acfl.site.ls$VertCol <- NULL
acfl.site.ls <- melt(acfl.site.ls)
acfl.site.ls$value <- round(acfl.site.ls$value,4)
acfl.site.ls <- separate(acfl.site.ls, variable, into= c("site", "subplot"), sep="\\.")
#acfl.site.ls$ssplot <- with(acfl.site.ls, paste0(site, ".",subplot))

acfl.ls <- join(acfl.site.ls, site.info, by = c("site"), type = "left")
acfl.ls <- subset(acfl.ls, value!=0)


info <-  data.frame(sb.age =  c( "OldEndemic", "YoungEndemic","Old1940", "Old1960", "Old1990", "Old2000", "Old2010", "Young1990", "Young2000", "Young2010"),
  age = c("Old", "Young", "Old", "Old", "Old", "Old", "Old", "Young", "Young", "Young"),
  year = c("Endemic", "Endemic", "1940", "1960", "1990", "2000", "2010", "1990", "2000", "2010"))
tmp1 <- data.frame(unique(acfl.ls$site))
names(tmp1)[names(tmp1)=="unique.acfl.ls.site."] <- "site"
tmp <- join(tmp1, site.info, by= c("site"), type="left")

CV1 <-NULL; cs.ls.aov <- NULL

for (i in tmp[,1]){
    q<- subset(acfl.ls, site== i)
    t <- aov(value ~ subplot, data=q)
    cs.ls.aov[[i]] <- summary(t)
    CV1[[i]] <- cv.model(t)
}

s.ls <- as.data.frame(CV1);s.ls$site <- tmp[,1]
s.ls <- join(tmp, s.ls, by="site"); s.ls$id <- NULL
tmp2 <- summaryBy(CV1 ~ sb.age, data=s.ls,  FUN=function(x){c(mn = mean(x),se=sd(x)/sqrt(length(x)))})
cs.ls <- join(info, tmp2, by ="sb.age", type= "left")
names(cs.ls)[names(cs.ls)=="CV1.mn"] <- "cv"
names(cs.ls)[names(cs.ls)=="CV1.se"] <- "se"

# Mid stratum --------------------------------------------------------------
acfl.site.ms <- subset(acfl.site, VertCol > 5 & VertCol < 10)
acfl.site.ms$VertCol <- NULL
acfl.site.ms <- melt(acfl.site.ms)
acfl.site.ms$value <- round(acfl.site.ms$value,4)
acfl.site.ms <- separate(acfl.site.ms, variable, into= c("site", "subplot"), sep="\\.")
#acfl.site.ms$ssplot <- with(acfl.site.ms, paste0(site, ".",subplot))

acfl.ms <- join(acfl.site.ms, site.info, by = c("site"), type = "left")
acfl.ms <- subset(acfl.ms, value!=0)

info <-  data.frame(sb.age =  c( "OldEndemic", "YoungEndemic","Old1940", "Old1960", "Old1990", "Old2000", "Old2010", "Young1990", "Young2000", "Young2010"),
                    age = c("Old", "Young", "Old", "Old", "Old", "Old", "Old", "Young", "Young", "Young"),
                    year = c("Endemic", "Endemic", "1940", "1960", "1990", "2000", "2010", "1990", "2000", "2010"))
tmp1 <- data.frame(unique(acfl.ms$site))
names(tmp1)[names(tmp1)=="unique.acfl.ms.site."] <- "site"
tmp <- join(tmp1, site.info, by= c("site"), type="left")

CV1 <-NULL
cs.ms.aov <- NULL

for (i in tmp[,1]){
  q<- subset(acfl.ms, site== i)
  t <- aov(value ~ subplot, data=q)
  cs.ms.aov[[i]] <- summary(t)
  CV1[[i]] <- cv.model(t)
}

s.ms <- as.data.frame(CV1);s.ms$site <- tmp[,1]
s.ms <- join(tmp, s.ms, by="site"); s.ms$id <- NULL
tmp2 <- summaryBy(CV1 ~ sb.age, data=s.ms,  FUN=function(x){c(mn = mean(x),se=sd(x)/sqrt(length(x)))})
cs.ms <- join(info, tmp2, by ="sb.age", type= "left")
names(cs.ms)[names(cs.ms)=="CV1.mn"] <- "cv"
names(cs.ms)[names(cs.ms)=="CV1.se"] <- "se"

# Upper stratum -----------------------------------------------------------
acfl.site.us <- subset(acfl.site, VertCol >= 10)
acfl.site.us$VertCol <- NULL
acfl.site.us <- melt(acfl.site.us)
acfl.site.us$value <- round(acfl.site.us$value,4)
acfl.site.us <- separate(acfl.site.us, variable, into= c("site", "subplot"), sep="\\.")
#acfl.site.us$ssplot <- with(acfl.site.us, paste0(site, ".",subplot))

acfl.us <- join(acfl.site.us, site.info, by = c("site"), type = "left")
acfl.us <- subset(acfl.us, value!=0)
#acfl.us <- subset(acfl.us, site!= "13-60A" & site!="13-61A" & site!="13-101A"& site!="13-45A" & site!="13-30C" & site!="13-48C" & site!="13-49B" & site!="13-42A")

info <-  data.frame(sb.age =  c( "OldEndemic", "YoungEndemic","Old1940", "Old1960", "Old1990", "Old2000", "Old2010", "Young1990", "Young2000", "Young2010"),
                    age = c("Old", "Young", "Old", "Old", "Old", "Old", "Old", "Young", "Young", "Young"),
                    year = c("Endemic", "Endemic", "1940", "1960", "1990", "2000", "2010", "1990", "2000", "2010"))
tmp1 <- data.frame(unique(acfl.us$site))
names(tmp1)[names(tmp1)=="unique.acfl.us.site."] <- "site"
tmp <- join(tmp1, site.info, by= c("site"), type="left")

CV1 <-NULL

for (i in tmp[,1]){
  q<- subset(acfl.us, site== i)
  t <- aov(value ~ subplot, data=q)
  CV1[[i]] <- cv.model(t)
}

s.us <- as.data.frame(CV1);s.us$site <- tmp[,1]
s.us <- join(tmp, s.us, by="site"); s.us$id <- NULL
tmp2 <- summaryBy(CV1 ~ sb.age, data=s.us,  FUN=function(x){c(mn = mean(x),se=sd(x)/sqrt(length(x)))})
cs.us <- join(info, tmp2, by ="sb.age", type= "left")
names(cs.us)[names(cs.us)=="CV1.mn"] <- "cv"
names(cs.us)[names(cs.us)=="CV1.se"] <- "se"

# Entire vertical column --------------------------------------------------
acfl.site.wc <- acfl.site
acfl.site.wc$VertCol <- NULL
acfl.site.wc <- melt(acfl.site.wc)
acfl.site.wc <- separate(acfl.site.wc, variable, into= c("site", "subplot"), sep="\\.")
#acfl.site.wc$ssplot <- with(acfl.site.wc, paste0(site, ".",subplot))

acfl.wc <- join(acfl.site.wc, site.info, by = c("site"), type = "left")
acfl.wc <- subset(acfl.wc, value!=0)

info <-  data.frame(sb.age =  c( "OldEndemic", "YoungEndemic","Old1940", "Old1960", "Old1990", "Old2000", "Old2010", "Young1990", "Young2000", "Young2010"),
                    age = c("Old", "Young", "Old", "Old", "Old", "Old", "Old", "Young", "Young", "Young"),
                    year = c("Endemic", "Endemic", "1940", "1960", "1990", "2000", "2010", "1990", "2000", "2010"))
tmp1 <- data.frame(unique(acfl.wc$site))
names(tmp1)[names(tmp1)=="unique.acfl.wc.site."] <- "site"
tmp <- join(tmp1, site.info, by= c("site"), type="left")

CV1 <-NULL

for (i in tmp[,1]){
  q<- subset(acfl.wc, site== i)
  t <- aov(value ~ subplot, data=q)
  CV1[[i]] <- cv.model(t)
}

s.wc <- as.data.frame(CV1);s.wc$site <- tmp[,1]
s.wc <- join(tmp, s.wc, by="site"); s.wc$id <- NULL
tmp2 <- summaryBy(CV1 ~ sb.age, data=s.wc,  FUN=function(x){c(mn = mean(x),se=sd(x)/sqrt(length(x)))})
cs.wc <- join(info, tmp2, by ="sb.age", type= "left")
names(cs.wc)[names(cs.wc)=="CV1.mn"] <- "cv"
names(cs.wc)[names(cs.wc)=="CV1.se"] <- "se"

# Surface fuels -----------------------------------------------------------
sfl <- read.xlsx("~/Dropbox/PhD Program/Dissertation_Research/2_Fuels/Fuel_Output_Sheets/SurfaceFuels/ASFL.xlsx", sheet = "Surf_CoVar")

# Litter and Duff ---------------------------------------------------------
info <-  data.frame(sb.age =  c( "OldEndemic", "YoungEndemic","Old1940", "Old1960", "Old1990", "Old2000", "Old2010", "Young1990", "Young2000", "Young2010"),
                    age = c("Old", "Young", "Old", "Old", "Old", "Old", "Old", "Young", "Young", "Young"),
                    year = c("Endemic", "Endemic", "1940", "1960", "1990", "2000", "2010", "1990", "2000", "2010"))
tmp1 <- data.frame(unique(sfl$site))
names(tmp1)[names(tmp1)=="unique.sfl.site."] <- "site"
tmp <- join(tmp1, site.info, by= c("site"), type="left")

CV1 <-NULL

for (i in tmp[,1]){
  q<- subset(sfl, site== i)
  t <- aov(duff_litter ~ subplot, data=q)
  CV1[[i]] <- cv.model(t)
}

s.dl <- as.data.frame(CV1);s.dl$site <- tmp[,1]
s.dl <- join(tmp, s.dl, by="site"); s.dl$id <- NULL
tmp2 <- summaryBy(CV1 ~ sb.age, data=s.dl,  FUN=function(x){c(mn = mean(x),se=sd(x)/sqrt(length(x)))})
duff.lit <- join(info, tmp2, by ="sb.age", type= "left")
names(duff.lit)[names(duff.lit)=="CV1.mn"] <- "cv"
names(duff.lit)[names(duff.lit)=="CV1.se"] <- "se"

# Fine surface fuels ------------------------------------------------------
info <-  data.frame(sb.age =  c( "OldEndemic", "YoungEndemic","Old1940", "Old1960", "Old1990", "Old2000", "Old2010", "Young1990", "Young2000", "Young2010"),
                    age = c("Old", "Young", "Old", "Old", "Old", "Old", "Old", "Young", "Young", "Young"),
                    year = c("Endemic", "Endemic", "1940", "1960", "1990", "2000", "2010", "1990", "2000", "2010"))
tmp1 <- data.frame(unique(sfl$site))
names(tmp1)[names(tmp1)=="unique.sfl.site."] <- "site"
tmp <- join(tmp1, site.info, by= c("site"), type="left")

CV1 <-NULL

for (i in tmp[,1]){
  q<- subset(sfl, site== i)
  t <- aov(fine_fuel ~ subplot, data=q)
  CV1[[i]] <- cv.model(t)
}

s.ff <- as.data.frame(CV1);s.ff$site <- tmp[,1]
s.ff <- join(tmp, s.ff, by="site"); s.ff$id <- NULL
tmp2 <- summaryBy(CV1 ~ sb.age, data=s.ff,  FUN=function(x){c(mn = mean(x),se=sd(x)/sqrt(length(x)))})
fine.fuel <- join(info, tmp2, by ="sb.age", type= "left")
names(fine.fuel)[names(fine.fuel)=="CV1.mn"] <- "cv"
names(fine.fuel)[names(fine.fuel)=="CV1.se"] <- "se"

# Sound Coarse fuels ------------------------------------------------------
info <-  data.frame(sb.age =  c( "OldEndemic", "YoungEndemic","Old1940", "Old1960", "Old1990", "Old2000", "Old2010", "Young1990", "Young2000", "Young2010"),
                    age = c("Old", "Young", "Old", "Old", "Old", "Old", "Old", "Young", "Young", "Young"),
                    year = c("Endemic", "Endemic", "1940", "1960", "1990", "2000", "2010", "1990", "2000", "2010"))
tmp1 <- data.frame(unique(sfl$site))
names(tmp1)[names(tmp1)=="unique.sfl.site."] <- "site"
tmp <- join(tmp1, site.info, by= c("site"), type="left")

CV1 <-NULL

for (i in tmp[,1]){
  q<- subset(sfl, site== i)
  t <- aov(s_coarse ~ subplot, data=q)
  CV1[[i]] <- cv.model(t)
}

s.sc <- as.data.frame(CV1);s.sc$site <- tmp[,1]
s.sc <- join(tmp, s.sc, by="site"); s.sc$id <- NULL
tmp2 <- summaryBy(CV1 ~ sb.age, data=s.sc,  FUN=function(x){c(mn = mean(x),se=sd(x)/sqrt(length(x)))})
s.coarse <- join(info, tmp2, by ="sb.age", type= "left")
names(s.coarse)[names(s.coarse)=="CV1.mn"] <- "cv"
names(s.coarse)[names(s.coarse)=="CV1.se"] <- "se"

# Rotten Coarse fuels ------------------------------------------------------
info <-  data.frame(sb.age =  c( "OldEndemic", "YoungEndemic","Old1940", "Old1960", "Old1990", "Old2000", "Old2010", "Young1990", "Young2000", "Young2010"),
                    age = c("Old", "Young", "Old", "Old", "Old", "Old", "Old", "Young", "Young", "Young"),
                    year = c("Endemic", "Endemic", "1940", "1960", "1990", "2000", "2010", "1990", "2000", "2010"))
tmp1 <- data.frame(unique(sfl$site))
names(tmp1)[names(tmp1)=="unique.sfl.site."] <- "site"
tmp <- join(tmp1, site.info, by= c("site"), type="left")

CV1 <-NULL

for (i in tmp[,1]){
  q<- subset(sfl, site== i)
  t <- aov(r_coarse ~ subplot, data=q)
  CV1[[i]] <- cv.model(t)
}

s.rc <- as.data.frame(CV1);s.rc$site <- tmp[,1]
s.rc <- join(tmp, s.rc, by="site"); s.rc$id <- NULL
tmp2 <- summaryBy(CV1 ~ sb.age, data=s.rc,  FUN=function(x){c(mn = mean(x),se=sd(x)/sqrt(length(x)))})
r.coarse <- join(info, tmp2, by ="sb.age", type= "left")
names(r.coarse)[names(r.coarse)=="CV1.mn"] <- "cv"
names(r.coarse)[names(r.coarse)=="CV1.se"] <- "se"

# AOV ---------------------------------------------------------------------

ls.o <- summary(aov(CV1 ~ year, data=subset(s.ls, age == "Old")))     #1
ls.y <- summary(aov(CV1 ~ year, data=subset(s.ls, age == "Young")))     #2

ms.o <- summary(aov(CV1 ~ year, data=subset(s.ms, age == "Old")))     #3
ms.y <- summary(aov(CV1 ~ year, data=subset(s.ms, age == "Young")))     #4

us.o <- summary(aov(CV1 ~ year, data=subset(s.us, age == "Old")))     #5
us.y <- summary(aov(CV1 ~ year, data=subset(s.us, age == "Young")))     #6

wc.o <- summary(aov(CV1 ~ year, data=subset(s.wc, age == "Old")))     #7
wc.y <- summary(aov(CV1 ~ year, data=subset(s.wc, age == "Young")))     #8

dl.o <- summary(aov(CV1 ~ year, data=subset(s.dl, age == "Old")))     #9
dl.y <- summary(aov(CV1 ~ year, data=subset(s.dl, age == "Young")))     #10

ff.o <- summary(aov(CV1 ~ year, data=subset(s.ff, age == "Old")))     #11
ff.y <- summary(aov(CV1 ~ year, data=subset(s.ff, age == "Young")))     #12

sc.o <- summary(aov(CV1 ~ year, data=subset(s.sc, age == "Old")))     #13
sc.y <- summary(aov(CV1 ~ year, data=subset(s.sc, age == "Young")))     #14

rc.o <- summary(aov(CV1 ~ year, data=subset(s.rc, age == "Old")))     #15
rc.y <- summary(aov(CV1 ~ year, data=subset(s.rc, age == "Young")))     #16

aov.out <- c(ls.o, ls.y, ms.o, ms.y, us.o, us.y, wc.o, wc.y, dl.o, dl.y, ff.o, ff.y, sc.o, sc.y, rc.o, rc.y)

# Plotting theme (ggplot2) ####
theme_cust<-theme(
  plot.title = element_text(hjust = 0.5,lineheight=.8,size=12, family = "Times"),
  legend.position="right",
  legend.text = element_text(colour="black", size = 12),
  legend.title = element_text(size=12),
  legend.position="left",
  legend.title.align=0.5,
  plot.margin = unit(c(1.5,1,1,1),"mm"), #top, right, bottom, left
  legend.background = element_rect(colour = "black"),
  axis.text.x= element_text(family= "Times"),
  axis.text.y= element_text(family= "Times"),
  axis.text=element_text(size=10),
  axis.title=element_text(size=12, family = "Times"),
  axis.text         = element_text(size = 10),
  axis.ticks        = element_line(colour = "black"),
  legend.key        = element_rect(colour = "#FFFFFF"),
  panel.background  = element_rect(fill = "white", colour = NA),
  panel.border      = element_rect(fill = NA, colour = "black"),
  #panel.grid.major  = element_line(colour = "grey90", size = 0.2),
  #panel.grid.minor  = element_line(colour = "grey98", size = 0.5),
  strip.background  = element_rect(fill = "grey80", colour = "black",size = 0.5),
  strip.text.y = element_text(size = 12, colour = "black", family="Times", face="bold")
)

# ggfaxt function ---------------------------------------------------------

ggfaxt <-
  function(ggplot2.object, x.coord = NULL, y.coord = NULL, 
           labels = NULL, ...) {
    
    x <- y <- NULL
    
    dat <- ggplot2.object$data
    look <- sapply(ggplot2.object$facet[1:2], as.character)
    empt <- function(x) {!identical(x, character(0))}
    who <- sapply(look, empt)
    if (all(who)) {
      rows <- ggplot2.object$facet[[1]][[1]]
      frow <- dat[, as.character(rows)]
      cols <- ggplot2.object$facet[[2]][[1]]
      fcol <- dat[, as.character(cols)]
      len <- length(levels(factor(fcol))) *  length(levels(factor(frow)))
      vars <- data.frame(expand.grid(levels(factor(frow)), levels(factor(fcol))))
      colnames(vars) <- c(as.character(rows), as.character(cols))
    } else {
      if (who[1]) {
        rows <- ggplot2.object$facet[[1]][[1]]
        frow <- dat[, as.character(rows)]    
        len <- length(levels(factor(frow)))
        vars <- data.frame(levels(factor(frow)), stringsAsFactors = FALSE)
        colnames(vars) <- as.character(rows)
      } else {
        cols <- ggplot2.object$facet[[2]][[1]]
        fcol <- dat[, as.character(cols)]  
        len <- length(levels(factor(fcol)))
        vars <- data.frame(levels(factor(fcol)), stringsAsFactors = FALSE)
        colnames(vars) <- as.character(cols)    
      }
    }
    if (any(class(ggplot2.object) %in% c("ggplot", "gg"))) {
      if (is.null(labels)) {
        labels <- LETTERS[1:len]
      }
      if (!length(labels) %in% c(1, len)) {
        stop("labels must be of length 1 or equal to number of facets")
      }
      if (length(x.coord) == 1) {
        x.coord <- rep(x.coord, len)
      }
      if (length(y.coord) == 1) {
        y.coord <- rep(y.coord, len)
      }
      text.df <- data.frame(x = x.coord, y = y.coord, vars, labs=labels)
    } else {
      if (class(ggplot2.object) == "qfacet") {
        text.df <- ggplot2.object$dat
        if (!is.null(x.coord)) {
          text.df$x.coord <- x.coord
        }
        if (!is.null(y.coord)) {
          text.df$y.coord <- y.coord
        }
        if (!is.null(labels)) {
          text.df$labs <- labels
        }
        ggplot2.object <- ggplot2.object$original
      }
    }
    p <- ggplot2.object + ggplot2::geom_text(ggplot2::aes_string('x', 'y', 
                                                                 label='labs', group=NULL), data=text.df, ...)
    print(p)
    v <- list(original = ggplot2.object, new = p, dat = text.df)
    class(v) <- "qfacet"
    invisible(v)
  }


# Plots -------------------------------------------------------------------
pdf("CoVar.pdf", height = 8, width = 12)
p.ls <- ggplot(cs.ls,aes(x=year,y=cv)) +
  scale_colour_discrete(guide = FALSE) +
  facet_grid(age~.,  scales = "free_y")+
  scale_y_continuous(limits= c(0,70))+
  geom_point(colour= "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin=cv-se, ymax=cv+se),width=.2) +
  theme_cust + theme(strip.text.y = element_blank())+  
  ggtitle("Available crown fuel load\n (lower canopy)")+
  xlab("") + ylab("Coefficient of variation within plot (%)")+
  geom_text(aes(x, y, label=lab),
            data=data.frame(x=2.5, y=65, lab=c("(a) F(5,17) = 0.89, p = 0.5091",
                                                "(e) F(5,17) = 7.81, p = 0.0169"),age=c("Old", "Young")), 
            family= "Times", size = 3, vjust=1)
#p.ls <- ggfaxt(ggplot2.object = p.ls, x.coor = 1, y.coor = 60, labels = c("(a)", "(e)"), color="black", family= "serif")

p.ms <-  ggplot(cs.ms, aes(x=year,y=cv)) +
  scale_colour_discrete(guide = FALSE) +
  facet_grid(age~.,  scales = "free_y")+
  scale_y_continuous(limits= c(0,70))+
  geom_point(colour= "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin=cv-se, ymax=cv+se),width=.2) +
  theme_cust + theme(#axis.text.y=element_blank(), 
                     strip.text.y = element_blank())+
  ggtitle("Available crown fuel load\n (mid-canopy)")+
  xlab("") +ylab("")+
  geom_text(aes(x, y, label=lab),
            data=data.frame(x=2.5, y=65, lab=c("(b) F(5,17) = 1.23, p = 0.3376",
                                               "(f) F(5,17) = 1.58, p = 0.2893"),age=c("Old", "Young")), 
            family= "Times", size = 3, vjust=1)


p.us <-  ggplot(cs.us,aes(x=year,y=cv)) +
  scale_colour_discrete(guide = FALSE) +
  facet_grid(age~.,  scales = "free_y")+
  scale_y_continuous(limits= c(0,250))+
  geom_point(colour= "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin=cv-se, ymax=cv+se),width=.2) +
  theme_cust + theme(#axis.text.y=element_blank(), 
                     strip.text.y = element_blank())+  
  ggtitle("Available crown fuel load\n (upper canopy)")+
  xlab("")+ylab("")+
  geom_text(aes(x, y, label=lab),
            data=data.frame(x=2.7, y=235, lab=c("(c) F(5,17) = 1.57, p = 0.2220",
                                               "(g) F(5,17) = 21.44, p = 0.0013"),age=c("Old", "Young")), 
            family= "Times", size = 3, vjust=1)


p.wc <-  ggplot(cs.wc,aes(x=year,y=cv)) +
  scale_colour_discrete(guide = FALSE) +
  facet_grid(age~.,  scales = "free_y")+
  scale_y_continuous(limits= c(0,175))+
  geom_point(colour= "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin=cv-se, ymax=cv+se),width=.2) +
  theme_cust + #theme(axis.text.y=element_blank())+
  ggtitle("Available crown fuel load\n (whole canopy)")+
  xlab("")+ylab("")+
  geom_text(aes(x, y, label=lab),
            data=data.frame(x=2.8, y=168, lab=c("(d) F(5,17) = 2.25, p = 0.0957",
                                                "(h) F(5,17) = 16.23, p = 0.0028"),age=c("Old", "Young")), 
            family= "Times", size = 3, vjust=1)


p.ld <-  ggplot(duff.lit,aes(x=year,y=cv)) +
  scale_colour_discrete(guide = FALSE) +
  facet_grid(age~.,  scales = "free_y")+
  scale_y_continuous(limits= c(0,150))+
  geom_point(colour= "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin=cv-se, ymax=cv+se),width=.2) +
  theme_cust + theme(strip.text.y = element_blank())+
  ggtitle("Litter + Duff depth")+
  xlab("")+ylab("Coefficient of variation within plot (%)")+
  geom_text(aes(x, y, label=lab),
            data=data.frame(x=2.5, y=145, lab=c("(i) F(5,17) = 0.73, p = 0.6101",
                                                "(m) F(5,17) = 2.53, p = 0.1535"),age=c("Old", "Young")), 
            family= "Times", size = 3, vjust=1)

p.ff <-  ggplot(fine.fuel,aes(x=year,y=cv)) +
  scale_colour_discrete(guide = FALSE) +
  facet_grid(age~.,  scales = "free_y")+
  scale_y_continuous(limits= c(0,150))+
  geom_point(colour= "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin=cv-se, ymax=cv+se),width=.2) +
  theme_cust + theme(strip.text.y = element_blank())+
  ggtitle("Fine woody surface fuels")+
  xlab("")+ylab("")+
  geom_text(aes(x, y, label=lab),
            data=data.frame(x=2.65, y=145, lab=c("(j) F(5,17) = 0.97, p = 0.4618",
                                                "(n) F(5,17) = 0.8669, p = 0.5080"),age=c("Old", "Young")), 
            family= "Times", size = 3, vjust=1)

p.sc <-  ggplot(s.coarse,aes(x=year,y=cv)) +
  scale_colour_discrete(guide = FALSE) +
  facet_grid(age~.,  scales = "free_y")+
  scale_y_continuous(limits= c(0,150))+
  geom_point(colour= "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin=cv-se, ymax=cv+se),width=.2) +
  theme_cust + theme(strip.text.y = element_blank())+
  ggtitle("Sound coarse woody surface fuels")+
  xlab("")+ylab("")+
  geom_text(aes(x, y, label=lab),
            data=data.frame(x=2.65, y=145, lab=c("(k) F(5,17) = 0.38, p = 0.8638",
                                                 "(o) F(5,17) = 0.35, p = 0.7905"),age=c("Old", "Young")), 
            family= "Times", size = 3, vjust=1)

p.rc <-  ggplot(r.coarse,aes(x=year,y=cv)) +
  scale_colour_discrete(guide = FALSE) +
  facet_grid(age~.,  scales = "free_y")+
  scale_y_continuous(limits= c(0,225))+
  geom_point(colour= "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin=cv-se, ymax=cv+se),width=.2) +
  theme_cust + #theme(axis.text.y=element_blank())+
  ggtitle("Rotten coarse woody surface fuels")+
  xlab("")+ylab("")+
  geom_text(aes(x, y, label=lab),
            data=data.frame(x=2.75, y=215, lab=c("(l) F(5,17) = 2.12, p = 0.113",
                                                 "(p) F(5,17) = 1.64, p = 0.2778"),age=c("Old", "Young")), 
            family= "Times", size = 3, vjust=1)

grid.arrange(p.ls,p.ms,p.us, p.wc,p.ld,p.ff,p.sc,p.rc, nrow=2, 
             bottom = textGrob("Year of spruce beetle outbreak",
                               hjust = 0.35, vjust=0,
                               gp = gpar(fontsize = 12, fontfamily = "Times")))

dev.off()