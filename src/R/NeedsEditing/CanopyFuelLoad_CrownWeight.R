library(reshape2) # for melt
library(ggplot2)
library(lattice)

Melt.CW <- read.csv("~/Dropbox/PhD Program/FieldSeason_2013/2013_Datasheets/FuelsSites_2013/Fuel_Output_Sheets/SFuels_R/Melt.CW.csv")
melt.sum <- aggregate(cbind(LCW) ~ Year + +Age + LD, data=Melt.CW, FUN=sum)

q <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),panel.background = element_rect(fill = 'white', colour = 'black'))
t <-theme(plot.title=element_text(family="Times New Roman"), axis.title.x=element_text(family="Times New Roman"),
          axis.title.y=element_text(family="Times New Roman"), axis.text.x=element_text(family="Times New Roman"),
          axis.text.y=element_text(family="Times New Roman"), legend.text=element_text(family="Times New Roman"),
          strip.text= element_text(family="Times New Roman"),legend.title=element_blank(),panel.grid=element_blank())
t2 <-theme(axis.title.x=element_text(family="Times New Roman"),
           axis.text.x=element_text(family="Times New Roman"),panel.grid=element_blank())
t3 <- theme(axis.title.x=element_text(family="Times New Roman"),
            axis.text.x=element_text(family="Times New Roman"),
            legend.text=element_text(family="Times New Roman"),legend.title=element_blank(),panel.grid=element_blank())

cairo_pdf(file="CrownWeight.pdf", width = 10, height = 6)
par(mfrow = c(1,2))
op <- par(family = "Times")
p1 <- ggplot(subset(melt.sum, Year == 1940), 
             aes(x = Age, y = LCW, fill = LD,width=0.5)) +
  labels(y="Crown Weight")+
  theme_bw() +
  geom_histogram(stat = "identity", position = "stack", colour="black") +
  xlab("1940") + scale_fill_manual(values=c("#999999", "#FFFFFF"))+
  theme(panel.border = element_blank())+
  guides(fill=FALSE)+t

p2 <- ggplot(subset(melt.sum, Year == 1960), 
             aes(x = Age, y = LCW, fill = LD,width=0.5)) +
  theme_bw() +
  geom_histogram(stat = "identity", position = "stack", colour="black") +
  xlab("1940") + scale_fill_manual(values=c("#999999", "#FFFFFF"))+
  xlab("1960") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank())+
  guides(fill=FALSE)+t2

p3 <- ggplot(subset(melt.sum, Year == 1990),
             aes(x = Age, y = LCW, fill = LD)) +
  theme_bw() +
  geom_histogram(stat = "identity", position = "stack", colour="black") +
  xlab("1940") + scale_fill_manual(values=c("#999999", "#FFFFFF"))+
  xlab("1990") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank())+
  guides(fill=FALSE)+t2
p4 <- ggplot(subset(melt.sum, Year == 2000), 
             aes(x = Age, y = LCW, fill = LD)) +
  theme_bw() +
  geom_histogram(stat = "identity", position = "stack", colour="black") +
  xlab("1940") + scale_fill_manual(values=c("#999999", "#FFFFFF"))+
  xlab("2000") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank())+
  guides(fill=FALSE)+t2
p5 <- ggplot(subset(melt.sum, Year == 2010), 
             aes(x = Age, y = LCW, fill = LD)) +
  geom_histogram(stat = "identity", position = "stack", colour="black") +
  xlab("1940") + scale_fill_manual(values=c("#999999", "#FFFFFF"))+
  theme_bw() +xlab("2010") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank())+
  guides(fill=FALSE)+t2

p6 <- ggplot(subset(melt.sum, Year == "Endemic"),
             aes(x = Age, y = LCW, fill = LD)) +
  theme_bw() +
  geom_histogram(stat = "identity", position = "stack", colour="black") +
  xlab("1940") + scale_fill_manual(values=c("#999999", "#FFFFFF"))+
  labs(x = "Endemic", y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.85, 0.9),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.2)),
        panel.border = element_blank())+t3
par(op)
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 1)

par(op)
dev.off()


