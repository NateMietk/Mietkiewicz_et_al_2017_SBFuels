libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales')
lapply(libs, require, character.only = T)
attach(SDLFL)

my.theme <- trellis.par.get()
my.theme$strip.background$col <- "grey80"
my.theme$plot.symbol$pch <- 16
my.theme$plot.symbol$col <- "grey60"
my.theme$plot.polygon$col <- "grey90"


### Basic historgram stratified by Year,
### Representated in a 2x3 graph matrix
hist.lattice <- histogram(~ Duff | Year,
                          as.table = TRUE,
                          par.settings = my.theme)

l.his <- hist.lattice

print(l.his)

### Density plots startified by Year and Stand Age
### Representated in a 6x2 graph matrix
dens.lattice <- densityplot(~ Duff | Year + Age,
                            as.table = TRUE,
                            par.settings = my.theme,
                            plot.points = FALSE,
                            between = list(x = 0.2, y = 0.2),
                            scales = list(x = list(rot = 45)))

l.den <- useOuterStrips(dens.lattice)

print(l.den)

### Binned historgrams per fuels constitent - NOT stratified
hist.ggplot <- ggplot(SDLFL, aes(x = Duff), binwidth = x)

g.his <- hist.ggplot +
  geom_histogram()

print(g.his)

### Binned historgrams per fuels constitent stratified by Year and Stand Age
### Representated in a 6x2 graph matrix
g.his <- hist.ggplot +
  geom_histogram(aes(y = ..ncount..)) +
  scale_y_continuous(labels = percent_format()) +
  facet_grid(Age ~ Year) + 
  ylab("Percent")

print(g.his)


###################################
errbar.ggplot.facets <- ggplot(SDLFL, aes(x = Year, y = Duff))

### function to calculate the standard error of the mean
se <- function(x) sd(x)/sqrt(length(x))

### function to be applied to each panel/facet
my.fun <- function(x) {
  data.frame(ymin = mean(x) - se(x), 
             ymax = mean(x) + se(x), 
             y = mean(x))
}

g.err.f <- errbar.ggplot.facets + 
  stat_summary(fun.y = mean, geom = "bar", 
               fill = "gray", border = "black") + 
  stat_summary(fun.data = my.fun, geom = "linerange") + 
  facet_wrap(~ Age) +
  theme_bw()

print(g.err.f)





errbar.ggplot.facets <- ggplot(SDLFL, aes(x = Year, y = Duff))

### function to calculate the standard error of the mean
se <- function(x) sd(x)/sqrt(length(x))

### function to be applied to each panel/facet
my.fun <- function(x) {
  data.frame(ymin = mean(x) - se(x), 
             ymax = mean(x) + se(x), 
             y = mean(x),
             size=0.75, 
             width=0.05)
}

g.err.f <- errbar.ggplot.facets + 
  stat_summary(fun.y = mean, geom = "bar", 
               fill = "gray", border = "black") + 
  stat_summary(fun.data = my.fun, geom = "linerange") + 
  theme_bw()

print(g.err.f)