
# Make the Year variables factors - this will exclude them from any analysis
SF$Year <- as.factor(SF$Year)

# If we counts (i.e., 1-hr fuels) they will not be normally distributed, therefore 
# we have to transform them to create an approximate normal distribution, we can use the 
# square root transformation 
# The transformation below takes the sqrt of the Variable and reassigns it to the new
# dummy variable Variable.transform
# SF$Variable.transform <- sqrt(SF$Variable + 3/8)
# mod.1d <- aov(Variable.transform ~ Year, data=SF)

# Producing an ANOVA linear model
mod.1 <- aov(Duff ~ Year, data = SF)

