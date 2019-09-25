# between groups anova
# (one way anova)
# The data are from an agricultural experiment in which six different insect
# sprays were tested in many different fields, and the response variable (DV)
# was a count of insects found in each field after spraying.

# Traditional parametric analysis of variance makes the following assumptions:
#
# Random sampling, or at the very least, random assignment to groups.
# Independence of scores on the response variable -- i.e., what you get from one subject should be in no way influenced by what you get from any of the others.
# Sampling from normal populations within each cell of the design.
# Homogeneity of variance -- the populations within each cell of the design should all have the same variance.
# The first two of these are methodological issues and will not be discussed further. The last two are assumptions we need to look at statistically.

data(InsectSprays)
str(InsectSprays)
print(summary(InsectSprays))
attach(InsectSprays)
tapply(count, spray, mean)
tapply(count, spray, var)
tapply(count, spray, length)
boxplot(count~spray, data=InsectSprays)

# data violates anova assumptions
oneway.test(count~spray, data=InsectSprays)

# so lets fix this
# test the null hypothesis of equal group variances.
bartlett.test(count ~ spray, data=InsectSprays)
# levene's test
library(car)
leveneTest(y=InsectSprays$count, group=InsectSprays$spray) # def. median
leveneTest(y=InsectSprays$count, group=InsectSprays$spray, center=mean)#mean

## get over limitatinos of one.way.test
aov.out = aov(count ~ spray, data=InsectSprays)
summary(aov.out)
par(mfrow=c(1,2))          # set graphics window to plot side-by-side
plot(aov.out, 1)           # graphical test of homogeneity
plot(aov.out, 2)           # graphical test of normality
# close the graphics device to reset its parameters
## Inference
# The graph on the left shows the residuals (score - group mean) clearly getting larger as the group means get larger, a bad pattern, but not unusual when the DV results from counting something. The graph on the right shows that the residuals are not normally distributed, so the normality assumption is also violated.

## Post hoc test
TukeyHSD(aov.out)

# Power
meancounts = tapply(count, spray, mean)
var(meancounts) ## get between.var from here
summary(aov(count ~ spray)) ## with.var (Residuals, Mean Sq)
power.anova.test(groups=6, between.var=44.48, within.var=15.38, power=.9)

# non parametric test
kruskal.test(count ~ spray, data=InsectSprays)
# It performs best when the distributions all have the same shape, and when there is homogeneity of variance.



