## simple linear correlation and regerssion
library("MASS")
data(cats)
str(cats)

# look at the data
with(cats, plot(Bwt, Hwt))
title(main="Heart Weight (g) vs. Body Weight (kg)\nof Domestic Cats")

# another way
with(cats, plot(Hwt ~ Bwt))     # or...
plot(Hwt ~ Bwt, data=cats)

# Pearson's r
with(cats, cor(Bwt, Hwt))
with(cats, cor.test(Bwt, Hwt))

# we expect a +ve corr, see plot, so
with(cats, cor.test(Bwt, Hwt, alternative="greater", conf.level=.8))

## using the fomrmula, you can subset
cor.test(~ Bwt + Hwt, data=cats)
cor.test(~Bwt + Hwt, data=cats, subset=(Sex=="F"))

# better way to plot
with(cats, plot(Bwt, Hwt, type="n", las=1, xlab="Body Weight in kg",
                                 ylab="Heart Weight in g",
                                 main="Heart Weight vs. Body Weight of Cats"))
with(cats, points(Bwt[Sex=="F"], Hwt[Sex=="F"], pch=16, col="red"))
with(cats, points(Bwt[Sex=="M"], Hwt[Sex=="M"], pch=17, col="blue"))

# remove cats
rm(cats)

# numeric values
# Correlation and Covariance Matrices
data(cement)                         # also in the MASS library
str(cement)
cor(cement) # fails if NAs
cov(cement)

cov.matr = cov(cement)
cov2cor(cov.matr)
pairs(cement)

# Correlations for Ranked Data
# If the data are ordinal rather than true numerical measures, or have been converted to ranks to fix some problem with distribution or curvilinearity, then R can calculate a Spearman rho coefficient or a Kendall tau coefficient.
coach1 = c(1,2,3,4,5,6,7,8,9,10)
coach2 = c(4,8,1,5,9,2,10,7,3,6)
cor(coach1, coach2, method="spearman")
cor(coach1, coach2, method="kendall")
cor.test(coach1, coach2, method="kendall")

# To get cor() and cov() to work with missing values, you set the "use=" option. Possible values are "everything", "all.obs" (not sure what the difference is there), "complete.obs" (deletes cases with any missing values from the computation even if there are no missings in the two variables that are currently being computed with), "na.or.complete" (pretty much the same), and "pairwise.complete.obs" (all complete pairs of the two variables currently being considered are used).
with(data=cats, cor(Bwt, Hwt, use="pairwise"))

# simple regression
lm.out = lm(Hwt ~ Bwt, data=cats)
summary(lm.out)
# inf = Hwt-hat = 4.0341 (Bwt) - 0.3567
# use: options(show.signif.stars=F) if needed
anova(lm.out)
plot(Hwt ~ Bwt, data=cats, main="Kitty Cat Plot")
abline(lm.out, col="red")
par(mfrow=c(2,2))          # partition the graphics device
plot(lm.out)

# The first plot is a standard residual plot showing residuals against fitted values.
# Points that tend towards being outliers are labeled. If any pattern is apparent in the points on this plot, then the linear model may not be the appropriate one.
#$ The second plot is a normal quantile plot of the residuals. We like to see our residuals normally distributed. Don't we? T
#$ he last plot shows residuals vs. leverage. Labeled points on this plot represent cases we may want to investigate as possibly having undue influence on the regression relationship.
# Case 144 is one perhaps worth taking a look at.
cats[144,]
lm.out$fitted[144]
lm.out$residuals[144]

# This cat had the largest body weight and the largest heart weight in the data set. (See the summary table at the top of this tutorial.) The observed value of "Hwt" was 20.5g, but the fitted value was only 15.4g, giving a residual of 5.1g. The residual standard error (from the model output above) was 1.452, so converting this to a standardized residual gives 5.124/1.452 = 3.53, a substantial value to say the least! A commonly used measure of influence is Cook's Distance, which can be visualized for all the cases in the model as follows.
par(mfrow=c(1,1))
plot(cooks.distance(lm.out))

# how to address this?
lm.without144 = lm(Hwt ~ Bwt, data=cats, subset=(Hwt<20.5))
lm.without144

# s robust in the face of outlying points.
rlm(Hwt ~ Bwt, data=cats)

# Lowess stands for locally weighted scatterplot smoothing.
plot(Hwt ~ Bwt, data=cats)
lines(lowess(cats$Hwt ~ cats$Bwt), col="red")
# another approach
scatter.smooth(cats$Hwt ~ cats$Bwt)       # same programming flaw here
scatter.smooth(cats$Hwt ~ cats$Bwt, pch=16, cex=.6)

# inference: The plots indicate we were on the right track with our linear model.





