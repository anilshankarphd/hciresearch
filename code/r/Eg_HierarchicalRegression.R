# hierarchical regression
# National Educational Longitudinal Study.
# NELS was a study begun in 1988, when a large, nationally representative sample of 8th graders was tested, and then retested again in 10th and 12th grades.

nels = read.csv("Keith.csv")
summary(nels)

grades1 = nels[,c("f1txhstd","byses","bygrads","f1cncpt2","f1locus2")]
summary(grades1)
rm(nels)
apply(grades1, 2, mean, na.rm=T)
apply(grades1, 2, sd, na.rm=T)

sampsize = function(x)  sum(!is.na(x))

apply(grades1, 2, sampsize)
cor(grades1, use="pairwise.complete")
colnames(grades1) = c("std.achieve.score","SES","prev.grades","self.esteem","locus.of.control")
head(grades1)

# DV~., which means DV as a function of all other variables
lm.out = lm(std.achieve.score ~ ., data=grades1)
summary(lm.out)

confint(lm.out)
print(anova(lm.out), digits=7)

# get beta coeffs
grades3 = scale(grades1)
grades3 = as.data.frame(grades3)
coef(lm(std.achieve.score~SES+prev.grades+self.esteem+locus.of.control, data=grades3))

# note
# Standard regression enters all of the predictor variables at once into the regression, and each predictor is treated as if it were entered last. That is, each predictor variable in the regression table above is controlled for all of the other predictors in the table.
# Thus, we can say that the effect of SES when prev.grads, self.esteem, and locus.of.control are held constant or are held at their mean values.
# standard regression reveals direct effects only. Hierarchical regression reveals total effects, provided we have entered the predictors in the correct order.
print(anova(lm.out), digits=7)
