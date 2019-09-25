# ancova
# example 1 Childhood Abuse and Adult Post-Traumatic Stress Disorder
library("faraway")
data(sexab)
summary(sexab)
# How are sexual and physical abuse that occur in childhood related to the severity of PTSD as an adult?
# How does the effect of physical abuse differ for women who were or were not sexually abused as children?
plot(ptsd ~ cpa, data=sexab, pch=as.numeric(csa), col=as.numeric(csa))
# check for interactions
lm.sexab1 = lm(ptsd ~ cpa + csa, data=sexab)
lm.sexab2 = lm(ptsd ~ cpa * csa, data=sexab)
anova(lm.sexab1, lm.sexab2)
# The F-value is near 1, so we can assume the absence of important interactions.
anova(lm.sexab1)                     # sequential tests
summary(lm.sexab1)
# inference
# We have an answer. The estimated coefficient for csaNotAbused is statistically significant. When we include it in the regression equation(s), it will result in different intercepts for the csaAbused and csaNotAbused lines. Furthermore, the difference in intercepts, 6.3 points on a scale with a range of scores of a little more than 22 points, is quite large. The regression equations are:
#   ptsd-hat(Abused) = 10.2480 + 0.5506 * cpa
# ptsd-hat(NotAbused) = 10.2480 + 0.5506 * cpa - 6.2728 = 3.9752 + 0.5506 * cpa
abline(a=10.25, b= 0.55, lty="dashed", col="black")
abline(a=3.98, b=0.55, lty="dotted", col="red")
## inference: It appears that the effects of physical and sexual abuse are additive.



cars = mtcars[,c(1,2,6)]
cars$cyl = factor(cars$cyl)
summary(cars)

par(mfrow=c(1,3))
boxplot(mpg ~ cyl, data=cars, xlab="cylinders", ylab="mpg")
boxplot(wt ~ cyl, data=cars, xlab="cylinders", ylab="weight")
plot(mpg ~ wt, data=cars)

lm.out1 = lm(mpg ~ cyl, data=cars)   # aov() works as lm() internally
anova(lm.out1)                       # same as summary.aov(lm.out1)
summary(lm.out1)

# No surprises in the ANOVA output. The interesting stuff is in the regression output. The (estimated) coefficients relate to the means of "cyl". The base, or reference, level of "cyl" is the one not listed with a coefficient in the regression output, or cyl4. The intercept (26.66) is the mean gas mileage for that level of "cyl". The coefficient for cyl6 is -6.92, which means that six cylinder cars get (got) 6.92 mpg less than four cylinder cars, on the average. Eight cylinder cars got 11.56 mpg less than four cylinder cars. The significance tests on these coefficients are equivalent to Fisher LSD tests on the differences in means.

with(cars, tapply(mpg, cyl, mean))

# The effect size is R2 = .7325. In the language of ANOVA, that statistic would be called eta-squared. That is the "proportion of explained variation" in mpg accounted for by the explanatory variable.
# inf = cylinders and all confounds from other variables that might be confounded with cylinders account for 73% of the variability in gas mileage.
# could weight be a confouding variable?
lm.out2 = lm(mpg ~ wt + cyl, data=cars)
summary(lm.out2)

plot(mpg ~ wt, data=cars, pch=as.numeric(cars$cyl), xlab="weight of car in 1000s of pounds")
abline(a=33.99, b=-3.21, lty="solid")
abline(a=33.99-4.26, b=-3.21, lty="dashed")
abline(a=33.99-6.07, b=-3.21, lty="dotted")
anova(lm.out2)

# Two things distress me here that I feel compelled to comment on. First, notice there is very little overlap on the scatterplot in the weight of 4- vs. 6- vs. 8-cylinder cars.
lm.out3 = lm(mpg ~ wt * cyl, data=cars)
anova(lm.out2, lm.out3)
