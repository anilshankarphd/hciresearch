# Logistics regression
data("menarche")
summary(menarche)
plot(Menarche/Total ~ Age, data=menarche)
glm.out = glm(cbind(Menarche, Total-Menarche) ~ Age, family=binomial(logit),
              data=menarche)

# our data frame does not contain a row for every case (i.e., every girl upon whom data were collected). Therefore, we do not have a binary (0,1) coded response variable. No problem! If we feed glm() a table (or matrix) in which the first column is number of successes and the second column is number of failures, R will take care of the coding for us. In the above analysis, we made that table on the fly inside the model formula by binding "Menarche" and "Total − Menarche" into the columns of a table (matrix) using cbind().
plot(Menarche/Total ~ Age, data=menarche)
lines(menarche$Age, glm.out$fitted, type="l", col="red")
title(main="Menarche Data with Fitted Logistic Regression Line")

summary(glm.out)

# response variable is log odds, so the coefficient of "Age" can be interpreted as "for every one year increase in age the odds of having reached menarche increase by exp(1.632) = 5.11 times."

# To evaluate the overall performance of the model, look at the null deviance and residual deviance near the bottom of the print out. Null deviance shows how well the response is predicted by a model with nothing but an intercept (grand mean). This is essentially a chi square value on 24 degrees of freedom, and indicates very little fit (a highly significant difference between fitted values and observed values). Adding in our predictors--just "Age" in this case--decreased the deviance by 3667 points on 1 degree of freedom. Again, this is interpreted as a chi square value and indicates a highly significant decrease in deviance. The residual deviance is 26.7 on 23 degrees of freedom. We use this to test the overall fit of the model by once again treating this as a chi square value. A chi square of 26.7 on 23 degrees of freedom yields a p-value of 0.269. The null hypothesis (i.e., the model) is not rejected. The fitted values are not significantly different from the observed values.

### Eg 2
# Multiple Numerical Predictors
# IB could be predicted from performance on the Stroop Color Word test. This test produces three scores: "W" (word alone, i.e., a score derived from reading a list of color words such as red, green, black), "C" (color alone, in which a score is derived from naming the color in which a series of Xs are printed), and "CW" (the Stroop task, in which a score is derived from the subject's attempt to name the color in which a color word is printed when the word and the color do not agree)

cor(gorilla)
with(gorilla, tapply(W, seen, mean))
with(gorilla, tapply(C, seen, mean))
with(gorilla, tapply(CW, seen, mean))
glm.out = glm(seen ~ W * C * CW, family=binomial(logit), data=gorilla)
summary(glm.out)

anova(glm.out, test="Chisq")
plot(glm.out$fitted)
abline(v=30.5,col="red")
abline(h=.3,col="green")
abline(h=.5,col="green")
text(15,.9,"seen = 0")
text(40,.9,"seen = 1")

# Categorical Predictors
ftable(UCBAdmissions, col.vars="Admit")
margin.table(UCBAdmissions, c(2,1))
### begin copying here
ucb.df = data.frame(gender=rep(c("Male","Female"),c(6,6)),
                    dept=rep(LETTERS[1:6],2),
                    yes=c(512,353,120,138,53,22,89,17,202,131,94,24),
                    no=c(313,207,205,279,138,351,19,8,391,244,299,317))
### end copying here and paste into the R Console
# we do not have a binary coded response variable
mod.form = "cbind(yes,no) ~ gender * dept"
glm.out = glm(mod.form, family=binomial(logit), data=ucb.df)
anova(glm.out, test="Chisq")
summary(glm.out)
exp(-1.0521)                         # antilog of the genderMale coefficient
1/exp(-1.0521)
# men were actually at a significant disadvantage when department and the interaction are controlled.
# The odds of a male being admitted were only 0.35 times the odds of a female being admitted. The reciprocal of this turns it on its head. All else being equal, the odds of female being admitted were 2.86 times the odds of a male being admitted.

# Each coefficient compares the corresponding predictor to the base level
exp(-2.2046) # dept C: 1/9 odds
# compare, for example, department C to department D
exp(-2.2046) / exp(-2.1662)
