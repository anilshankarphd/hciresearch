## multiple regression
st = as.data.frame(state.x77)

# add pop. density
colnames(st)[4] = "Life.Exp"                   # no spaces in variable names, please
colnames(st)[6] = "HS.Grad"                    # ditto
st$Density = st$Population * 1000 / st$Area

# quick summary
round(cor(st), 3)
pairs(st)

# inference =
# Life expectancy does have a bivariate relationship with a lot of the other variables, but many of those variables are also related to each other. Multiple regression allows us to tease all of this apart and see in a "purer" (but not pure) form what the relationship really is between, say, life expectancy and average number of days with frost.
par(mfrow=c(3,3))
for(i in 1:9) qqnorm(st[,i], main=colnames(st)[i])

# start modeling
model1 = lm(Life.Exp ~ ., data=st)
summary(model1)
summary.aov(model1)

# min. adequate model
model2 = update(model1, .~. -Area)
summary(model2)
# compare the two models
anova(model2, model1)

model3 = update(model2, .~. -Illiteracy)
summary(model3)

# Looks like "Income" should go next, but gosh darn it, how can income not be related to life expectancy? Well, you have to be careful how you say that. The model is NOT telling us that income is unrelated to life expectancy! It's telling us that, within the parameters of this model, "Income" is now the worst predictor. A look back at the correlation matrix shows that "Income" is strongly correlated with "HS.Grad". Maybe "HS.Grad" is stealing some its thunder. On the other hand, income is important because it can buy you things, like a house in a neighborhood with good schools and low murder rate, and in the "burbs" (lower population density). Sorry "Income" but I gotta give you the ax. (NOTE: The important thing is, I thought about it and came to this conclusion rationally. I didn't blindly let the numbers tell me what to do--entirely.)

model4 = update(model3, .~. -Income)
summary(model4)

model5 = update(model4, .~. -Density)
summary(model5)

anova(model5, model4)
anova(model5, model1)

# test everything up to the three-way interactions
model.int = update(model5, .~.^3)
anova(model5, model.int)

#  the interactions do not make a significant contribution to the model. So I'm not going to worry about them further.

## stepwise regression
# loewr values of AIC are better

# let's go with model5
confint(model5)

# predict
predict(model5, list(Population=2000, Murder=10.5, HS.Grad=48, Frost=100))

# regression diagnostics
par(mfrow=c(2,2))                    # visualize four graphs at once
plot(model5)
par(mfrow=c(1,1))


plot(model5, 1)            # syntax: plot(model.object.name, 1)
plot(model5, 2)
plot(model5, 3)
plot(model5, 4)            # syntax: plot(model.object.name, 1)
plot(model5, 5)

# Extracting Elements of the Model Object
names(model5)
model5$coefficients
sort(model5$resid)


# Beta Coefficients
# Beta or standardized coefficients are the slopes we would get if all the variables were on the same scale, which is done by converting them to z-scores before doing the regression. Betas allow a comparison of the approximate relative importance of the predictors, which neither the unstandardized coefficients nor the p-values do. Scaling, or standardizing, the data vectors can be done using the scale() function. Once the scaled variables are created, the regression is redone using them. The resulting coefficients are the beta coefficients.
#
# That sounds like a lot of fuss and bother, so here is the way I do it. It's based on the fact that a coefficient times the standard deviation of that variable divided by the standard deviation of the response variable gives the beta coefficient for the variable

sdexpl = c(sd(st$Population), sd(st$Murder), sd(st$HS.Grad), sd(st$Frost))
sdresp = sd(st$Life.Exp)
coef(model5)[2:5] * sdexpl / sdresp

##INFERENCE
# So an increase of one standard deviation in the murder rate is associated with a drop of 0.825 standard deviations in life expectancy, if all the other variables are held constant. VERY roughly and approximately, these values can be interpreted as correlations with the other predictors controlled. We can see that HS.Grad and Frost are of roughly the same importance in predicting Life.Exp. Population is about right in there with them, or maybe a little less. Clearly, in our model, Murder is the most important predictor of Life.Exp.

# rm(list=ls())

