#   multiple regression
cars = mtcars[,c(1,3,4,6)]
summary(cars)
cor(cars)
pairs(cars, cex=.6)
lm.out = lm(mpg ~ disp * hp * wt, data=cars)
coef(lm.out)          # not shown
fitted(lm.out)        # not shown
residuals(lm.out)     # not shown
summary(lm.out)

library("car")
vif(lm.out)
# The function vif() calculates variance inflation factors. Collinearity produces variance inflation that affects the error terms for the coefficient tests. The variance inflation factors tell us by how much. An ideal variance inflation factor is 1 (no inflation). Anything greater than 10 is considered to be high.
lm.out = lm(mpg ~ (disp + hp + wt)^2, data=cars)
summary(lm.out)
vif(lm.out)

# Let's fix the "meaningless simple effects" problem by making 0 a meaningful value for each of the predictors. We do that by a procedure called mean centering.
cars$disp.c = cars$disp - mean(cars$disp)
cars$hp.c = cars$hp - mean(cars$hp)
cars$wt.c = cars$wt - mean(cars$wt)
summary(cars)
lm.out = lm(mpg ~ (disp.c + hp.c + wt.c)^2, data=cars)
vif(lm.out)

lm.add = lm(mpg ~ (disp.c + hp.c + wt.c), data=cars)
anova(lm.add, lm.out)
# Throwing out the interactions would significantly change the model, and since deleting terms never makes the model better, that means the model without the interactions is significantly worse. Somewhere among those interactions is something useful.

summary(lm.out)

# model redution
lm.step = step(lm.out, trace=0)
# Now I'm going to toss the least significant interaction.
lm.out = lm(mpg ~ disp.c + hp.c + wt.c + disp.c:wt.c + hp.c:wt.c, data=cars)
summary(lm.out)
# , disp.c:wt.c goes next.
lm.out = lm(mpg ~ disp.c + hp.c + wt.c + hp.c:wt.c, data=cars)
summary(lm.out)
# remove one more term
lm.out = lm(mpg ~ hp.c + wt.c + hp.c:wt.c, data=cars)
summary(lm.out)

# test vs. computer
lm.step
# Of the three variables we started with, we have retained horsepower and weight and their mutual interaction. Let's see how we're doing with variance inflation.
vif(lm.out)
par(mfrow=c(2,2))
plot(lm.out, c(1,2,4,5))
# decent fit
confint(lm.out, level=.99)

# look at interactions
coplot(mpg ~ hp | wt, show.given = F,     # formula: mpg by hp given wt
                data=cars,          # where the data are (the function can be ended here)
                 rows=1,             # number of rows to put the graphs in
                 number=3,           # number of graphs to plot
                 overlap=0,
       panel=panel.smooth,
       )          # don't overlap values of wt across graphs
# The top panel shows the ranges of "wt" that are covered in each of the graphs. The bottom panel shows a scatterplot of "mpg" by "hp" within each of those ranges. The only other option I might have set here was "panel=panel.smooth", which would have plotted a smoothed regression line on each of the plots. And possibly "show.given=F", which would turn off the top panel of the graphic. You can play with the options to see how they alter the graph. The only necessary parts are the formula and the data= argument.
#
# The coplot is telling us that, at low values of vehicle weight, the relationship between mpg and hp is very steep. That is, gas mileage drops off rapidly as horsepower increases. At moderate values of vehicle weight the relationship is less steep but still strongly negative. Finally, at high vehicle weights, increasing horsepower has much less effect but still appears to be slightly negative.
