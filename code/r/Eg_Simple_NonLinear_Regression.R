# simple non linear regression
planets = read.csv("kepler_planets.csv")
planets$dist = planets$dist/149.6
planets$period = planets$period/365.26
planets

# convert units to earth orbits
#  units of distance will become AUs,
# and the units of period will become earth years.
planets$dist = planets$dist/149.6
planets$period = planets$period/365.26
planets

with(planets, scatter.smooth(period ~ dist, span=7/8, pch=16, cex=.6, las=1))

# In the scatter.smooth() function, "span=" sets a loess smoothing parameter, "pch=" sets the point character for the plot (16 = filled circles), "cex=" sets the size of the points (6/10ths of default), and "las=1" turns the numbers on the axis to a horizontal orientation. We can see that the loess line is a bit lumpy, as loess lines often are, but it is definitely not linear.

# An accelerating curve such as this one suggests either an exponential relationship or a power relationship. To get the first, we would log transform the response variable. To get the second, we would log transform both variables. Let's try that and see if either one gives us a linear relationship.
with(planets, scatter.smooth(log(period) ~ dist))
title(main="exponential")
with(planets, scatter.smooth(log(period) ~ log(dist)))
title(main="power")

# I'm gonna go out on a limb here and guess that the exponential relationship is not the one we want. The shape of that curve suggests that we need an additional log transform of the explanatory variable, which is what we have with the power function. The power relationship looks pretty much linear. Let's go with that.
lm.out = lm(log(period) ~ log(dist), data=planets)
summary(lm.out)
# The residuals are virtually zero, as is the intercept, and R-squared is 1.
# I'm guessing we've nailed this
plot(lm.out, 1)
# inf = log(period) = 1.5 log(dist)


## Non monotonic example ####
# The study examined the effect of human disturbance on the nesting of house sparrows (Passer domesticus). Breeding pairs of birds ("pairs") per hectare were counted in 23 parks in Madrid, Spain. They also counted the number of people walking through the park per hectare per minute ("pedestrians"). The relationship is nonlinear and nonmonotonic, as shown by a scatterplot. (The black line is the lowess line. The red line will be explained below.)

sparrows = read.csv("sparrows_pedestrains.csv")
# We will model this relationship with a polynomial regression equation
lm.out = lm(pairs ~ pedestrians + I(pedestrians^2) + I(pedestrians^3), data=sparrows)
summary(lm.out)
## inf = pairs-hat = -91.2 + 49.7 * pedestrians - 2.83 * pedestrians^2 + 0.0426 * pedestrians^3
curve(-91.2 + 49.7*x - 2.83*x^2 + 0.0426*x^3, add=T, col="red")
# inf: Notice that the regression line picks up that last point better than the lowess line does. And therein lies a problem.

plot(lm.out, 4)                 # Cook's Distance plot
lm.out2 = update(lm.out, data=sparrows[-23,])       # delete case 23
summary(lm.out2)
with(data=sparrows[-23,], scatter.smooth(pairs ~ pedestrians))
curve(-105.19464 + 54.09956*x + -3.18069*x^2 + 0.05024*x^3, add=T, col="red")
## inf = They reasoned that, at first, as human activity increases, nesting activity also increases, because humans mean food. But when human activity gets too high, nesting activity is disrupted.

## moving on to more interesting stuff
#  vapor pressure of mercury and its relationship to temperature has been an important question
data(pressure)
str(pressure)
# convert to SI Units
pressure$temperature = pressure$temperature + 273.15
pressure$pressure = pressure$pressure * .1333  # kilo pascals
summary(pressure)
# rename the vars
pres = pressure$pressure
temp = pressure$temperature
ls()

# Log Transformations
par(mfrow=c(1,4))                    # one row of four graphs
plot(pres ~ temp, main="Vapor Pressure\nof Mercury",
                         xlab="Temperature (degrees Kelvin)",
                          ylab="Pressure (kPascals)")

# make R plot with a log y axis
plot(pres ~ temp, main="Vapor Pressure\nof Mercury",
                        xlab="Temperature (degrees Kelvin)",
                          ylab="Pressure (kPascals)", log="y")

# appears to have overcorrected. So let's try a power function (log(y) and log(x) transforms).
plot(pres ~ temp, main="Vapor Pressure\nof Mercury",
                  xlab="Temperature (degrees Kelvin)",
                  ylab="Pressure (kPascals)", log="xy")

# lets try log x transform for shuts and giggles
plot(pres ~ temp, main="Vapor Pressure\nof Mercury",
                       xlab="Temperature (degrees Kelvin)",
                          ylab="Pressure (kPascals)", log="x")

## have to use polynomial regression
# 3rd order
lm.out3 = lm(pres ~ temp + I(temp^2) + I(temp^3))
summary(lm.out3)

# The squared and cubed terms MUST be entered using "I", which in a model formula means "as is." This is because the caret symbol inside a formula does not have its usual mathematical meaning. If you want it to be interpreted mathematically, which we do here, it must be protected by "I", as in the term "I(temp^2)". This gives the same result as if we had created a new variable, tempsquared=temp^2, and entered tempsquared into the regression formula.

par(mfrow=c(1,1))                    # unnecessary if this has already been reset
plot(lm.out3$fitted, lm.out3$resid)  ### output not shown

# inf =  the residual plot shows we do not have the right model. To see why, do this.
plot(pres~temp)
curve(-475.8 + 3.804*x - .009912*x^2 + .00000844*x^3, add=T)

# This plot is shown above right, and as you can see, the cubic regression equation has distinct inflection points. Our mercury vapor data do not. Increasing the order of the polynomial is a straighforward extension of the above, and it will improve the fit, but it will not result in the correct model. (A fourth order polynomial would be a reasonable try.)


# Box-Cox transformations allow us to find an optimum transformation of the response variable using maximum-likelihood methods.

library("MASS")
par(mfrow=c(1,2))
boxcox(pres ~ I(1/temp))                            # basic box-cox plot
boxcox(pres ~ I(1/temp), lambda=seq(-.2,.2,.01))    # finer resolution on lambda
par(mfrow=c(1,1))
# inference
# The plots suggest we adopt a lambda value very close to zero, which is to say, we should use a log transform of the response variable.
plot(log(pres) ~ I(1/temp))
lm.out4 = lm(log(pres) ~ I(1/temp))
summary(lm.out4)

par(mfrow=c(2,2))
plot(lm.out4)
par(mfrow=c(1,1))

newPress = pressure[-4,]
lm.out5 = lm(log(newPress$pres) ~ I(1/newPress$temp))
summary(lm.out5)
par(mfrow=c(2,2))
plot(lm.out5)
par(mfrow=c(1,1))

with(data=newPress, scatter.smooth(pressure~ temperature))
curve(16.29 - 7322/x, add=T, col="red")

## making predictions from the model
new.temps = seq(from=400, to=600, by=50)
predict(lm.out5, list(temp=new.temps))
# transform back
exp(predict(lm.out5, list(temp=new.temps)))
