# Making predictions from a MReg model
rm(list=ls())                        # clean up (WARNING! this will wipe your workspace
data(airquality)                     # see ?airquality for details on these data
na.omit(airquality) -> airquality    # toss cases with missing values
lm.out = lm(Ozone ~ Solar.R + Wind + Temp + Month, data=airquality)
coef(lm.out)
confint(lm.out)

# predict for new values: Solar.R=200, Wind=11, Temp=80, Month=6
(prediction = c(1, 200, 11, 80, 6) * coef(lm.out))
sum(prediction)

# How accurate is it?
#  Is the prediction being made for the mean response for cases like this? Or is it being made for a new single case? The difference this makes to the CI is shown below.

### Prediction of mean response for cases like this...
predict(lm.out, list(Solar.R=200, Wind=11, Temp=80, Month=6), interval="conf")

## Prediction for a single new case...
predict(lm.out, list(Solar.R=200, Wind=11, Temp=80, Month=6), interval="pred")
#  CI is much wider in the second case

# when we did confint(lm.out) above, what are those confidence limits? Are they for predicting the mean response for cases like this? Or for predicting for a single new case?
intervals = confint(lm.out)
t(intervals)
values = c(1,200,11,80,6)
sum(colMeans(t(intervals)) * values)
t(intervals) %*% matrix(values)
# inference: Neither!

