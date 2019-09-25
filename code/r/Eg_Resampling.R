# resampling techniques

# The experiment is on the effect of a soporific drug. All subjects were measured during a baseline (no-drug) period and again during the drug trial, and the "extra" variable is how much additional sleep time they got over baseline during the drug trial.
# Group 1 is the control (placebo) condition
# Group 2 is the treatment (drug) condition.
data(sleep)
str(sleep)
attach(sleep)
tapply(extra, group, mean)
tapply(extra, group, sd)
tapply(extra, group, length)


sleep$group = factor(sleep$group, labels=c("control","treatment"))
summary(sleep)

with(sleep, tapply(extra, group, mean))
with(sleep, tapply(extra, group, sd))

t.test(extra ~ group, var.eq=T)
# not sig.
# How many subjects/group should we use to get a power of 80%?
# params from the output
power.t.test(n=NULL, delta=1.58, sd=1.9, sig.level=.05, power=.8)

# Bootstrapping The t-Test
with(sleep, t.test(extra ~ group, var.eq=T)$statistic)   # for reference

scores = sleep$extra                           # the data
R = 1000                                       # the number of replicates
t.star = numeric(R)                            # storage for the results
for (i in 1:R) {
    group1 = sample(scores, size=10, replace=T)
    group2 = sample(scores, size=10, replace=T)
    t.star[i] = t.test(group1, group2, var.eq=T)$statistic
}
hist(t.star, freq=F)
points(x=-1.8608, y=0, pch=16)
lines(x<-seq(from=-4, to=4, by=.1), y=dt(x, df=18), col="red")
quantile(t.star, c(.025,.05,.5,.950,.975))

# bootstrap CIs
data(crabs, package="MASS")
cara = crabs$CL[crabs$sp=="B"]
summary(cara)
qqnorm(cara)
t.test(cara)$conf.int

# The procedure is to take a large number of bootstrap samples and to examine the desired statistic.
R = 1000
boot.means = numeric(R)
for (i in 1:R) {
    boot.sample = sample(cara, 100, replace=T)
    boot.means[i] = mean(boot.sample)
 }
quantile(boot.means, c(.025,.975))
