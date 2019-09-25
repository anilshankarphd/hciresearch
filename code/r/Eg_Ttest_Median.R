# t-test median example
## if t value =  0.8848029, dof=66
## what is the p-value 2 sided?
# ans: pt(0.8848029, df=66, lower=F) * 2

data(anorexia, package="MASS")
attach(anorexia)
print(str(anorexia))
weight.gain.CBT = Postwt[Treat=="CBT"] - Prewt[Treat=="CBT"]
boxplot(weight.gain.CBT)

#HO: cognitive behavior therapy produces no change in median body weight
# when used in the treatment of anorexia.
print(weight.gain.CBT)
print(length(weight.gain.CBT))
print(sum(weight.gain.CBT > 0))

### There are 29 cases in the data set, of whom 18 showed a gain.
## The null implies that as many women should lose weight as gain if CBT is valueless.
### I.e., the null implies a median weight change of zero.
binom.test(x=18, n=29, p=1/2, alternative="greater")


## indep samples
nonsmokers = c(18,22,21,17,20,17,23,20,22,21)
smokers = c(16,20,14,21,20,18,13,15,17,21)
scores = c(nonsmokers, smokers)
groups = c("nonsmokers","smokers")
groups = rep(groups, times=c(10,10))      # 10 nonsmokers, 10 smokers
mj.data = data.frame(groups, scores)
print(t.test(scores ~ groups))

# calculate t when summary stats are given
# smokers     nonsmokers
# ----------------------
#   mean    17.5         20.1
# sd       2.95         2.13

mean.diff = 17.5 - 20.1
df = 10 + 10 - 2
pooled.var = (2.95^2 * 9 + 2.13^2 * 9) / df
se.diff = sqrt(pooled.var/10 + pooled.var/10)
t.obt = mean.diff / se.diff
print(t.obt)
p.value = 2 * pt(abs(t.obt), df=df, lower.tail=F)        # two-tailed
print(p.value)

# alternative
print(wilcox.test(nonsmokers, smokers))

## related measures
ft = subset(anorexia, subset=(Treat=="FT"))
# with(ft, t.test(Postwt-Prewt, mu=0, alternative="greater"))
with(ft, t.test(Postwt, Prewt, paired=T, alternative="greater"))

## example of how real world data is coded (by subject in the test)
ft$Treat = NULL                      # delete unnecessary columns
ft.long = stack(ft)
ft.long$subjects = rep(LETTERS[1:17],2)
ft.long = ft.long[order(ft.long$subjects),]
print(ft.long)

qqnorm(ft$Postwt - ft$Prewt)                   # output not shown
qqline(ft$Postwt - ft$Prewt)
plot(ft$Prewt, ft$Postwt)
plot(ft$Prewt, Change <- ft$Postwt-ft$Prewt)   # cannot use = here!
# We have a problem with normality in a small data set (n=17).
# We also have a problem with nonresponders (four women whose body weight didn't
# change or actually fell during therapy).
# And even among those who did change, the effect was not additive.
# Women who started off with lower body weights tended to change the most.
# These are all violations of the assumptions of the test.

t.test(values ~ ind, paired=T, alternative="less", data=ft.long)
