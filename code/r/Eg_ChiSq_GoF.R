## chi-sq g.o.fit (vector) or indepdendence (table)

# How to use this test:
# For the goodness of fit test, set "p" equal to the null hypothesized prop
# or probabilies for each of the categories represented in the vector "x". 

# Example 1
# there are 25 freshman in the sample, 32 sophomores, 18 juniors, and 20 seniors 
# H0: freshman, sophomores, juniors, and seniors are equally represented 
print(chisq.test(c(25,32,18,20)))
## cannot reject H0

# another H0 : the number of freshman and sophomores enrolled is each twice 
# the number of juniors and the number of seniors
null.probs = c(1/3,1/3,1/6,1/6)
freqs = c(25,32,18,20)
results = chisq.test(freqs, p=null.probs)
print(results)
# inf: It doesn't appear that we can reject 'null hypothesis either.

print(results$expected) # expect. freq under HO
print(results$residuals) # see where biggest deviations from H0 are predicted

## Eg 2
# Last year:  30 freshman, 28 sophomores, 28 juniors, and 11 seniors.
# do last yrs results match this years?
# H0: They're in the same proportion
new.freqs = c(30,28,28,11)
old.freqs = freqs # this year as in eg.1 above
print(chisq.test(new.freqs, p=old.freqs/sum(old.freqs)))
# inference = sig. different!

## working with tables
print(dimnames(HairEyeColor))
eyes = margin.table(HairEyeColor, 2) 
# H0: 50% have brown eyes, 25% blue eyes, 15% hazel eyes, and 10% green eyes.
print(chisq.test(eyes, p=c(.5, .25, .15, .1)))
# inference = H0 is not supported


## working with a data frame
data(survey, package="MASS")
print(str(survey))
smokers = table(survey$Smoke)
print(smokers)
# Ho: null hypothesis that 70% are nonsmokers, 
# and that the other 30% are divided equally among the remaining categories
results = chisq.test(smokers, p=c(.1, .7, .1, .1))
print(results)
print(results$residuals)
print(results$expected)
# inference
# reject H0
# we have very much overestimated the percentage of "Heavy" smokers






















