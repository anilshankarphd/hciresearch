## Distributions
# The dnorm() function returns the height of the normal curve at some value along the x-axis
print(dnorm(1)) # height of std. normal curve at x = 1
print(pnorm(1))  # cdf, area under the curve
print(1 - pnorm(1)) # area to the right x=1, p-value?
print(pnorm(1, lower.tail = F))


# Quantiles and critical values
print(qnorm(.95)) # p = 0.05, 1-tailed, upper
print(qnorm(c(.025, .975))) # p= 0.05, two-tailed
print(qnorm(seq(from=.1,to=.9,by=.1)))      # deciles from the unit normal dist

# other distributions
print(pt(2.101, df=8))  # area below t = 2.101, df = 8)
print(qchisq(.95, df=1)) # critical value of chi square, df = 1
print(qf(c(.025,.975), df1=3, df2=12))  # quantiles from the F distribution
print(dbinom(60, size=100, prob=.5) # a discrete binomial probability
)

## generators
print(runif(9)) # 9 uniformly distributed random nos.
print(rnorm(9)) # 9 random nos
print(rt(9, df=10)) # t-distribution

# emprical quantiles
print(quantile(rivers))
print(quantile(rivers, probs=seq(.2,.8,.2))) # quintiles
print(quantile(rivers, probs=seq(.1,.9,.1))) # deciles
print( quantile(rivers, probs=.55)) # 55th percentile
print(quantile(rivers, probs=c(.05,.95)))

# not normal
qqnorm(rivers)
qqline(rivers)
plot(density(rivers))
print(shapiro.test(rivers)) # H0: = Not normal
print(shapiro.test(rnorm(100))) # Ho: = Not normal, reject

# another test
print(ks.test(rivers, "pnorm", alternative="two.sided"))

