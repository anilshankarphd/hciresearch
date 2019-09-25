## counts and proprotitons 
# print(Seatbelts)

deaths = as.vector(Seatbelts[,1])
print(summary(deaths))
print(length(deaths))
hist(deaths, breaks=15, prob=T, ylim=c(0,.04))
lines(x<-seq(65,195,10),dpois(x,lambda=mean(deaths)),lty=2,col="red")
# empirical distribution doesn't match theoretical poisson

# lets create one
sim.dist = rpois(192, lambda=mean(deaths))
qqplot(deaths, sim.dist, main="Q-Q Plot") # compare shapes of 2 distrib
abline(a=0, b=1, lty=2, col="red")
# inference : two distributions should not only both be poisson, they should 
# also both have the same mean (or lambda value). Thus, the points should fall 
# along a line with intercept 0 and slope 1. 

# has an annual cycle
plot(Seatbelts[,1]) 
scatter.smooth(1:192, deaths) # prob. of deaths decreases over time

# Binomial test
# card guessin ESP
# Zener cards: 5 cards of 5 different symbols
# H0: No ESP in the guesser
# if the guesses are at random: p = 0.20 (1/5)

# eg1. user guesse: 9 correctly
print(binom.test(x=9, n=25, p=0.20))

# we would expect his success rate to be not just different from chance 
# but greater than chance. 
print(binom.test(x=9, n=25, p=.2, alternative="greater"))

# say the experiment continues to large N
print(prop.test(x=2022, n=10000, p=.2, alternative="greater"))

# Eg 2
# A random sample of 428 adults from Myrtle Beach reveals 128 smokers. A random 
# sample of 682 adults from San Francisco reveals 170 smokers. Is the proportion 
# of adult smokers in Myrtle Beach different from that in San Francisco?
print(prop.test(x=c(128,170), n=c(428,682), alternative="two.sided", 
               conf.level=.99))

#power calculation, calculate n for EACH GROUP  
print(power.prop.test(p1=.299, p2=.249, sig.level=.05, power=.9,
                      alternative="two.sided"))
