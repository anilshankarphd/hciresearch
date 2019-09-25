## Graphics
plot(1:100, 1:100, type="n", xlab="", ylab="")
curve(x^2/100, add=TRUE)
text(x=80, y=50, "This is a graph of")
text(x=80, y=45, "the equation")
text(x=80, y=37, expression(y == frac(1,100) * x^2))

points(x=c(20, 60, 90), y=c(4, 36, 81), pch=6)

points(x=rep(100,10), y=seq(0,90,10), pch=seq(1,20,2))

#"a=the y-intercept" and "b=the slope" of the desired line.
abline(a=-18, b=1.1, col="red")
text(x=19, y=0, "A")
text(x=100, y=95, "B") ## add my labels

abline(h=20, lty=2)             # abline(h=20, lty="dashed") also works
abline(v=20, lty=3)             # abline(v=20, lty="dotted") also works

lines(x=c(40, 40, 60, 60), y=c(80, 100, 100, 80), type="b")
lines(x=c(40, 60), y=c(80, 80), type="l")      # type="lower case L", not "one"

title(main="A Drawing To Put On the Refrigerator!")
title(xlab="This is the x-axis", col.lab="blue", cex.lab=1.5)

## stacked barplots
data(UCBAdmissions)
par(mfrow=c(1,2))
margin.table(UCBAdmissions, c(1,3)) -> Admit.by.Dept
barplot(Admit.by.Dept)
barplot(Admit.by.Dept, beside=T, ylim=c(0,1000), legend=T,
                main="Admissions by Department")

# histograms
data(faithful)                       # This is optional.
attach(faithful)
hist(waiting)
hist(waiting, breaks=seq(40,100,10), right=F) # inclue xlim

hist(waiting, prob=T) # To plot the smoothed curve on top of a histogram, set the "prob=" option to TRUE
lines(density(waiting))
detach(faithful)


# Numerical Summaries by Groups
data(chickwts)                  # Weight gain by type of diet.
attach(chickwts)
plot(feed, weight)              # boxplot(weight ~ feed) will also work
#boxplot(feed~weight)
title(main="Body Weight of Chicks by Type of Diet")

## adding means
means = tapply(weight, feed, mean)
points(x=1:6, y=means, pch=12)       # pch=16 is a filled circle
detach(chickwts)

# Scatterplots
data(mammals, package="MASS")
attach(mammals)
plot(log(body), log(brain))          # plot(x=body, y=brain, log="xy") is similar (try it)
scatter.smooth(log(body), log(brain))
detach(mammals)


