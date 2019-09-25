aov.out = aov(count ~ spray, data=InsectSprays)
TukeyHSD(aov.out)

# HSD value
(5.532742-(-3.866075))/2             # HSD value from the B-A result
plot(TukeyHSD(aov.out), las=1)       # las=1 just turns the axis labels

# Fisher LSD test
# It's good for about three or four comparisons without inflating the error rate.
# Beyond that, the inflation grows rapidly with increasing numbers of comparisons,
with(InsectSprays, pairwise.t.test(x=count, g=spray, p.adjust="none"))# x=DV, g=IV

# If we could make B the first level, then we could get the B-comparisons. Easy peasy!
Bspray = relevel(InsectSprays$spray, ref="B")       # "reference group" is now B
levels(Bspray)
summary.lm(aov(InsectSprays$count ~ Bspray))$coef   # the ANOVA must be recalculated
#The LSD (least significant difference) can be calculated this way.

alpha = .05
s = 3.921902                         # from the aov output, this is sqrt of MSE
df = 66                              # also from the aov output, df error
tcrit = qt(p=1-alpha/2, df=df)
LSD = tcrit * s * sqrt(1/12+1/12) # reciprocals of the group sizes, each group is n=12
LSD

# Bonferroni test
# The Bonferroni test is done exactly like the Fisher LSD test, but once the p-values are obtained, they are multiplied by the number of comparisons being made.
with(InsectSprays, pairwise.t.test(x=count, g=spray, p.adjust="bonferroni"))
## better
with(InsectSprays, pairwise.t.test(x=count, g=spray, p.adjust="holm"))

## orthogonal contrasts is in a different source file
