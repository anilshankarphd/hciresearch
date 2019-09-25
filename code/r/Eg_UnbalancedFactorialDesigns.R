## unbalanced factorial designs
aovIII = function (formula, data = NULL)
{
  options(contrasts = c("contr.sum", "contr.poly"))
  aovIII.out <<- aov(formula, data)
  print(drop1(aovIII.out, . ~ ., test = "F"))
  options(contrasts = c("contr.treatment", "contr.poly"))
}

data(genotype, package="MASS")
summary(genotype)

# check for balance
with(genotype, table(Litter, Mother))

# Type I (R default)
# ult is different depending upon the order in which the factors are entered
# into the model formula.
aov.out1 = aov(Wt ~ Litter * Mother, data=genotype)
print(aov.out1)

aov.out2 = aov(Wt ~ Mother * Litter, data=genotype)
print(aov.out2)

# Type II
library("car")
Anova(aov.out1)

## Type III buckle up
# We cannot use the above runs because they were done when the contrasts were
# set to treatment.
aov.out3 = aov(Wt ~ Litter * Mother, data=genotype)
Anova(aov.out3, type=3)
