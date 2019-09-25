## Orthogonal Contrasts

# dry-weight production of biomass by plants as the result of three different treatments:
#   a control, and two experimental treatments.
data(PlantGrowth)
summary(PlantGrowth)
with(PlantGrowth, tapply(weight, group, mean))
with(PlantGrowth, tapply(weight, group, var))

with(PlantGrowth, bartlett.test(weight ~ group))
# inference = artlett's test does not reveal a significant deviation from homogeneity of variance

## NOTE
# Contrasts do not have to be "orthogonal."
# Think of orthogonal as meaning something like "independent" or "unconfounded."


levels(PlantGrowth$group)

# setting up contrasts
##  H1: lump the treatment groups into a megagroup, and test that megagroup against the control group.
vec1 = c(1,-1/2,-1/2)
# H2:  we want to compare "trt1" to "trt2". The secret here is to code any level not included in the contrast as 0
vec2 = c(0,1,-1)
### now cbind the contrast vectors into the columns of matrix
cons = cbind(vec1, vec2)
rownames(cons) = c("ctrl", "trt1", "trt2")
cons

### to be orthogonal, all pairs of contrast vectors must have a dot product of 0
### here's a convenient way to test that for a contrast matrix of any size
t(cons) %*% cons
### if this matrix multiplication results in a matrix that is nonzero ONLY on
### the main diagonal, then the contrasts are orthogonal

contrasts(PlantGrowth$group) = cons
aov.out = aov(weight ~ group, data=PlantGrowth)
aov.out
summary(aov.out, split=list(group=list("ctrl v trts"=1, "trt1 v trt2"=2)))
##  we give our contrasts sensible names, and we set those names equal to one
# of the columns in the contrast matrix (by number of the column).

## example 2
## The data are based on an experiment by Eysenck (1974) on incidental learning,
# which is learning that takes place when one is not really attempting to learn
# but just through exposure to the material. Subjects were given a list of
# 30 words and one of five different instructions: count the letters in each word,
# think of a word that rhymes with each word, think of an adjective that could be
# used to modify the word in a sentence, form a mental image of the object
# described by the word, or a control condition in which the subjects were told to memorize the list.
scores_df = data.frame(scores)

write.csv(scores_df, file = "incidential_learning.csv")


# gl(), which means generate levels for a balanced design. The syntax is kind
# of goofy in that n stands for the number of levels of the IV, while k stands
# for the number of replicates (subjects per group)

groups = gl(n=5,           # generate levels, n=no. of levels
            k=10,          # k=replications per group
            labels=c("count","rhyme","adjec","image","contr"))

levels(groups)

c1=c(-1/4,  -1/4,   -1/4,   -1/4,   +1)   # control vs. treatments
c2=c(-1/2,  -1/2,   +1/2,   +1/2,    0)   # shallow vs. deep processing
c3=c(   0,     0,     -1,     +1,    0)   # are the deep processing conditions different
c4=c(  -1,    +1,      0,      0,    0)   # are the shallow processing conditions different
cons  = cbind(c1, c2, c3, c4)
rownames(cons) = levels(groups)
cons

contrasts(groups) = cons
aov.out = aov(scores ~ groups)       # do this AFTER contrasts are assigned
summary(aov.out, split=list(groups=list("cont-treat"=1, "shallow-deep"=2,
                                        "deep-deep"=3, "shallow-shallow"=4)))
