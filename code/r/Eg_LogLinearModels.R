## log linear models
data("Titanic")
print(dimnames(Titanic))

# survivors
print(margin.table(Titanic))

#look at the data collapsed over "Class" and "Age".
margin.table(Titanic, c(2,4))

likelihood.test = function(x) {
  nrows = dim(x)[1]                      # no. of rows in contingency table
  ncols = dim(x)[2]                      # no. of cols in contingency table
  chi.out = chisq.test(x,correct=F)      # do a Pearson chi square test
  table = chi.out[[6]]                   # get the OFs
  ratios = chi.out[[6]]/chi.out[[7]]     # calculate OF/EF ratios
  sum = 0                                # storage for the test statistic
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      sum = sum + table[i,j]*log(ratios[i,j])
    }
  }
  sum = 2 * sum                          # the likelihood ratio chi square
  df = chi.out[[2]]                      # degrees of freedom
  p = 1 - pchisq(sum,df)                 # p-value
  out = c(sum, df, p, chi.out[[1]])      # the output vector
  names(out) = c("LR-chisq","df","p-value","Pears-chisq")
  round(out,4)                           # done!
}

sex.survived = margin.table(Titanic, c(2,4))   # create the contingency table
likelihood.test(sex.survived)

# Our goal is to explain the observed frequencies in the "Titanic" table with
# as simple a model as possible. Here is what we have to work with.
#
#
# EFFECTS	WHAT THEY MEAN
# Class	there are more passengers in some classes than in others
# Sex	there are more passengers of one sex than of the other
# Age	there are more passengers of one age group than of the other
# Survived	more passengers lived or died than the alternative
# Class × Sex	Class and Sex are not independent
# Class × Age	Class and Age are not independent
# Class × Survived	Class and Survived are not independent
# Sex × Age	Sex and Age are not independent
# Sex × Survived	Sex and Survived are not independent
# Age × Survived	Age and Survived are not independent
# Class × Sex × Age	there is a three-way interaction between these factors
# Class × Sex × Survived	there is a three-way interaction between these factors
# Class × Age × Survived	there is a three-way interaction between these factors
# Sex × Age × Survived	there is a three-way interaction between these factors
# Class × Sex × Age × Survived	there is a four-way interaction between these factors

print(summary(Titanic))

library(MASS)
myModel1 = loglm( ~ Sex + Survived, data=sex.survived)
print(myModel1)
# p = 0; there is an interaction in the table
print(sex.survived)

myModel2 = loglm(~ Class + Sex + Age + Survived, data=Titanic)
print(myModel2)
#  four-way chi-square test of independence, and we reject H0 (independence)

model_fourway = loglm(~ Class * Sex * Age * Survived -
                        Class:Sex:Age:Survived, data=Titanic)
print(model_fourway)


# interaction between Sex, Class, Age and Survived
sat.model = loglm(~ Class * Sex * Age * Survived, data=Titanic)
print(sat.model)

# refine this
model2 = update(sat.model, ~.-(Class:Sex:Age:Survived+Sex:Age:Survived+Class:Sex:Survived+Sex:Survived))
print(model2)

model3 = update(sat.model, ~.-(Class:Age:Sex:Survived+Sex:Age:Survived+Class:Sex:Survived))
print(model3)

# Nope! This model is also rejected. Apparently, the "Sex" by "Survived"
# interaction is conditioned on (dependent upon, changes form with) another one
# of the factors. I.e., there is an important three-way interaction involving
# "Sex", "Survived", and one of the other variables.

## okay lets cut R loose here
step(sat.model, direction="backward")
# "Sex" and "Survived" is conditioned on "Class"
# check it out
margin.table(Titanic, c(2,4,1))

# inference: In all classes the odds of a female surviving were better than the
# odds of a male surviving, but this was especially true in first class, and only
# somewhat true in third class.

## GLMs
ti = as.data.frame(Titanic)
glm.model = glm(Freq ~ Class * Age * Sex * Survived, data=ti, family=poisson)
print(glm.model)
print(anova(glm.model, test="Chisq"))

# refine this
##  Here we are looking for a significant reduction in deviance
print(anova(update(glm.model,.~.-(Class:Age:Sex:Survived+Age:Sex:Survived+
                                    Class:Age:Sex)),test="Chisq"))
mosaicplot(Titanic, shade=T)
