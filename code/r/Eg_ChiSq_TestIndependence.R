# chi square tests of independence
# age and freq. of breast cancer exams
row1 = c(91,90,51)                   # or col1 = c(91,150,109)
row2 = c(150,200,155)                # and col2 = c(90,200,198)
row3 = c(109,198,172)                # and col3 = c(51,155,172)
data.table = rbind(row1,row2,row3)   # and data.table = cbind(col1,col2,col3)

dimnames(data.table) = list(Age=c("lt.45","45.to.59","ge.60"),
                            Freq=c("Monthly","Occasionally","Never"))

print(chisq.test(data.table))
addmargins(data.table)
print(data.table)

### Examine conditional distributions...
prop.table(data.table, 1)  

barplot(data.table, beside=T) 

barplot(t(data.table), beside=T, legend=T, ylim=c(0,250),
          ylab="Observed frequencies in sample",
          main="Frequency of Breast Self-Examination by Age")


# data from table object
print(UCBAdmissions)                                  
# compact version
print(ftable(UCBAdmissions, row.vars=c("Admit")))
print(round(prop.table(ftable(UCBAdmissions, row.vars=c("Admit")),2),2))

# look at dept. B
deptB = UCBAdmissions[,,2]    
#res = chisq.test(deptB)
res = chisq.test(dept, correct = F)
print(res)
print(res$residuals)
print(res$expected)

# data from data frame
# For one item, the students were asked to fold their arms, and the recorded
# result was which arm was on top: right, left, or neither.
# Let's see how this variable relates to the sex of the student.
data(survey, package="MASS")
attach(survey)
table(Sex, Fold)
print(chisq.test(x=Sex, y=Fold))
# inference = no. sig. difference
detach(survey)

#  study of the effectiveness of the antidepressant Celexa in the treatment of 
# compulsive shopping. 

freqs = c(2, 2, 3, 8, 7, 2)               # entered "down the columns" this time
data.matrix = matrix(freqs, nrow=2)       # fills down columns by default
dimnames(data.matrix) = list(treatment=c("Celexa","placebo"), 
                             outcome=c("worse","same","better"))
print(data.matrix)
print(chisq.test(data.matrix))
print(chisq.test(data.matrix)$expected)

# values are less than 5
# There are two choices: a Fisher Exact Test, and a p-value calculated by 
# Monte Carlo simulation. Let's look at the Fisher Exact Test first.
print(fisher.test(data.matrix))
# no stat. sig. difference

mc.sim.result = chisq.test(data.matrix, simulate.p.value=T, B=999)
# The "simulate.p.value=T" option (default value is FALSE) does the 
# Monte Carlo simulation using "B=999" (default value is B=2000) replicates.
print(mc.sim.result)
# even with MC there is no sig. relationship




