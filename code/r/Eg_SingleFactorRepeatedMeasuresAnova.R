# single factor repeated measures anova

# single factor within subjects design

#example dataset
groceries = data.frame(
  c(1.17,1.77,1.49,0.65,1.58,3.13,2.09,0.62,5.89,4.46),
  c(1.78,1.98,1.69,0.99,1.70,3.15,1.88,0.65,5.99,4.84),
  c(1.29,1.99,1.79,0.69,1.89,2.99,2.09,0.65,5.99,4.99),
  c(1.29,1.99,1.59,1.09,1.89,3.09,2.49,0.69,6.99,5.15))
rownames(groceries) = c("lettuce","potatoes","milk","eggs","bread","cereal",
                        "ground.beef","tomato.soup","laundry.detergent","aspirin")
colnames(groceries) = c("storeA","storeB","storeC","storeD")
### end copying with this line and paste
write.csv(groceries, file = "groceries.csv")

# need long format for aov
gr2 = stack(groceries)                              # I'm tired of typing "groceries"!
gr2$subject = rep(rownames(groceries), 4)           # create the "subject" variable
gr2$subject = factor(gr2$subject)                   # "I declare thee a factor."
colnames(gr2) = c("price", "store", "subject")      # rename the columns

# which store should we shop?
with(gr2, tapply(price, store, sum))

# We need to supply an Error term,
# which must reflect that we have "treatments nested within subjects."
# That is to say, in principle at least, we can see the "store" effect within each and every "subject" (grocery item).
aov.out = aov(price ~ store + Error(subject/store), data=gr2)
summary(aov.out)

# post-hoc
with(gr2, pairwise.t.test(x=price, g=store, p.adjust.method="none", paired=T))

with(gr2, pairwise.t.test(x=price, g=store, p.adjust.method="bonf", paired=T))


# can also setup contrasts
cons = cbind(c(-1,1/3,1/3,1/3), c(0,-1/2,-1/2,1), c(0,1,-1,0))     # define the contrasts
t(cons) %*% cons                                                   # test for orthogonality
contrasts(gr2$store) = cons                                        # assign contrasts to treatment
aov.out = aov(price ~ store + Error(subject/store), data=gr2)      # do the ANOVA
summary(aov.out, split=list(
      store=list("A vs BCD"=1,"BC vs D"=2,"B vs C"=3)
   ))                                                                 # visualize the results

# inference; Notice that the result for B vs C is not quite the same as the t-test on that comparison above with no p-value adjustment. That's because in the paired pairwise t-tests, the overall pooled variance (MSE) is not used.


## alt. approach
# treated as a two-factor design without replication and without an interaction.
aov.tbys = aov(price ~ store + subject, data=gr2)
summary(aov.tbys)
TukeyHSD(aov.tbys, which="store")


#  nonparametric version of the oneway ANOVA with repeated measures
friedman.test(price ~ store | subject, data=gr2)
