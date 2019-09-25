## blocking design example
# In a treatment by blocks design we have subjects (experimental units) matched on some blocking variable that makes them somehow similar to one another in a way that should influence their response to the experimental treatments. In other words, a treatment by blocks design is what we would call a matched groups design when there are two levels of treatment and we are contemplating a t-test, except there are more than two levels of the treatment, and we are therefore contemplating an ANOVA.
# Once the subjects are matched (blocked), if they are then randomly assigned to treatment conditions within blocks--which is not true in any of the above examples--then we have a randomized blocks design. If every block has a subject in every condition, then it's a randomized complete blocks design. If that's not the case, then it's a randomized incomplete blocks design.
# An important assumption of treatment by blocks designs is that there is no treatment-by-blocks interaction in the population. Any interaction-like variability in the sample is assumed to be random error and constitutes the error term for the ANOVA.

# latin squares design

# An experiment was conducted to assess the potency of various constituents
# of orchard sprays in repelling honeybees, using a Latin square design.
# Individual cells of dry comb were filled with measured amounts of lime
# sulphur emulsion in sucrose solution. Seven different concentrations of
# lime sulphur ranging from a concentration of 1/100 to 1/1,562,500 in
# successive factors of 1/5 were used as well as a solution containing no
# lime sulphur.
#
# The responses for the different solutions were obtained by releasing 100
# bees into the chamber for two hours, and then measuring the decrease in
# volume of the solutions in the various cells.
#
# An 8 x 8 Latin square design was used and the treatments were coded as follows:
#   A  highest level of lime sulphur
# B  next highest level of lime sulphur
# ...
# G  lowest level of lime sulphur
# H  no lime sulphur
data(OrchardSprays)
OS = OrchardSprays              # I'm not typing Orch#ardSprays any more than necessary!
summary(OS)


OS$rowpos = factor(OS$rowpos)   # the row ...
OS$colpos = factor(OS$colpos)   # and column blocking factors must be seen by R as factors!
with(OS, table(rowpos,colpos))  # an unreplicated Latin square (n=1 per cell)

latSq = matrix(OS$treatment, nrow=8)    # and here is the Latin square
print(latSq)

par(mfrow=c(2,2))
plot(decrease ~ rowpos + colpos + treatment, data=OS)

aov.out = aov(decrease ~ rowpos + colpos + treatment, data=OS)
summary(aov.out)

TukeyHSD(aov.out, which= "treatment")

# Latin Square and Repeated Measures: Counterbalancing
# effect of background music on a card matching task. The subject was seated at
#a table on which he or she found an array of cards. The subject's task was to
# turn over the cards two at a time. If the cards matched, they were removed
#from the array. If they did not match, they were turned face down again, and t
#he subject continued to turn the cards two at a time. The objective was to
# match all the cards and remove them from the array as quickly as possible.
# Each subject did the task three times, once with each of three kinds of background music:
# no music, classical music, and instrumental rap music.
music_data = "http://ww2.coastal.edu/kingw/statistics/R-tutorials/text/rowe.txt"
match = read.csv(music_data)
write.csv(match, file = "musicdata.csv")
summary(match)

with(match, table(Order,Music))
par(mfrow=c(2,2))                                   # partition the graphics device
plot(Time ~ Order + Column + Music, data=match)
with(match, interaction.plot(Column, Order, Time))  # row by column interaction if any

aov.out = aov(Time ~ Order + Column + Music + Error(Subject/Music), data=match)
summary(aov.out)
