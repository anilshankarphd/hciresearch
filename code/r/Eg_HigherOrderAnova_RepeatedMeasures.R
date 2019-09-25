# higher order anova with repeated measures
# "Five rabbits were studied on two occasions, after treatment with saline
#      (control) and after treatment with the 5-HT_3 antagonist MDL 72222. After
#      each treatment ascending doses of phenylbiguanide were injected intravenously
#      at 10 minute intervals and the responses of mean blood pressure measured.
#      The goal was to test whether the cardiogenic chemoreflex elicited by
#      phenylbiguanide depends on the activation of 5-HT_3 receptors."

# We will treat Treatment and Dose as factors
# thus, we have a 2x6 factorial design with repeated measures on both factors.
# The response is BPchange. The subject ID is Animal.

data(Rabbit, package="MASS")
summary(Rabbit)

Rabbit$Dose = factor(Rabbit$Dose)              # make sure R sees your factors as factors
with(Rabbit, table(Treatment, Dose))           # check for balance

### a more thorough check would be table(Subject,Treatment,Dose) showing
 ### that each rabbit appears once in each of the treatment cells
with(Rabbit, tapply(BPchange, list(Treatment,Dose), mean))
with(Rabbit, tapply(BPchange, list(Treatment,Dose), var))
# inf = some pretty ugly problems with the variances

with(Rabbit, interaction.plot(Dose,       # factor displayed on x-axis
                              Treatment,  # factor displayed as multiple lines
                              BPchange,   # the response variable
                              type="b",   # plot both points and lines
                              pch=1:2,    # use point characters 1 and 2
                              bty="l"))   # draw a box around the plot that looks like an L

# We want an Error term that specifies which effects are seen within subjects
# and in this case that is all of them:
# the main effect of Dose, the main effect of Treatment, and the Dose-by-Treatment interaction.
aov.out = aov(BPchange ~ Dose * Treatment + Error(Animal/(Dose*Treatment)), data=Rabbit)
summary(aov.out)

# inf = There is a significant main effect of dose.
# No surprise there!
# There is a significant main effect of Treatment, which is what we were supposedly looking for.
# But the main effects are complicated by the presence of a significant interaction.


## two way mixed factorial designs
data("ChickWeight")
CW = subset(ChickWeight, as.numeric(Diet) > 1 & Time < 20)  ###  look at summaries
CW$Diet = factor(CW$Diet, exclude="1")                        ## to see what we
CW$Time = factor(CW$Time)                                   ###  are doing here
summary(CW)
plot(density(CW$weight))

with(CW, table(Diet, Time))               # check for balance
# The help page says there are 4 diets. We threw out the first one to create a balanced data set.
with(CW, interaction.plot(Time, Diet, weight, type="b", pch=1:3))

# error term
# Our subject ID is Chick (and notice it is already declared a factor), and the
# effect that is "inside" of Chick is Time.
aov.out = aov(weight ~ Diet * Time + Error(Chick/Time), data=CW)
summary(aov.out)

# weight of chicks is a whole lot less variable when they are 0 days old
# than it is when they are 18 days old.
with(CW, interaction.plot(Time,Diet,weight,fun=var,type="b",pch=1:3))   # not shown
# ;argest variance is about 4000 times the smallest variance.



## Three Way Designs With Repeated Measures on One Factor
## GOOD FOR CASE STUDY: EXAMPLE -1 SCALING THE EXPERITMENT
#  data from an experiment on the cold tolerance of the grass species
data(CO2)
summary(CO2)

# approach
# "Plant" is the subject identifier. "Type" gives the geographical origin of the plant.
# "Treatment" tells whether the plant was chilled or not. Both of those factors are between subjects.
# The numeric variable "conc" is ambient carbon dioxide concentration.
# These values are repeated for each plant, and once again we are going to toss out valuable
# numerical information and make this a factor.
# The response is "uptake", giving carbon dioxide uptake rates.
CO2$conc = factor(CO2$conc)
with(CO2, table(Treatment, conc, Type))       # checking for balance
with(CO2, table(Plant, conc))                  # really making sure!
par(mfrow=c(1,2))
with(CO2[1:42,], interaction.plot(conc, Treatment, ylim=c(0, 40), uptake, type="b", pch=1:2, bty="l"))
title(main="Quebec")
with(CO2[43:84,], interaction.plot(conc, Treatment, ylim=c(0, 40), uptake, type="b", pch=1:2, bty="l"))
title(main="Mississippi")
## inference  warm climate plants don't handle being chilled as well as the cold climate plants do.
## error term: error term must recognize that "conc" is the only within subjects factor.
aov.out = aov(uptake ~ Type * Treatment * conc + Error(Plant/conc), data=CO2)
summary(aov.out)


## Three Way Designs With Repeated Measures on Two Factors
## THE IS MY MEDIUM/LHIGH LEVEL CASE STUDY
file = "http://ww2.coastal.edu/kingw/statistics/R-tutorials/text/react.txt"
react = read.csv(file)
write.csv(react, file="react_3_way_repeated_measures_2factors.csv")
summary(react)

## how the experiment was set up
# These data were collected in an undergraduate experimental psychology lab. There were two lab sections ("lab"), A and B, with 9 students in each section, so this is a between subjects factor. Each student did three different kinds of reaction time task ("task"), simple, disjunctive, and choice, so this is a within subjects factor. In addition, each student did each task with two different colors of stimulus lights ("color"), green and red, so this is also a within subjects variable. The response was reaction time in milliseconds. So we have a 2x3x2 factorial design with repeated measures on factors two and three.
# The experiment was similar to studies done by Donders in the 19th century in which he was attempting to measure the amount of time it takes a mental event to occur (mental chronometry it was called). Simple reaction time tasks are quite simple. You see a stimulus, and you react to it as quickly as you possibly can. Disjunctive tasks are a little more complex, in that two (or more, but two here) different stimuli might be presented, and you are only to respond to one of them. So in addition to the time it takes to react, there is also a decision to make: should I react or not. Finally, in choice reaction time tasks, there are two (or more) different stimuli, and each requires a different response. So the decision has become more complex: which response should I make.
with(react, tapply(RT, list(color, task, lab), mean))
with(react, tapply(RT, list(color, task, lab), var))
## The largest variance is 4333.6/133.7=32.4 times as large as the smallest one, once again problematic, and once again something we try to pretend isn't a problem
condition = paste(react$lab, react$task, react$color,sep=".")
bartlett.test(react$RT ~ condition)
## inf = variances are not equal!
par(mfrow=c(1,2))
with(react[1:54,], interaction.plot(task, color, RT,
                               type="b", pch=1:2, bty="l", ylim=c(300,450)))
title(main="Lab A")
with(react[55:108,], interaction.plot(task, color, RT,
                             type="b", pch=1:2, bty="l", ylim=c(300,450)))       # y-axis same on both graphs
title(main="Lab B")
## inference = Both task and color as well as their interaction are within subjects
aov.out = aov(RT ~ lab * task * color + Error(subject/(task*color)), data=react)
summary(aov.out)
## inference
# There are two significant main effects, lab and task.
# Task I expected.
# Lab I have no explanation for considering the very simple nature of the response.
