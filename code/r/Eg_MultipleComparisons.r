# Multiple comparisons example -- good
# source: http://ww2.coastal.edu/kingw/statistics/R-tutorials/multcomp.html

## example
options("contrasts") ## check if this is set
## if not: options(contrasts = c("contr.treatment","contr.poly"))

rawData = data.frame(contr = c(22,18,25,25,20),treat1 = c(32,35,30,42,31),
                     treat2 = c(30,28,25,22,33))
myRData = stack(rawData)
colnames(myRData) = c("scores", "groups")

## going from wide to long to wide
# 1 participant to many participants
data(anorexia, package="MASS")
# 9 cases, 3 from each group
# just one subject
# contains information on ONE SUBJECT, even though that subject was measured
# multiple times (twice) on weight (Prewt, Postwt). So all the data for each
# subject goes on ONE LINE, even though we could interpret this as a repeated
# measures design, or longitudinal data.
anor = anorexia[c(1,2,3,27,28,29,56,57,58),]
anor.long = reshape(data=anor, direction="long",
        varying=c("Prewt","Postwt"), v.names="Weight",
        idvar="subject", ids=row.names(anor),timevar="PrePost",
        times=c("Prewt","Postwt"))
rownames(anor.long) <- as.character(1:18) # have to, sadly
back.anor = reshape(anor.long) ## roll back is easy
# another way to restore specific col (Weight)
anor.long2 = reshape(data=anor.long, direction="wide", v.names=c("Weight"),
                     idvar="subject",  timevar="PrePost") # much better!

