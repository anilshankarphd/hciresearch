# r shiny for dtk?
# say B-glasses, 3 prototypes, 3 experiments

# dimensions
# age: 1, 2, 3, 4, 5, 6, 7
# computer experience: 1, 2, 3, 4, 5 ## we balanced it out for now; all same exp
# target size (W): 10, 20, 30, 40, 50
# target distance (D): 10, 20
# time
## (fitts law), a, b : constants proportional to input device
##  = a + b * index-of-difficulty
##  = a + b * log_2  (2 * distance from starting point to center of target)/
##                                                   (width of target)

set.seed(19782019)
# comp_exp = c(1, 2, 3, 4, 5)
#comp_exp = factor(comp_exp)
age = c(1, 2, 3, 4, 5, 6, 7)
age = factor(age)
target_distance = c(10, 20, 30, 40, 50)
target_width = c(10, 20)
n_users = 10 # no. of users in each group
users = paste(rep("participant", n_users))
ids = paste(seq(1, n_users))
uIds = paste0(users, ids)
uIds = data.frame(uIds)

# 3 prototypes -- move it out of the loop
aVals = c(25, 12, 40)
bVals = c(12, 8, 6)
aStdDev = 6
bStdDev = 4

a1 = data.frame(rnorm(n=n_users, mean = aVals[1], sd=aStdDev))
b1 = data.frame(rnorm(n=n_users, mean = bVals[1], sd=bStdDev))

a2 = data.frame(rnorm(n=n_users, mean = aVals[2], sd=aStdDev))
b2 = data.frame(rnorm(n=n_users, mean = bVals[2], sd=bStdDev))

a3 = data.frame(rnorm(n=n_users, mean = aVals[3], sd=aStdDev))
b3 = data.frame(rnorm(n=n_users, mean = bVals[3], sd=bStdDev))


calc_fitts_time = function(UserId, D, W, a1, b1, a2, b2, a3, b3) {
  #time_taken = a + b * log2(2*D/W)
  for (dist in target_distance) {
    for (width in target_width) {

       myRes = data.frame (a1 + b1 * log2(2*dist/width))
       names(myRes)[1] = "Proto_1_time"

       myRes2 = data.frame (a2 + b2 * log2(2*dist/width))
       names(myRes2)[1] = "Proto_2_time"

       myRes3 = data.frame (a3 + b3 * log2(2*dist/width))
       names(myRes3)[1] = "Proto_3_time"


       myRes$dist = dist
       myRes$width = width
       myRes$UserId = UserId

       names(myRes)[2] = "Dist"
       names(myRes)[3] = "Width"
       names(myRes[4]) = "User"
       myRes[5] = myRes2
       myRes[6] = myRes3

       myRes = myRes[c(4, 2, 3, 1, 5, 6)]
       print(myRes)
    }
  }
}
t1 = calc_fitts_time(uIds, target_distance, target_width, a1, b1, a2, b2, a3, b3 )


