##########################
# TRUE AND OBSERVED SCORES
##########################
# author: Giorgio Arcra
# ver: 13/10/2023

#################
# DESCRIPTION
###################
# this is a short script to simulate some properties of True and Observed scores
# under assumptions of classical test theory


# X = observed score
# T = True score
# E = error

# E = N(0, sd) # error is distributed "by definition" as a normal variable with 0 mean and a given standard deviation sd

##########################
## simulation paramters
#########################
T = 20 # true score
E.sd = 10 # Error  (sd)
n.obs = 10 # number of observation

# data simulation
Xs = NULL
for (iE in 1:n.obs){
  E = rnorm(1, mean = 0, sd = E.sd)
  Xs[iE] = T + E
}

mean(Xs) # calculate mean of Observed scores
hist(Xs) # histogram of observed scores
