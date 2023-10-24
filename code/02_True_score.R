##########################
# TRUE AND OBSERVED SCORES
##########################
# author: Giorgio Arcara
# ver: 21/10/2023

#################
# DESCRIPTION
###################
# this is a short script to simulate some properties of True and Observed scores
# under assumptions of classical test theory


# X = observed score
# T = True score
# E = error

# E = N(0, sd) # error is distributed "by definition" as a normal variable with 0 mean and a given standard deviation sd

#############################################################
## SIMULATE A SINGLE OBSERVED SCORE GIVEN A KNOWN TRUE SCORE
#############################################################
T = 20 # true score
E.sd = 0.1 # Error (sd)

E = rnorm(1, mean=0, sd = E.sd)

X = T + E

print(X)

#############################################################
## SIMULATE MULTIPLE OBSERVED SCORE WITH A GIVEN TRUE SCORE
#############################################################
# the following lines show what 

T = 20 # true score
E.sd = 0.1 # Error  (sd)
n.obs = 200 # number of observations

# data simulation
Xs = NULL
for (iE in 1:n.obs){
  E = rnorm(1, mean = 0, sd = E.sd)
  Xs[iE] = T + E
}

print(Xs)

# calculate mean of observed scores (the larger the number of repeated measurement, the better the approximation to the True score)
mean(Xs) 

# histogram of observed scores
hist(Xs) 

# plot observed scores as compared to True Score
plot(1:n.obs, Xs)
lines(1:n.obs, Xs)
abline(h=T)
