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
Ts = 37 # true score
E.sd = 0.5 # Error (sd)

E = rnorm(1, mean=0, sd = E.sd)

X = Ts + E

print(X)

#############################################################
## SIMULATE MULTIPLE OBSERVED SCORE WITH A GIVEN TRUE SCORE
#############################################################
source("R_functions/simulate_obs.R")

simulate_obs(Ts=37, n.obs = 30, Mean.T=40, E.sd=1.5, Mean.E.sd=5, label="High Reliability")
simulate_obs(Ts=37, n.obs = 30, Mean.T=40, E.sd=4, Mean.E.sd=5, label="Low Reliability", plot_thresh = T)

