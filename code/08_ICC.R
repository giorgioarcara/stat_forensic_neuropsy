# install.packages("psych")
# install.packages("faux")
rm(list=ls())
library(psych)
library(faux)


## SIMULATE DATA WITH GIVEN CORRELATION
# sim parameters
n = 50 # number of observations from samples (note: n1 must be equal to n2)
mus=c(20,20,20) # mean of populations from which samples are drawn 
t_r = 0.6 # true underlying correlation
# rnorm_multi (from faux package) simulate random observation from populations with given correlation
mat = round(rnorm_multi(n, mu=mus, r = t_r))


# the one to be used is ICC 2 (that takes into account absolute agreement of random rater). 
# it corresponds to ICC(2,1) agreement in the slides
ICC(mat)

# here I create a version with a systematic bias in the scores (rater 2)
# are systematically higher but a precise amount
mat_bias = mat
mat_bias[,2]= mat_bias[,2] + 2 # the second rater has a score of + 4 as compared to before.

# check results when a systematic bias is present in the different versions of ICC
ICC(mat_bias)


# check what happen in the correlations (nothing)
cor.test(mat[,1], mat[,2])
cor.test(mat_bias[,1], mat_bias[,2])


#####################################################################################
# DATA SIMULATION. ICC21 OF SAMPLE, GIVEN KNOWN CORRELATION OF POPULATION
#####################################################################################

n = 200
n.rep = 200
t_r = 0.7
mus = c(20, 20, 20)


ICC21s = NULL
for (iE in 1:n.rep){
  res = rnorm_multi(n, mu=mus, sd=c(10, 10, 10), r = t_r)
  curr_ICC = ICC(res)
  ICC21s[iE] = curr_ICC$results$ICC[2]
}

mean(ICC21s)
hist(ICC21s, xlim=c(0,1))
range(ICC21s)





