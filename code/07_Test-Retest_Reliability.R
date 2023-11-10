# install.packages("faux")
rm(list=ls())
library(faux)

## SIMULATE DATA WITH GIVEN CORRELATION
# sim parameters
n = 50 # number of observations from samples (note: n1 must be equal to n2)
mus=c(20,20) # mean of populations from which samples are drawn 
t_r = 0.6 # true underlying correlation
# rnorm_multi (from faux package) simulate random observation from populations with given correlation
res = rnorm_multi(n, mu=mus, r = t_r)
names(res)=c("test", "retest")

# the line below serves to define a larger ylim (the range in y axis), for better plotting below
my_ylim = range(unlist(res)) + c(-5*sd(unlist(res)), +5*sd(unlist(res)))

# plot test and retest 
plot(res$test, res$retest, main=paste("r = ", round(cor(res$test, res$retest), 2)), ylim=my_ylim, xlab = "test", ylab="retest")


# create a column that simulate a practice effect
pract_eff = + 2
res$retest_p = res$retest + pract_eff

points(res$test, res$retest_p, pch=19)
points(res$test, res$test, pch=21, bg="gray")

abline(0, 1) # this plots a bisection line where x = y
legend("topright", legend=c("original", "with practice effect", "perfect fit (test = retest)"), 
       bg=c("white", "black"), pch=c(1, 19, 21), pt.bg=c("gray"), cex=0.7)


# test if there is a significant different between test and retest
t.test(res$test, res$retest, paired=TRUE)

# test if there is a significant different between test and retest with practice effect
t.test(res$test, res$retest_p, paired=TRUE)
# NOTE: the mean difference approximate what I added with + pract_eff

# correlations are unchanged
cor.test(res$test, res$retest)
cor.test(res$test, res$retest_p)

# don't confuse High test-retest with high "stability". It is just high consistency.


