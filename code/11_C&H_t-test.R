## CREATE A FUNCNTION FOR CRAWFORD T-TEST

crawford.t <- function(pat.score, control.scores, tails=c("lower", "upper", "two"), verbose=TRUE){
  n = length(control.scores)
  craw.t = (pat.score - mean(control.scores))/( sd(control.scores)*sqrt((n+1)/n))
  df = n-1
  
  if (tails[1]=="lower"){
    p.val = pt(craw.t, df=df, lower=T)
  }
  if (tails[1]=="upper"){
    p.val = pt(craw.t, df=df, lower=T)
  }
  if (tails[1]=="two"){
    p.val = pt(abs(craw.t), df=df, lower=F) * 2
  }
  
  if (verbose){
    cat("t = ", craw.t, "\n")
    cat("p value = ", p.val, "\n")
    cat("tails = ", tails[1], "\n")
  }
  res = data.frame(t = craw.t, p = p.val, tails = tails[1])
  invisible(res)
}


##################################################
## CASE 1) TYPE 1 ERROR WITH NORMAL DISTRIBUTION 
##################################################
# in a population we know the real cut-offs
pop_mean = 20
pop_sd = 10
real_cut_off = pop_mean - 1.64 * pop_sd

# plot population distribution
x = seq(pop_mean-3*pop_sd, pop_mean+3*pop_sd, 0.1)
y = dnorm(x, mean=pop_mean, sd=pop_sd) 
yrand = rnorm(10000, mean=pop_mean, sd=pop_sd) 

par(mfrow=c(1,2))
plot(x, y, type="l", main="Population density")
hist(yrand, main="Population histogram\n(10000 observations)")


# we can simulate samples and see what are the observed cut-offs
n = 100 # size of sample (number of participants in my normative data)
k = 1000 # number of samples to simulate
p = 1000 # number of simulated tested participants from normal population

# for each sample I test p participants, if my method is accurate, I will classify them
# below the cut-off 5% of the time (by definition, this is my definition). The sample cut-off will be different, however,
# and I can classify my miscalculation of patient for each sample.

z_below = NULL
perc_below = NULL
t_below = NULL




for (iK in 1:k){
  
  norm_sample_values = rnorm(n, mean=pop_mean, sd=pop_sd)
  
  pat_values = rnorm(p, mean=pop_mean, sd=pop_sd)
  
  # z test results
  z_scores = (pat_values - mean(norm_sample_values))/sd(norm_sample_values)
  z_below_samp = z_scores < -1.64 # NOTE: calculated on sample
  
  # non-parametric percentile results
  perc_crit = quantile(norm_sample_values, probs=0.05)
  perc_below_samp = pat_values < perc_crit
  
  # t-test results
  t_res = crawford.t(pat_values, norm_sample_values, verbose=F)
  t_below_samp = t_res$p < 0.05
  
  z_below=c(z_below, sum(z_below_samp)/p)
  perc_below=c(perc_below, sum(perc_below_samp)/p)
  t_below = c(t_below, sum(t_below_samp)/p)
  
}



# NOTE: i is thte total number of iteration, that is 
# k x p (number of simulated samples x number of simulated patients)

ERR_Z = mean(z_below) # expected is 5%
ERR_PERC = mean(perc_below)  # expected is 5%
ERR_T = mean(t_below)  # expected is 5%

print(ERR_Z)
print(ERR_PERC)
print(ERR_T)



par(mfrow=c(1,3))
tot_range = range(c(z_below, perc_below, t_below))
hist(z_below, breaks=10, xlim=tot_range, main="percentage of participants below cut-offs\n using z-score")
hist(perc_below, breaks=10, xlim=tot_range, main="percentage of participants below cut-offs\n using non-par percentile")
hist(t_below, breaks=10, xlim=tot_range, main="percentage of participants below cut-offs\n using Crawford & Howell t-test")


###########################################################################
## CASE 2) TYPE 1 ERROR WITH NON-NORMAL DISTRIBUTION
##########################################################################
# in a population we know the real cut-offs
shape1 = 10
shape2 = 1 # the higher this value the steeper the distribution (the better t)

real_cut_off = qbeta(0.05, shape1, shape2)*20

# plot population distribution
x = seq(0, 1, 0.1)
y = dbeta(x, shape1, shape2) * 20
yrand = rbeta(1000, shape1, shape2)*20

par(mfrow=c(1,2))
plot(x, y, type="l", main="Population density")
hist(yrand, main="Population histogram\n(1000 observations)")

# we can simulate samples and see what are the observed cut-offs
n = 5 # size of sample (number of participants in my normative data)
k = 1000 # number of sample to simulate
p = 1000 # number of simulated tested participants from normal population

# for each sample I test p participants, if my method is accurate, I will classify them
# below the cut-off 5% of the time (by definition, this is my definition). The sample cut-off will be different, however,
# and I can classify my miscalculation of patient for each sample.

z_below = NULL
perc_below = NULL
t_below = NULL

for (iK in 1:k){
  
  norm_sample_values = rbeta(n, shape1, shape2)*20
  
  pat_values = rbeta(p, shape1, shape2)*20
  
  # z test results
  z_scores = (pat_values - mean(norm_sample_values))/sd(norm_sample_values)
  z_below_samp = z_scores < -1.64 # NOTE: calculated on sample
  
  # non-parametric percentile results
  perc_crit = quantile(norm_sample_values, probs=0.05)
  perc_below_samp = pat_values < perc_crit
  
  # t-test results
  t_res = crawford.t(pat_values, norm_sample_values, verbose=F)
  t_below_samp = t_res$p < 0.05
  
  z_below=c(z_below, sum(z_below_samp)/p)
  perc_below=c(perc_below, sum(perc_below_samp)/p)
  t_below = c(t_below, sum(t_below_samp)/p)
  
}


# NOTE: i is thte total number of iteration, that is 
# k x p (number of simulated samples x number of simulated patients)

ERR_Z = mean(z_below) # expected is 5%
ERR_PERC = mean(perc_below)  # expected is 5%
ERR_T = mean(t_below)  # expected is 5%

print(ERR_Z)
print(ERR_PERC)
print(ERR_T)


par(mfrow=c(1,3))
tot_range = range(c(z_below, perc_below, t_below))
hist(z_below, breaks=10, xlim=tot_range, main="percentage of participants below cut-offs\n using z-score")
hist(perc_below, breaks=10, xlim=tot_range, main="percentage of participants below cut-offs\n using non-par percentile")
hist(t_below, breaks=10, xlim=tot_range, main="percentage of participants below cut-offs\n using Crawford & Howell t-test")


##################################################
### EFFECT OF SAMPLE SIZE ON CRITICAL T (i. on cut-offs)
##################################################
crawford.cut.off = function(prob = 0.05, control.scores){
  n = length(control.scores)
  df = n-1
  crit.t =qt(prob, df = df) 
  pat.score = (crit.t *  sd(control.scores)*sqrt((n+1)/n)) + (mean(control.scores))
  return(pat.score)
}


# in a population we know the real cut-offs
pop_mean = 20
pop_sd = 10
real_cut_off = pop_mean - 1.64 * pop_sd

# plot population distribution
x = seq(pop_mean-3*pop_sd, pop_mean+3*pop_sd, 0.1)
y = dnorm(x, mean=pop_mean, sd=pop_sd) 
yrand = rnorm(10000, mean=pop_mean, sd=pop_sd) 

par(mfrow=c(1,2))
plot(x, y, type="l", main="Population density")
hist(yrand, main="Population histogram\n(10000 observations)")


# we can simulate samples and see what are the observed cut-offs
ns = c(5, 10, 50, 500) # size of samples (number of participants in my normative data)
k = 1000 # number of samples to simulate

# for each sample I test p participants, if my method is accurate, I will classify them
# below the cut-off 5% of the time (by definition, this is my definition). The sample cut-off will be different, however,
# and I can classify my miscalculation of patient for each sample.


cut_offs = list(NULL)
length(cut_offs) = length(ns)


for (iN in 1:length(ns)){
  n = ns[iN]
  for (iK in 1:k){
    
    norm_sample_values = rnorm(n, mean=pop_mean, sd=pop_sd)
    
    # t-test results
    cut_off = crawford.cut.off(0.05, norm_sample_values)
    
    cut_offs[[iN]][iK] = cut_off
    
    
  }
}

par(mfrow=c(1,length(ns)))
for(iN in 1:length(ns)){
  hist(cut_offs[[iN]], xlab="cut-offs", main=paste("normative sample n = ", ns[iN], sep=""))
  abline(v = real_cut_off, lwd=2)
}
legend("topright", legend="real cut-off", lwd=3)







## ADDITIONAL
# interactive plot with manipulate to check effect of shape1 and shape2
# on distribution
library(manipulate)

shape2=10
x = seq(0, 1, 0.1)

manipulate(
  hist(rbeta(sample_size, shape1=shape1, shape2=shape2) * 20),
  shape1=slider(0, 10, step=0.1),
  shape2=slider(0,10),
  sample_size=slider(10,1000, step=10, initial=500, label="sample size"))




