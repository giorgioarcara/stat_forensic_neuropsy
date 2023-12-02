rm(list=ls())

# simulate normative data with normal distribution
n = 100
m = 20
s = 5

xs = rnorm(n = n, mean = m, sd = s)


hist(xs, breaks=10)

### calculate z-score to define known percentage
zs = ( xs - mean(xs) ) / sd(xs)

hist(zs, breaks=10) # note, histogram may be visually slightly diffrence, but this depends on how bins (i.e. breaks) are defined

plot(xs, zs)
cor.test(xs, zs)

# find clinical cut-off (i.e. 5% population) with z-scores formula
cut_off_z = mean(xs) - (1.64 * sd(xs))

## non-parametric percentile
cut_off_perc = quantile(xs, probs= 0.05)
print(cut_off_perc)
print(cut_off_z)


hist(xs, main = paste("difference in cut-off 5% = ", round(cut_off_z - cut_off_perc,2), sep=""))
abline(v = cut_off_z, lwd=3, col="red") # z score in red
abline(v = cut_off_perc, lwd=3, col="blue") # non-parametric percentile in blue


#### non normal distribution
# beta distribution can be used to build data that are non normally distributed
xs = rbeta(n,1,0.1)*20
hist(xs)

cut_off_z = mean(xs) - 1.64 * sd(xs)

## non-parametric percentile
cut_off_perc = quantile(xs, probs= 0.05)
print(cut_off_perc)
print(cut_off_z)


hist(xs, main = paste("difference in cut-off 5% = ", round(cut_off_z - cut_off_perc,2), sep=""))
abline(v = cut_off_z, lwd=3, col="red") # z score in red
abline(v = cut_off_perc, lwd=3, col="blue") # non-parametric percentile in blue

###################################
## POPULATION AND SAMPLE THRESHOLDS
##################################
# in a population we know the real cut-offs
pop_mean = 20
pop_sd = 5
real_cut_off = pop_mean - 1.64 * pop_sd
print(real_cut_off)

# we can simulate samples and see what are the observed cut-offs
n = 100 # size of each sample (number of participants in my normative data)
k = 500 # number of simulated samples


samp_cut_offs = NULL
for (iK in 1:k){
  obs_values = rnorm(n, mean=pop_mean, sd = pop_sd)
  cut_off_obs = mean(obs_values) - 1.64 * sd(obs_values)
  samp_cut_offs[iK] = cut_off_obs
}

hist(samp_cut_offs, main= paste("distribution of sample cut-offs\n real cut-off=",real_cut_off, sep=""))

### IMPORTANT: we cannot trust cut-offs or thresholds from samples!
# (especially when sample size is low)



#######################################################
## TYPE 1 ERROR USING CUT-OFF WITH Z-SCORES
######################################################
# in a population we know the real cut-offs
pop_mean = 20
pop_sd = 10
real_cut_off = pop_mean - 1.64 * pop_sd

# we can simulate samples and see what are the observed cut-offs
n = 20 # size of each sample (number of participants in my normative data)
k = 1000 # number of simulated samples
p = 1000 # number of simulated tested participants from normal population

# for each sample I test p participants, if my method is accurate, I will classify them
# below the cut-off 5% of the time (by definition, this is my definition). The sample cut-off will be different, however,
# and I can classify my miscalculation of patient for each sample.

samp_cut_offs = NULL
samp_below_cut_off = NULL
pop_below_cut_off = NULL
for (iK in 1:k){
  obs_values = rnorm(n, mean=pop_mean, sd = pop_sd)
    samp_cut_off = mean(obs_values) - 1.64 * sd(obs_values)
  
  # simulate participants (from population) and test against sample cut-off and then against population cut-off
  ps = rnorm(p, mean = pop_mean, sd = pop_sd)
  samp_below_cut_off[iK] = sum(ps < samp_cut_off) / p
  pop_below_cut_off[iK] = sum(ps < real_cut_off) / p
  
}


par(mfrow=c(1,2))
tot_range = range(c(samp_below_cut_off, pop_below_cut_off))
hist(samp_below_cut_off, breaks=10, xlim=tot_range, main="percentage of participants below cut-offs\n using sample z-scores")
hist(pop_below_cut_off, breaks=10, xlim=tot_range, main="percentage of participants below cut-offs\n using population cut-off")

# note that if sample size is low error when using population approximate expected error (0.05 = 5%)
# however this simulation show what will happen when using k samples. 
# If k = 1, the error can be much higher.

mean(samp_below_cut_off)
mean(pop_below_cut_off)


#######################################################
## TYPE 1 ERROR USING CUT-OFF WITH PERCENTILES
######################################################
# in a population we know the real cut-offs
pop_mean = 100
pop_sd = 10
real_cut_off = pop_mean - 1.64 * pop_sd

# we can simulate samples and see what are the observed cut-offs
n = 200 #size of each sample (number of participants in my normative data)
k = 1000 # number of simulated samples
p = 1000 # number of simulateted tested participants from normal population

# for each sample I test p participants, if my method is accurate, I will classify them
# below the cut-off 5% of the time (by definition, this is my definition). The sample cut-off will be different, however,
# and I can classify my miscalculation of patient for each sample.

samp_cut_offs = NULL
samp_below_cut_off = NULL
pop_below_cut_off = NULL
for (iK in 1:k){
  obs_values = rnorm(n, mean=pop_mean, sd = pop_sd)
  samp_cut_off = quantile(obs_values, prob=0.05)

  # simulate participants (from population) and test against sample cut-off and then against population cut-off
  ps = rnorm(p, mean = pop_mean, sd = pop_sd)
  samp_below_cut_off[iK] = sum(ps < samp_cut_off) / p
  pop_below_cut_off[iK] = sum(ps < real_cut_off) / p
  
}

par(mfrow=c(1,2))
tot_range = range(c(samp_below_cut_off, pop_below_cut_off))
hist(samp_below_cut_off, breaks=10, xlim=tot_range, main="percentage of participants below cut-offs\n using sample percentile")
hist(pop_below_cut_off, breaks=10, xlim=tot_range, main="percentage of participants below cut-offs\n using population cut-off")

# note that if sample size is low error when using population approximate expected error (0.05 = 5%)
# however this simulation show what will happen when using k samples. 
# If k = 1, the error can be much higher.

mean(samp_below_cut_off)
mean(pop_below_cut_off)

### IMPORTANT: we cannot trust cut-offs or thresholds from samples!
# (especially when sample size is low)





########################
# ADDITIONAL MATERIALS #
########################




### Population -> samples figure (as in slides)
zs = seq(-3, 3, 0.01)
x_mean = 20
x_sd = 5
xs = (zs  * x_sd) + x_mean

ys = dnorm(xs, mean = x_mean, sd = x_sd)

#
png("Figures/population.png", height = 800, width=1000, res=300)
plot(xs, ys, type="l", main= round(x_mean -1.64 * x_sd) )
dev.off()

# set numerosity of drawn samples
n_s = 25

png("Figures/samples.png", height = 800, width=1000*3, res=350)
par(mfrow=c(1,3))
set.seed(100)
r_1 = rnorm(n_s, mean = x_mean, sd = x_sd)
hist(r_1, main = round(mean(r_1)-1.64*sd(r_1)))
#hist(r_1, main = round(quantile(r_1, probs=0.05)))

r_2 = rnorm(n_s, mean = x_mean, sd = x_sd)
hist(r_2, main = round(mean(r_2)-1.64*sd(r_2)))
#hist(r_2, main = round(quantile(r_2, probs=0.05)))

r_3 = rnorm(n_s, mean = x_mean, sd = x_sd)
hist(r_3, main = round(mean(r_3)-1.64*sd(r_3)))
#hist(r_3, main = round(quantile(r_3, probs=0.05)))

dev.off()


