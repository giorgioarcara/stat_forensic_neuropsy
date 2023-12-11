###############################
# MALINGERING
###############################
rm(list=ls())


## test based on unlikely performance under Random responses
# suppose you have a forced choice test.

n_items = 50
cut_off_rand_success = qbinom(p = 0.05, prob=0.5, size = n_items)


# probability to obtain lesss thatn cut_off_rand_success in n_items
pbinom(cut_off_rand_success, prob=0.5, size = n_items)

# confidence interval around. Is the observed probability different from 0.5? (that is random choice)
prop.test(cut_off_rand_success, n_items, p=0.5, corr=F)


##
# below I simulate what happens if I simulate k patients that are actually responding randomly
k = 1000
res = rbinom(k, n_items, prob=0.5)
  
hist(res, main="distribution of successes\n under random response of n_items")
empirical_cut_off = quantile(res, prob=0.05)
abline(v=empirical_cut_off, lwd=2)

print(cut_off_rand_success)
print(empirical_cut_off)





