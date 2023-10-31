# install.packages("faux")
rm(list=ls())
library(faux)

## FORMULAS
n = 100
x = rnorm(100, mean = 20, sd = 5)
y = x + rnorm(n, mean=0, sd = 5) # you can simulate a  variable y correlated to x, by adding noise (N(0, sd)) to x. c

# Pearson's correlation from covariance and standard deviation
r = cov(x, y)/(sd(x)*sd(y))
print(r)

# Pearson's correlation from full formula
r =  (sum((x - mean(x)) * (y - mean(y))) ) / ( (sqrt( sum((x - mean(x))^2))) * sqrt( sum((y - mean(y))^2)) )
print(r)

# Pearsons' correlation via z-scores
Zx = scale(x) #( x - mean(x) ) / sd(x)
Zy = scale(y) #( y - mean(y) ) / sd(y)

n = length(x) # or length(y)

r = sum(Zx*Zy) / (n-1)

print(r)

# Pearson's correlation, statistical test
cor.test(x, y)

# scatterplot (standard figure to show correlations)
plot(x, y, main=paste("r = ", round(r, 2)))


## SIMULATE DATA WITH GIVEN CORRELATION
# sim parameters
n = 10 # number of observations from samples (note: n1 must be equal to n2)
mus=c(20,20) # mean of populations from which samples are drawn 
t_r = 0.3 # true underlying correlation
# rnorm_multi (from faux package) simulate random observation from populations with given correlation
res = rnorm_multi(n, mu=c(20, 20), r = t_r)

plot(res$X1, res$X2, main=paste("r = ", round(cor(res$X1, res$X2), 2)))

## SPEARMAN CORRELATION
cor.test(res$X1, res$X2, method="spearman")

# spearman correlation is simply correlation between ranks of raw variables
cor.test(rank(res$X1), rank(res$X2))

## EFFECT OF OUTLIERS ON CORRELATION
# in the lines below I create a duplicate of res and then I add an outliers
# multiplying by 5 the largest value for X1 (see plot)
res_out=res
res_out$X1[which(res_out$X1 == max(res_out$X1))] = res$X1[which(res_out$X1 == max(res_out$X1))] * 5

plot(res_out$X1, res_out$X2, pch=22)
points(res$X1, res$X2, pch=1, cex=1.5)
legend("bottomright", pch=c(1, 22), legend=c("original", "with outlier"))

r_noout = cor.test(res$X1, res$X2)
rho_noout = cor.test(res$X1, res$X2, method="spearman")
r_out = cor.test(res_out$X1, res_out$X2)
rho_out = cor.test(res_out$X1, res_out$X2, method="spearman")

# the following lines show the results with r (Pearson) and rho (Spearman) correlations
# with and without outliers
cat(paste("r without outliers=", round(r_noout$estimate, 2), 
"\nrho with outliers=", round(rho_noout$estimate,2),
"\nr with an outlier=", round(r_out$estimate, 2),
"\nrho with an outlier", round(rho_out$estimate,2)))

# note that Spearman's correlation is not affected by outliers.
#####################################################################################
# DATA SIMULATION. PEARSON'S CORRELATION OF SAMPLE, GIVEN KNOWN CORRELATION OF POPULATION
#####################################################################################

n = 20
n.rep = 20
t_r = 0.4

Rs = NULL
for (iE in 1:n.rep){
res = rnorm_multi(n, mu=c(20, 20), r = t_r)
Rs[iE] = cor(res$X1, res$X2)
}

mean(Rs)
hist(Rs, xlim=c(-1,1))
range(Rs)





