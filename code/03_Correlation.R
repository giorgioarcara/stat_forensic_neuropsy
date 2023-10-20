# install.packages("faux")
library(faux)
n = 100

x = rnorm(n)
y = rnorm(n)

# correlation 
cor(x,y)

# correlation from covariance and standard deviation
( r = cov(x, y)/(sd(x)*sd(y)) )

# correlation from full formula
r =  (sum((x - mean(x)) * (y - mean(y))) ) / ( (sqrt( sum((x - mean(x))^2))) * sqrt( sum((y - mean(y))^2)) )
print(r)



#################
# DATA SIMULATION
#################

n = 30

x = rnorm(n)
y = rnorm(n)

n.rep = 20
t_r = 0.4

Rs = NULL
for (iE in 1:n.rep){
res = rnorm_multi(n, mu=c(20, 20), r = t_r)
Rs[iE] = cor(res$X1, res$X2)
}

mean(Rs)
hist(Rs, xlim=c(-1,1), breaks=100)
range(Rs)

