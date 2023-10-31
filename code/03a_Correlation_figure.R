library(faux)

png("Figures/correlations_figure.png", res=100)

n = 200 # number of observations from samples (note: n1 must be equal to n2)
mus=c(20,20) # mean of populations from which samples are drawn 
t_r = 0 # true underlying correlation
# rnorm_multi (from faux package) simulate random observation from populations with given correlation
res = rnorm_multi(n, mu=c(20, 20), r = t_r)

#plot(res$X1, res$X2, main=paste("r = ", round(cor(res$X1, res$X2), 1)), xlab="X", ylab="Y")
plot(res$X1, -res$X1, main="r = -1", xlab="X", ylab="Y")
dev.off()
