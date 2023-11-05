# install.packages("psych")
library(psych)

n = 100
x = rnorm(100, mean = 20, sd = 5)
y = x + rnorm(n, mean=0, sd = 10) 

cor.test(x,y)

mat = cbind(x,y)

# the one to be used is ICC 2 (that takes into account absolute agreement)
ICC(mat)

# here I create a version with a systematic bias in the scores (score in 2)
# are systematically higher but a precise amount
mat_bias = mat
mat_bias[,2]= mat_bias[,2] + 4

# check results when a systematic bias is present in the different versions of ICC
ICC(mat_bias)

# check what happen in the correlations (nothing)
cor.test(mat[,1], mat[,2])
cor.test(mat_bias[,1], mat_bias[,2])





