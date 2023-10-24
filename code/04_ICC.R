# install.packages("psych")
library(psych)

n = 100
x = rnorm(100, mean = 20, sd = 5)
y = x + rnorm(n, mean=0, sd = 10) 

cor.test(x,y)

mat = cbind(x,y)

# the one to be used is ICC 2.
ICC(mat)

mat_bias = mat
mat_bias[,2]= mat_bias[,2] + 4

ICC(mat_bias)

cor.test(mat_bias[,1], mat_bias[,2])

