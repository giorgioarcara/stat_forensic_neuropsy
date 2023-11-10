# install.packages("psych")
# install.packages("readxl")
# install.packages("corrplot")
# install.packages("psych")

library(readxl)
library(corrplot)
library(psych)

## FACTOR ANALYSIS
rm(list=ls())

#################################
## FACTOR ANALYSIS FOR ITEMS
#################################
items = read.table(file="Example_data/items.txt", sep="\t")

# first inspect the correlation matrix
cor(items[,-1])
corrplot(cor(items[,-1]), method = "number")

# note that typically in factor analysis there are additional preliminary steps: bartlett test and KMO
# they are prerequisites to factor analysis.

# BARTLETT TEST
# this test check whether the correlation matrix is significantly is an identity matrix (all 0 except 1 in the diagnonal)
# If significant we can go on with the factor analysis
cortest.bartlett(items[,-1])

# KMO TEST
# this test check teh adequacy of sample size for factor analysis.
# Ideally in factor analysis we want an adequate ratio between columns and rows in the data matrix
KMO(items[,-1])

?KMO # check in the help if it is adequate


## FACTOR ANALYSIS (Exploratory)
items_fact_1 = factanal(items[, -1], factors=1)
print(items_fact_1)

# here the Chi square is significant, hence 1 factor is not sufficient

items_fact_2 = factanal(items[, -1], factors=2)
print(items_fact_2)
# here the Chi square is  not significant, hence 2 factors are sufficient

# typically to select the number of factors you use scree plot, selecting 
# the first solution after a drop (in this case, 2 factors)
scree(items[,-1], pc=F)

# you can plot the loadings to better interpret the results
loads = items_fact_2$loadings
plot(loads, type="n")
text(loads,labels=names(items[,-1]),cex=.7)

# you can also plot a diagram
fa.diagram(loads)

# here I am using a difference function for factor analysis (from psych package)
items_fa_1 = fa(items[,-1], nfactors=1)
print(items_fa_1)

items_fa_2 = fa(items[,-1], nfactors=2)
print(items_fa_2)

# results are similar but the algorithms used are different so they are not equal.

#################################
## FACTOR ANALYSIS FOR TESTS
#################################
tests = read.table(file = "Example_data/tests.txt", sep="\t")

# first inspect the correlation matrix
cor(tests)
corrplot(cor(tests), method = "number")

# BARTLETT TEST
cortest.bartlett(tests)

# KMO TEST
KMO(tests)

?KMO # check in the help if it is adequate

## FACTOR ANALYSIS (Exploratory)
tests_fact_1 = factanal(tests, factors=1)
print(tests_fact_1)

# here the Chi square is significant, hence already 1 factor is sufficient

tests_fact_2 = factanal(tests, factors=2)
print(tests_fact_2)
# here the Chi square is also not significant.

# typically to select the number of factors you use scree plot, selecting 
# the first solution after a drop (in this case, 2 factors)
scree(tests, pc=F)

# you can plot the loadings to better interpret the results
loads = tests_fact_2$loadings
loads

plot(loads, type="n")
text(loads,labels=names(tests),cex=.7)

# you can also plot a diagram
fa.diagram(loads)
# NOTE that one default argument is simple = TRUE and only the highest loading on a factor is shown
fa.diagram(loads, cut=0.3, simple=F)


# other hints can be found here https://rpubs.com/pjmurphy/758265

# here I am using a difference function for factor analysis (from psych package)
tests_fa_1 = fa(tests, nfactors=1)
print(tests_fa_1)

tests_fa_2 = fa(tests, nfactors=2)
print(tests_fa_2)

# results are similar but the algorithms used are different so they are not equal.

## EXAMPLE PCA
tests = prcomp(tests, scale=TRUE)
print(tests$rotation)


