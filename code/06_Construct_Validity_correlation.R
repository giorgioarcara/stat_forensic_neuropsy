# install.packages("faux")
rm(list=ls())
library(faux)
library(corrplot)
library(languageR)


tests = read.table(file = "Example_data/tests.txt", sep="\t")

# the most traditional way to inspect construct validity is through correlation in tests
cor(tests)

# with correlation, visualization is fundamental to detect potential issues.
corrplot(cor(tests))
corrplot(cor(tests), method="number")


# to properly inspect correlatios, scatterplot is the best kind of plot
plot(tests$Semantic_Fluency, tests$Phonemic_Fluency)
cor.test(tests$Semantic_Fluency, tests$Phonemic_Fluency)

pairs(tests)
pairscor.fnc(tests) # from languageR package

# Note that scatterplot can unveil important details on the data
# (and they are often missing from articles/papers)
plot(tests$TMT_B, tests$TMT_A)
cor.test(tests$TMT_B, tests$TMT_A)



# remember that the sample size is crucial to understand how much we can trust our result
# see script (03_Correlation.R for simulation on the difference between sample and population correlation)
dim(tests)
# data are from 67 participants


