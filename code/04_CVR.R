# install.packages("psych")
## CONTENT VALIDITY
rm(list=ls())
# According to Lawshe (1975) each item can be rated from an expert
# - essential
# - useful, but not essential
# - not necessary

# In the example below 

N = 10 # number of panelist
Ne = 5 # number of panelist that rated an Item as "essential" ()


CVR = (Ne - N/2) / (N/2)
print(CVR)

# let's load some example data
#install.packages("readxl")
library(readxl)
dat = as.data.frame(read_excel("Example_data/CVR_example.xlsx"))
# NOTE: you can change this example in excel, adding rater, or item, 
# or specifying difference responses to see what happens

Nes = apply(dat, 1, function(x)sum(x=="essential"))
N = dim(dat)[2] -1 # the number of raters is the number of columns minus.1 (the first is the column with item names)

CVRs = (Nes - N/2) / (N/2)
print(CVRs)

# select according to Lawshe (1975). see paper or slides for table reference to select the correct threshold
threshold = 0.99
CVRs_sel = CVRs[CVRs>threshold]
# adjust all CVR to 0.99 (this is acccording to Lawshe suggestion)
CVRs_sel[CVRs_sel==1]=0.99

# CVI of the overall test
CVI = mean(CVRs_sel)

print(CVI)


