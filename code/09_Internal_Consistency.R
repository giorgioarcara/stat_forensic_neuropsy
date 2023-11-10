library(readxl)
library(psych)

rm(list=ls())

items_bin = read.table(file="Example_data/items_bin.txt", sep="\t")


# this code obtain proportion of people (rows) obtaining 1
p = apply(items_bin, 2, function(x){sum(x)/length(x)})
# this code obtain proportion of people (rows) obtaining 0
q = 1-p

# calculate number of items
n = dim(items_bin)[2]

# calculate total of the score
tot = apply(items_bin, 1, sum)

# calculate variance of the test
s2_tot = var(tot)

# calculate KR-20 for binary items
KR20_items_bin = (n / (n-1)) * ( (s2_tot - sum(p*q)) / s2_tot)
print(KR20_items_bin)

# here I wrap up a function
# it is the same as above, but with the function the code is handier
KR20_fun <- function(items) {
  p = apply(items, 2, function(x){sum(x)/length(x)})
  q = 1-p
  n = dim(items_bin)[2]
  tot = apply(items, 1, sum)
  s2_tot = var(tot)
  KR20 = (n / (n-1)) * ( (s2_tot - sum(p*q)) / s2_tot)
  return(KR20)
}

KR20_fun(items_bin)

###################
# CRONBACH'S ALPHA
###################
items = read.table(file="Example_data/items.txt", sep="\t")
# I remove the column with subject names for simplicity
items = items[,-1]

n = dim(items)[2]
tot = apply(items, 1, sum)
s2_tot = var(tot)
s2 = apply(items, 2, var)
  
alpha_items = (n / (n-1)) * ( (s2_tot - sum(s2)) / s2_tot)
print(alpha_items)

# create here a function for alpha
alpha_fun = function(items){
  
  n = dim(items)[2]
  tot = apply(items, 1, sum)
  s2_tot = var(tot)
  s2 = apply(items, 2, var)
  
  alpha_items = (n / (n-1)) * ( (s2_tot - sum(s2)) / s2_tot)
  return(alpha_items)
  
}

alpha_fun(items)

# compare with the alpha function from psych package
alpha(items)


## NOTE: alpha is higher as the number of items increases
alpha_res = alpha(items)
print(alpha_res$total$raw_alpha) #alpha for all items

# in the for loop that follows I recalculate alpha dropping everytime one item
# Note that when removing items, alpha is almost always smaller.
for (i in 1:dim(items)[2]){
  alpha_res = alpha(items[,-i])
  print(alpha_res$total$raw_alpha)
}


# note what happen if add a columns (identical to one of the previous)
items2 = cbind(items, items$Item_1)
alpha_fun(items)
alpha_fun(items2) # alpha increases

## there are also other alternatives, like omega or GLB (gretaest lower bound)
# Note that they are associated with factor analysis.
omega(items) #
glb(items)
