###############################
# SENSITIVITY /SPECIFIFICTY AND ROC ANALYSIS
###############################
rm(list=ls())
# In this script I simulate some data for sensitivity, Specificity and ROC analysis.
# to this aim I create two groups (Pathological and Patients), but note that this could be applied for any categorical distinction
# e.g. simulators, for non-simulators, etc.

### Simulate data
n_pat = 10 # number of Pathological individuals
n_contr = 100000 # number of Healthy (Controls) individuals

# simulate group of group of Pathologicals
pat_mean = 18
pat_sd = 2

# simulate group of Healthy
contr_mean = 25
contr_sd = 2

pat = rnorm(n_pat, mean=pat_mean, sd=pat_sd)
contr = rnorm(n_contr, mean=contr_mean, sd=contr_sd)

dat = data.frame(Score = c(pat, contr), Group=c(rep("Pathological", n_pat), rep("Healthy", n_contr)))
dat$Group = factor(dat$Group)

head(dat)

#########################
## PLOT HISTOGRAMS  #####
#########################

col_transp = rgb(0.3, 0.4, 0.8, alpha=0.2)

h1 = hist(dat[dat$Group=="Pathological", "Score"], plot=F)
h2 = hist(dat[dat$Group=="Healthy", "Score"], plot=F)
  
x_range = c( min(c(h1$breaks, h2$breaks)), max(c(h1$breaks, h2$breaks)))
y_range = c( min(c(h1$counts, h2$counts)), max(c(h1$counts, h2$counts)))

Path_scores = dat[dat$Group=="Pathological", "Score"]
Healthy_scores = dat[dat$Group=="Healthy", "Score"]

hist(Path_scores, main="Pathologicals vs Healthy", breaks=10, xlab="", density=10, angle=45, col="black",
     xlim=x_range, ylim=y_range)
par(new=TRUE)
hist(Healthy_scores, xlim=range_score, main="", breaks=10, xlab="", col=col_transp, add=TRUE)
legend("topright", legend=c("Pathologicals", "Healthy"), pt.bg=c("white", col_transp),  pch=c(22,22))

### ROC ANALYSIS

# library
library(pROC)

roc.dat = roc(dat$Group, dat$Score, ci=TRUE, ci.thresholds=TRUE,  percent=TRUE, direction=">")

plot(roc.dat, print.thres="best", print.thres.best.method="topleft")

plot(ci(roc.dat, of="thresholds", thresholds="best", best.method="topleft")) 

#### REPORT FP/FN, TP/TN WITH THE THRESHOLDS
res = ci(roc.dat, of="thresholds", thresholds="best", best.method="topleft")
threshold = as.numeric(rownames(res$sensitivity))

TP = sum(dat[dat$Group=="Pathological", "Score"] < threshold)
TN = sum(dat[dat$Group=="Healthy", "Score"] >= threshold)
FP = sum(dat[dat$Group=="Healthy", "Score"] < threshold)
FN = sum(dat[dat$Group=="Pathological", "Score"] >= threshold)

Sensitivity = TP/(TP + FN)
Specificity = TN/(TN + FP)
Accuracy = (TN + TP)/(TN+TP+FN+FP)

print(Sensitivity)
print(Specificity)
print(Accuracy)


### BASE RATE EFFECTS
# go back to the beginning lins of the code change the n_pat and n_contr at the beginning and see the base rate effect.
# (try to create and imbalance between Patients, and Controls).  Sensitivty and Negativity will not change,
# but PPV and NPV will

PPV = TP / (TP + FP)
NPV = TN / (TN + FN)

print(PPV)
print(NPV)


########################
## EFFECT OF SAMPLE/POPULATION
########################

### Simulate data from large sample (to assume it is population level values)
n_pat = 10000 # number of Pathological individuals
n_contr = 10000 # number of Healthy (Controls) individuals

# simulate group of group of Pathologicals
pat_mean = 18
pat_sd = 2

# simulate group of Healthys
contr_mean = 22
contr_sd = 2

pat = rnorm(n_pat, mean=pat_mean, sd=pat_sd)
contr = rnorm(n_contr, mean=contr_mean, sd=contr_sd)

dat = data.frame(Score = c(pat, contr), Group=c(rep("Pathological", n_pat), rep("Healthy", n_contr)))
dat$Group = factor(dat$Group)

roc.dat = roc(dat$Group, dat$Score, ci=TRUE, ci.thresholds=TRUE,  percent=TRUE, direction=">")
res = ci(roc.dat, of="thresholds", thresholds="best", best.method="topleft")
threshold = coords(roc.dat, ret="threshold", x="best", best.method="topleft")


# from this sample some participants (for sake of simplicity equal number)
n_pat_s = 200
n_contr_s = 200

k = 1000 # number of sample

pat_group = dat[dat$Group=="Pathological", ]
contr_group = dat[dat$Group=="Healthy", ]

thresholds_s = NULL
  
for (iK in 1:k){
  
pat_s = pat_group[sample(1:dim(pat_group)[1], n_pat_s), ]
contr_s = contr_group[sample(1:dim(contr_group)[1], n_contr_s), ]

dat_s = rbind(pat_s, contr_s)
roc.dat_s = roc(dat_s$Group, dat_s$Score, percent=TRUE, direction=">", verbose=F)
thresh_s = unlist(coords(roc.dat_s, ret="threshold", x="best", best.method="topleft"))
thresholds_s[iK] = thresh_s

}

hist(thresholds_s)

