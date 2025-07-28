###############################
# SENSITIVITY /SPECIFIFICTY AND ROC ANALYSIS
###############################
rm(list=ls())
library(manipulate)
library(pROC)

# library
# In this script I simulate some data for sensitivity, Specificity and ROC analysis.
# to this aim I create two groups (Pathological and Patients), but note that this could be applied for any categorical distinction
# e.g. simulators, for non-simulators, etc.
# a slider is created to inspect change in Sensitivy/Specificity according to different cut-offs

n_pat = 50
pat_mean=20
pat_sd = 2

n_contr = 50
contr_mean = 24
contr_sd = 2


pat = rnorm(n_pat, mean=pat_mean, sd=pat_sd)
contr = rnorm(n_contr, mean=contr_mean, sd=contr_sd)

dat = data.frame(Score = c(pat, contr), Group=c(rep("Pathological", n_pat), rep("Healthy", n_contr)))
dat$Group = factor(dat$Group)

roc.dat = roc(dat$Group, dat$Score, ci=TRUE, ci.thresholds=TRUE,  percent=TRUE, direction=">")




#########################
## PLOT HISTOGRAMS  #####
#########################

plot_hist = function(threshold=NULL, plot_roc=FALSE){

  
  col_transp = rgb(0.3, 0.4, 0.8, alpha=0.2)
  
  
  #### REPORT FP/FN, TP/TN WITH THE THRESHOLDS
  res = ci(roc.dat, of="thresholds", thresholds=threshold, best.method="topleft")
  #threshold = as.numeric(rownames(res$sensitivity))
  
  auc=roc.dat$auc
  
  TP = sum(dat[dat$Group=="Pathological", "Score"] < threshold)
  TN = sum(dat[dat$Group=="Healthy", "Score"] >= threshold)
  FP = sum(dat[dat$Group=="Healthy", "Score"] < threshold)
  FN = sum(dat[dat$Group=="Pathological", "Score"] >= threshold)
  
  Sensitivity = TP/(TP + FN)
  Specificity = TN/(TN + FP)
  Accuracy = (TN + TP)/(TN+TP+FN+FP)
  
  
  h1 = hist(dat[dat$Group=="Pathological", "Score"], plot=F)
  h2 = hist(dat[dat$Group=="Healthy", "Score"], plot=F)
  
  x_range = c( min(c(h1$breaks, h2$breaks)), max(c(h1$breaks, h2$breaks)))
  y_range = c( min(c(h1$counts, h2$counts)), max(c(h1$counts, h2$counts)))
  
  Path_scores = dat[dat$Group=="Pathological", "Score"]
  Healthy_scores = dat[dat$Group=="Healthy", "Score"]
  
  title = paste("Specificity = ", round(Specificity,2), ", Sensitivity = ", round(Sensitivity,2), "\nAUC = ", auc, sep="")
  
  if (plot_roc){
    par(mfrow=c(1,2))
    plot(roc.dat, print.thres=threshold, main="ROC curve")
  }
  
  hist(Path_scores, main=title, breaks=10, xlab="", density=10, angle=45, col="black",
       xlim=x_range, ylim=y_range)
  par(new=TRUE)
  hist(Healthy_scores, xlim=range_score, main="", breaks=10, xlab="", col=col_transp, add=TRUE)
  legend("topright", legend=c("Pathologicals", "Healthy"), pt.bg=c("white", col_transp),  pch=c(22,22))
  abline(v=threshold, lwd=2)
  
}

manipulate(
  plot_hist(threshold=cutoff, plot_roc=TRUE),
  cutoff = slider(0,30, 15)
)

########










