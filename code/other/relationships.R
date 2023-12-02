x = (1:100)
y =  x

png("../Figures/relationship.png", res=200, height=400, width=400*3)
par(mfrow=c(1,3))
plot(x, 0.5*x, type="l", main="y= 0.5 * x", ylab="y")
plot(x, x^2, type="l", main = "y = y^2", ylab="y")
plot(x, 1/x, type="l", main = "y = 1/x ", ylab="y")
dev.off()



x = seq(0, 1, 0.01)
png("../Figures/ROC.png", res=120, height=400, width=400*2)
par(mfrow=c(1,2))
plot(x, (100-1/x)/100, type="l", main = "Optimal ROC curve", ylab="Sensitivity", xlab="1-Specificity")
plot(x, x, type="l", main = "Random Classifier", ylab="Sensitivity", xlab="1-Specificity")
dev.off()
