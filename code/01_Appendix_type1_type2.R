#####################
# LOWER TAIL  ONLY: p(z) ######
#####################
 rm(list=ls())

#### GENERATE SAMPLE OF HEALHTY INDIVIDUALS
zobs_1 = -1.64
mu_1 <- 20
sigma_1 <- 5
step_1 <- 1000


#### GENERATE SAMPLE OF PATIENTS
zobs_2 = -1.64
mu_2 <- 10
sigma_2 <- 5
step_2 <- 1000

min_mu = min(mu_1, mu_2)
max_mu = max(mu_1, mu_2)
min_sigma = min(sigma_1, sigma_2)
max_sigma = max(sigma_1, sigma_2)

bounds <- c(min_mu-3*max_sigma, max_mu+3*max_sigma)



# colored area
#cord.x <- c(upper.x,seq(upper.x,lower.x,step),lower.x)
#cord.y <- c(0,dnorm(seq(upper.x,lower.x,step),mu,sigma),0)
# plot_title
#plot_title = paste("prob = ", round( pnorm(zobs_1, mean=mu_1, sd = sigma_1, lower.tail=T), 2))

c1 = curve(dnorm(x,mu_1,sigma_1),xlim=bounds) 
c2 = curve(dnorm(x,mu_2,sigma_2),xlim=bounds, add=TRUE) 

find_intersection = function(x1, x2, y1, y2, tolx = 0.001, toly=tolx, exclude.edge=T){
  inter = which(abs(x1-x2)<tolx & abs(y1-y2)<toly)
  if (exclude.edge){
    inter = inter[-(c(1, length(inter)))]
  }
return(inter)  
}

inters = find_intersection(c1$x, c2$x, c1$y, c2$y)
abline(v = c1$x[inters])

#poly2_left = polygon(c(c2$x[1:inters], c2$x[inters]), c(c2$y[1:inters], 0), col='skyblue') 
#poly1_right = polygon(c(c1$x[inters], c1$x[inters:length(c1$x)]), c(0, c1$y[inters:length(c1$y)]), col='red') 

#############################
# PLOT SEVERAL DISTRIBUTIONS
#

#####################
# LOWER TAIL  ONLY: p(z) ######
#####################
rm(list=ls())

#### GENERATE SAMPLE OF HEALHTY INDIVIDUALS
mu_1 <- 20
sigma_1 <- 5
step_1 <- 1000


#### GENERATE SAMPLE OF PATIENTS
mu_2 <- seq(2, 10, length.out=10)
sigma_2 <- rep(5, length(mu_2))
step_2 <- 1000
col_2 = rgb(0.8, 0.8, 0, alpha = seq(0.1, 1, length.out=length(mu_2)))

min_mu = min(mu_1, mu_2)
max_mu = max(mu_1, mu_2)
min_sigma = min(sigma_1, sigma_2)
max_sigma = max(sigma_1, sigma_2)

bounds <- c(min_mu-3*max_sigma, max_mu+3*max_sigma)



# colored area
#cord.x <- c(upper.x,seq(upper.x,lower.x,step),lower.x)
#cord.y <- c(0,dnorm(seq(upper.x,lower.x,step),mu,sigma),0)
# plot_title
#plot_title = paste("prob = ", round( pnorm(zobs_1, mean=mu_1, sd = sigma_1, lower.tail=T), 2))

png("Figures/Healthy_vs_Pathological.png", res=150, width=1400, height=1000)
c1 = curve(dnorm(x,mu_1,sigma_1),xlim=bounds, col="blue3", lwd=3, xlab="x", ylab="proportion of scores", main="Healthy Individuals (observed) vs \n Pathological individuals (hypothesized)")
crit_score_x = qnorm(0.05, mean=mu_1, sd=sigma_1)

coord.x <- c(c1$x[1], seq(c1$x[1],crit_score_x, 0.1),crit_score_x)
coord.y <- c(0,dnorm(seq(c1$x[1],crit_score_x, 0.1), mean=mu_1, sd =sigma_1),0)
polygon(coord.x,coord.y, col="gray")

for (iC in 1:length(mu_2)){
c2 = curve(dnorm(x,mu_2[iC],sigma_2[iC]),xlim=bounds, add=TRUE, col=col_2[iC], lwd=3) 
}

abline(v= crit_score_x, lwd=3)


legend("topright", legend=c("Healhty", "Pathological", "normality cut-off"), col=c("blue3", "yellow3", "black"), lwd=3)
dev.off()




  

