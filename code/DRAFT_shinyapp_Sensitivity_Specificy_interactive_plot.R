### DRAFT! to do a shiny app.
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

find_intersection = function(x1, x2, y1, y2, tolx = 0.001, toly=tolx, exclude.edge=T){
  inter = which(abs(x1-x2)<tolx & abs(y1-y2)<toly)
  if (exclude.edge){
    inter = inter[-(c(1, length(inter)))]
  }
  return(inter)  
}


c1 = curve(dnorm(x,mu_1,sigma_1),xlim=bounds, ylab="Density") 
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

