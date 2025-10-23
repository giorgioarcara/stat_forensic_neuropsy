simulate_obs = function(Ts, n.obs, Mean.T, E.sd, Mean.E.sd, label="True - observed relationship", plot_thresh=FALSE){
  
  # data simulation
  Xs = NULL
  for (iE in 1:n.obs){
    E = rnorm(1, mean = 0, sd = E.sd)
    Xs[iE] = Ts + E
  }
  
  # set cut-off as compared to the hypothetical Population mean (Mean.T)
  cut_off = Mean.T-1.64*Mean.E.sd
  ylims = c(Mean.T-3*Mean.E.sd, Mean.T+3*Mean.E.sd)
  
  print(Xs)
  
  # calculate mean of observed scores (the larger the number of repeated measurement, the better the approximation to the True score)
  #mean(Xs) 
  
  # histogram of observed scores
  #hist(Xs) 
  
  
  # set point bg (if thre)
  if (plot_thresh){
    curr_bg=ifelse(Xs<=cut_off, "black", "white")
  } else {
    curr_bg="white"
  }
  
  # plot observed scores as compared to True Score
  plot(1:n.obs, Xs, ylim=ylims, pch=21, bg=curr_bg)
  lines(1:n.obs, Xs)
  abline(h=Ts)
  if (plot_thresh){
    abline(h=cut_off, lty=2) 
  }
  
  png(paste0("Figures/Figure_true_obs_", E.sd, ".png"), res=200, height=800, width=1200)
  plot(1:n.obs, Xs, ylim=ylims, pch=21, bg=curr_bg)
  lines(1:n.obs, Xs)
  abline(h=Ts)
  if (plot_thresh){
    abline(h=cut_off, lty=2) 
  }
  dev.off()
  
}