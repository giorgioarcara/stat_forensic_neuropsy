CGcut.off.bv_table<-function(controls_data=NULL, model = NULL, preds_df = NULL,  Yobs = NULL, p.crit=0.05, upper=FALSE){

  # ver 0.2, 21/08/2020
  
  # TABLE OF MULTIVARIATE VERSION OF CG CUT-OFF
  
# preds is a list including all variables.
# e.g. list(t0_scoure=seq(20, 80, 10))
  
  if (!is.data.frame(preds_df)){
    stop("preds_df must be a data.frame")
  }

  results=preds_df
  
  for(iPred in 1:dim(preds_df)[1]){
    results$Score[iPred] = CGcut.off.bv(controls_data, model = model, pred = preds_df[iPred,], Yobs = NULL, p.crit=p.crit, upper=upper)$Y_obs_crit
  }
  
  
 return(results) 
  
  
}
  