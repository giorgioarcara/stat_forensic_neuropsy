CGcut.off.bv<-function(controls_data=NULL, model = NULL, pred = NULL,  Yobs = NULL, p.crit=0.05, upper=FALSE){
  
  # ver 0.2, 21/08/2020
  
  # BIVARIATE VERSION OF CG CUT-OFF
  
  # - controls_data = a vector or data.frame with the data from controls (each row a participant, each col a variable)
  # - model = a lm object (i.e., a linear model fitted with lm) for the data correction
  # - pred = a value with the predicted
  # - Yobs = OPTIONAL, the observed y. If not specified the function return the critical t and Y associated with the p.crit
  # - p.crit = the critical p.values. It is used only if Yobs is not specified.
  
  
  #######################
  # PERFORM SOME CHECKS #
  #######################
  if (is.null(model)){
    stop("you should specify a regression model (model), calculated on controls data")
  }
  
  if (is.null(controls_data)){
    stop("you should specify a the controls data, on which the model is calculated")
  }
  
  if (is.null(pred)){
    stop("you should include the values of the predictor")
  }
  
  if (length(pred)!=1){
    stop("you can specify only one value for the predictor (use CG.cut.off.table.bv for more values")
  }
  
  # change in data.frame if it is not
  if (!(is.data.frame(controls_data)|is.numeric(controls_data))){
    stop("controls data must be either numeric or a data.frame")
  }
  
  if (is.numeric(controls_data)){
    controls_data=data.frame(contr_pred=controls_data)
    names(controls_data)=names(coef(model))[2]
  }


  sigma=summary(model)$sigma #residual standard error. Nella formula di C & G (Eq.6) è Sy.x
  
  n=dim(controls_data)[1] # sample numerosity
  
  var1 = as.numeric(controls_data[,1]) # variable 1
  
  
  Sn_1=sigma*(sqrt( 1 + ( 1/n ) + ( (pred-mean(var1))^2 / ( var(var1) * (n-1) ) )))
  
  
  # Trovo il t-critico e quindi il risultato 
  newdata = data.frame(pred)
  names(newdata)[1]=names(coefficients(model))[2]
  
  Y_pred=predict(model, newdata=newdata) 
  df=(n-2) 
  t.crit=qt(p=p.crit, df=df)
  
  
  if (upper==TRUE){
    t.crit = -t.crit
  }
  
  # the following is the formula (3) from C & G, inverted.
  Y_obs_crit = (t.crit*Sn_1)+Y_pred
  
  t.obs = (Yobs - Y_pred)/Sn_1
  
  
  if (upper==FALSE){
    p.obs=pt(t.obs, df = df) # taken from p.262 line 5.
  }
  
  if (upper==TRUE){
    p.obs=1-pt(t.obs, df = df)
  }
  
  
  ##################
  # create results #
  ##################
  
  if (!is.null(Yobs)){
    res = data.frame(Yobs = Yobs, t.obs=t.obs,  df = df, p.obs = p.obs, t.crit=t.crit, p.crit=p.crit)
  } 
  
  if (is.null(Yobs)){
    res = data.frame(Y_obs_crit = Y_obs_crit, t.crit = t.crit, df=df, p.crit=p.crit)
  }
  return(res)
}