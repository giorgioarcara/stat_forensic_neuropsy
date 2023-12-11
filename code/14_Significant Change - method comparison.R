# 
rm(list=ls())
library(MASS)
source("R_functions/CGcut.off.bv.R")
#source("R_functions/CGcut.off.bv_table.R")

n.sim.train = 100

# paramters for normative sample
sim_prac = 0
n_size = c(5, 10, 20, 50)
corr = 0.5
err = 1

n_methods = 4 #number of methods, RCI, mRCI, Portaccio, C&G for determining array dimension


MODEL_RES = array(NA, dim=c(length(n_size), n_methods))
colnames(MODEL_RES)=c("RCI", "mRCI", "P10-regr", "C&G06-regr")
row.names(MODEL_RES)=n_size

### LOOP OVER SAMPLE SIZE
for (iN in 1:length(n_size)){ 
  
  n = n_size[iN]
  
  RCI_res = NULL
  mRCI_res = NULL
  RCIp_res = NULL # RCI portaccio et al. 2010 version
  CG_res= NULL
  
  for (iSim in 1:n.sim.train){
    
    # STEP 1) BUILD
    # simulate predictor
    xt0xt1 = mvrnorm(n, mu=c(0,0), Sigma=matrix(c(err,corr,corr,err), nrow=2))
    xt0 = xt0xt1[,1]
    xt1 = xt0xt1[,2] + sim_prac
    
    #plot(xt0, xt1)
    
    # calculate SEdiff
    s1 = sd(xt0)
    r = cor(xt0, xt1)
    SE=s1*(sqrt(1-r))
    SEdiff=sqrt(2*(SE^2))
    p_eff = mean(xt1)-mean(xt0)
    
    # C & G
    dat = data.frame(xt0, xt1)
    mod = lm(xt1~xt0, dat)
    
    ## STIMULATE TEST DATA
    n_test=100
    
    xt0xt1_test = mvrnorm(n_test, mu=c(0,0), Sigma=matrix(c(err,corr,corr,err), nrow=2))
    xt0_test = xt0xt1_test[,1]
    xt1_test = xt0xt1_test[,2] + sim_prac
    dat_test = data.frame(xt0=xt0_test, xt1=xt1_test)
    
    #plot(xt0_test, xt1_test)
    
    
    ## RCI
    z = (xt1_test-xt0_test)/SEdiff
    RCI_res[iSim] = sum(z <= -1.645)
    
    ## MRCI (modified with pract eff)
    mz = ((xt1_test-xt0_test) - p_eff)/SEdiff
    mRCI_res[iSim] = sum(mz <= -1.645)
    
    # RCI portaccio et al. 2010 version (p. 613)
    pred_xt1 = predict(mod, newdata=list(xt0=xt0_test))
    mod.err = summary(mod)$sigma
    pz = (xt1_test - pred_xt1)/mod.err
    RCIp_res[iSim] = sum(pz <= -1.645)
    
    ps = NULL
    for (iTest in 1:dim(dat_test)[1]){
      res = CGcut.off.bv(controls_data = dat, model= mod, pred = dat_test$xt0[iTest], Yobs = dat_test$xt1[iTest])
      ps[iTest]= res$p.obs
    }
    CG_res[iSim] = sum(ps < 0.05)
    
  }
  
  # total subject tested
  RCI_perf = sum(RCI_res)/(n.sim.train*n_test)*100
  
  mRCI_perf = sum(mRCI_res)/(n.sim.train*n_test)*100
  
  RCIp_perf = sum(RCIp_res)/(n.sim.train*n_test)*100
  
  CG_perf= sum(CG_res)/(n.sim.train*n_test)*100
  
  
  MODEL_RES[iN, ] = c(RCI_perf, mRCI_perf, RCIp_perf, CG_perf)
  
}

#save(MODEL_RES, file="MODEL_RES_pract.RData")
print(MODEL_RES) # expected is 5%, for each combination



