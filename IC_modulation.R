##IC Module Analysis
preg=0.7
pbad.reg=0.1
pgood.reg=1-pbad.reg
#
pirreg=1-preg
pbad.irreg=0.4
pgood.irreg=1-pbad.irreg

sim.ICmodule = function(n=10){
  simulated.modules = rep(NA,n)
  labels = sample(c(-1,1),n,
                  prob=c(pirreg,preg),
                  replace=T)
  
  if(any(labels==1)){
    simulated.modules[which(labels==1)]=
      sample(c("goodreg","badreg"), #從reg中拿出來是好(good)的或壞(bad)的
             sum(labels==1),
             prob=c(pgood.reg,pbad.reg),
             replace=TRUE) 
  }
  if(any(labels==-1)){
    simulated.modules[which(labels==-1)] = 
      sample(c("goodirreg","badirreg"),
             sum(labels==-1),
             prob=c(pgood.irreg,pbad.irreg),
             replace=T)
  }
  simulated.modules
}

sim.ICmodule()
S=10000
sim.table = replicate(S,sim.ICmodule())
dim(sim.table)


