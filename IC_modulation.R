##IC Module Analysis
preg=0.7
pbad.reg=0.1
pgood.reg=1-pbad.reg
#
pirreg=1-preg
pbad.irreg=0.4
pgood.irreg=1-pbad.irreg

sim.ICmodule = function(n=10){
  simulated.modules = c()
  lebels = sample(c(-1,1),1,
                  prob=c(pirreg,preg),
                  replace=T)
  
  if(any(labels==1)){
    simulated.modules[which(labels==1)] = 
      sample(c("goodreg","badreg"),n,
             sum(labels==1),
             prob=c(pgood.reg,pbad.reg),
             replace=T)
  }else{
    simulated.modules[which(labels==-1)] = 
      sample(c(-1,1),n,
             sum(labels==-1),
             prob=c(pbad.irreg,pgood.irreg),
             replace=T)
  }
  simulated.modules
}

ICmodule.sim()
S=10000