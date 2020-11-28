savings = 1000000
inflation=0.03
life.mean = 20
life.stdev = 10

#Return
growth.mean = 0.08
growth.std = 0.02
scale.est = life.stdev^2 / life.mean
shape.est = life.mean / scale.est
life.long = rgamma(S, shape.est, scale.est)
withdraw = seq(50000,80000,500)
sim.growth=list()

S=5000
prob.broke = c()
for(i in 1:S){
  sim.growth[[s]] = rnorm(S,growth.mean,growth.std)
}

for(i in 1:length(withdraw)){
  broke.i = 0 #計算破產次數
  for(s in 1:S){
    years.togo = round(life.long[s],0)
    spending = withdraw[i]
    savings.left = savings
    while(1 <= years.togo & savings.left > 0){
      savings.left = savings.left - withdraw[j]
      spending = spending + inflation
    }
    if(savings.left < 0){
      broke.i = broke.i+1
    }
    if(s%%1000==0){cat("Withdraw=",withdraw[i],
                       "S=",s,"\n")}
    
  }
  prob.broke[i]=broke.i/S
}

prob.broke
which(prob.broke<0.05)
withdraw[which(prob.broke<0.05)]

plot(withdraw,prob.broke,type='l',lwd=3)
points(withdraw,prob.broke,pch=1)
abline(h=0.05,col='green',lty=2,lwd=2)
