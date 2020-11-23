#Q1
install.packages("EnvStats")
library(EnvStats)
library(MASS)

fullboat = 3800
oper.cost = 7200
S=100

#Demand
D.Glou = round(rtri(S,4000,8000,7000),0)
D.Rock = round(rtri(S,4800,7200,6300),0)

#Price
mu.PRR = 3.65
sigma.PRR = 0.25
mu.PRG = 3.5
sigma.PRG = 0.35

corr.PR = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)
length(corr.array)
#expected price at Glou
sim_PRG = matrix(nrow = S, ncol = 9)
sim_PRR = matrix(nrow = S, ncol = 9)
for(s in 1:S){
  for(corr in 1:9){
    P.corcovMatrix = matrix(c(sigma.PRR^2, sigma.PRR * sigma.PRG * corr.PR[corr], sigma.PRR * sigma.PRG * corr.PR[corr], sigma.PRG^2),nrow=2)
    sim.PR = mvrnorm(S, mu=c(mu.PRR,mu.PRG), P.corcovMatrix)
    sim_PRR[s,corr] = mean(sim.PR[,1])
    sim_PRG[s,corr] = mean(sim.PR[,2])
  }
}

#Simulated Fish
FracRi = runif(S,0.6,0.9)
CapR = round(fullboat * FracRi, 0)
FracM = rtri(S,0.5, 1, 0.75)
CapM = round(fullboat * FracM, 0)


#strategies
#Rick goes to Glou and Morty goes to Rock
RtoG.MtoR=c()
RtoG.MtoR=matrix(nrow=S, col=9)
#Rick goes to Rock and Morty goes to Glou
RtoR.MtoG=c()
RtoR.MtoG=matrix(nrow=S, col=9)
# Rick and Morty go to Glou
RMtoG=c()
RMtoG=matrix(nrow=S, col=9)
#Rick and Morty go to Rock
RMtoR=c()
RMtoR=matrix(nrow=S, col=9)


#expected profit
for(s in 1:S){
  for(corr in 1:9){
    RtoG.MtoR[s,corr] = min(CapR[s],D.Glou[s]) * sim.PRG[s,corr] + min(CapM[s],D.Rock[s]) * sim.PRR[s,corr] - (oper.cost * 2)
    RtoR.MtoG[s,corr] = min(CapR[s],D.Rock[s]) * sim.PRR[s,corr] + min(CapM[s],D.Glou[s]) * sim.PRG[s,corr] - (oper.cost * 2)
    RMtoG[s,corr] = min(CapR[s] + CapM[s], D.Glou[s]) * sim.PRG[s,corr]
    RMtoR[s,corr] = min(CapR[s] + CapM[s], D.Rock[s]) * sim.PRR[s,corr]
  }
}

#profit mean
for(pro in 1:9){
  RtoG.MtoR[pro] = mean(RtoG.MtoR[,pro])
  RtoR.MtoG[pro] = mean(RtoR.MtoG[,pro])
  RMtoG[pro] = mean(RMtoG[,pro])
  RMtoR[pro] = mean(RMtoR[,pro])
}

hist(RtoG.MtoR, breaks=200)
hist(RtoR.MtoG, breaks=200)
hist(RMtoG, breaks=200)
hist(RMtoR, breaks=200)

plot()





##Q2
#Cost parameters
unitcost = 10
unitprice = 25
winPR = 25
losePR = 12.5
prob.win = 0.4
prob.lose = 1-prob.win

D.anti = rnorm(S,9000,2000)

lose.mean = 2000
lose.stdev = 1000

scale.lose = (lose.stdev)^2/lose.mean
shape.lose = lose.mean/scale.lose
scale.lose
shape.lose

#Number of simulation runs
S=15000
game = sample(c(0,1),S,replace=T,
              prob=c(prob.lose,prob.win))
sim.demand=rep(0,S)

for(i in 1:S){
  if(sim.game[s]==1){
    sim.demand[s] = rnorm(1,6000,2000)
  }else{
    sim.demand[s] = rgamma(1,shape.lose,scale.lose)
  }
  sim.demand[s] = rounf(sim.demand[s],0)
  
}

profit = function(x=20000,d){
  #d: demand realizations
  profit.val=c()
  for(i in 1:length(d)){
    sim.profit[i] = (winPR-unitcost) * min(x,demand[i])
  }
  profit.val
}





