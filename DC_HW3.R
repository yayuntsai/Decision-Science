#Q1
#install.packages("EnvStats")
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
#expected price at Glou
sim.PRG = matrix(nrow = S, ncol = 9)
sim.PRR = matrix(nrow = S, ncol = 9)
for(s in 1:S){
  for(corr in 1:9){
    P.corcovMatrix = matrix(c(sigma.PRR^2, sigma.PRR * sigma.PRG * corr.PR[corr], sigma.PRR * sigma.PRG * corr.PR[corr], sigma.PRG^2),nrow=2)
    sim.PR = mvrnorm(S, mu=c(mu.PRR,mu.PRG), P.corcovMatrix)
    sim.PRR[s,corr] = mean(sim.PR[,1])
    sim.PRG[s,corr] = mean(sim.PR[,2])
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
sim_profit.RtoG.MtoR=matrix(nrow=S, ncol=9)
#Rick goes to Rock and Morty goes to Glou
RtoR.MtoG=c()
sim_profit.RtoR.MtoG=matrix(nrow=S, ncol=9)
# Rick and Morty go to Glou
RMtoG=c()
sim_profit.RMtoG=matrix(nrow=S, ncol=9)
#Rick and Morty go to Rock
RMtoR=c()
sim_profit.RMtoR=matrix(nrow=S, ncol=9)


#expected profit
for(s in 1:S){
  for(corr in 1:9){
    sim_profit.RtoG.MtoR[s,corr] = min(CapR[s],D.Glou[s]) * sim.PRG[s,corr] + min(CapM[s],D.Rock[s]) * sim.PRR[s,corr] - (oper.cost * 2)
    sim_profit.RtoR.MtoG[s,corr] = min(CapR[s],D.Rock[s]) * sim.PRR[s,corr] + min(CapM[s],D.Glou[s]) * sim.PRG[s,corr] - (oper.cost * 2)
    sim_profit.RMtoG[s,corr] = min(CapR[s] + CapM[s], D.Glou[s]) * sim.PRG[s,corr] - (oper.cost * 2)
    sim_profit.RMtoR[s,corr] = min(CapR[s] + CapM[s], D.Rock[s]) * sim.PRR[s,corr] - (oper.cost * 2)
  }
}

#profit mean
for(pro in 1:9){
  RtoG.MtoR[pro] = mean(sim_profit.RtoG.MtoR[,pro])
  RtoR.MtoG[pro] = mean(sim_profit.RtoR.MtoG[,pro])
  RMtoG[pro] = mean(sim_profit.RMtoG[,pro])
  RMtoR[pro] = mean(sim_profit.RMtoR[,pro])
}


plot_exp_profit = function(){
  profit.max = max(RtoG.MtoR, RtoR.MtoG, RMtoG, RMtoR)
  profit.min = min(RtoG.MtoR, RtoR.MtoG, RMtoG, RMtoR)
  plot(corr.PR, RtoG.MtoR, type="b", col = "green", lwd = 2, main="Expected Profit", xlab = "corr(P.Glou, P.Rock)", ylab="Profit", xaxt="n", ylim = c(profit.min - 1000, profit.max + 2000))
  axis(1, at = seq(-0.8, 0.8, by = 0.2), las=2)
  lines(corr.PR, RtoR.MtoG, type = "b", col = "blue")
  lines(corr.PR, RMtoG, type = "b", col = "red")
  lines(corr.PR, RMtoR, type = "b", col = "orange")
  legend("left", c("a", "b", "c", "d"), fill = c("green", "blue", "red" ,"orange"), horiz=TRUE, cex = 0.8, box.lwd = 0, inset = 0.005)
}









##Q2
#Cost parameters
unitcost = 10
unitprice = 25
winPR = 25
losePR = 12.5
salvage = 0
prob.win = 0.4
prob.lose = 1-prob.win

lose.mean = 2000
lose.stdev = 1000

scale.lose = (lose.stdev)^2/lose.mean
shape.lose = lose.mean/scale.lose
scale.lose
shape.lose

#Number of simulation runs
S=10000
x.val=seq(5000,13000,1000)
#prob of game win/lose
sim.game = sample(c(0,1),S,replace=T,
              prob=c(prob.lose,prob.win))

sim.anti = round(rnorm(S,9000,2000), 0)
after.demand = c()

for(s in 1:S){
  if(sim.game==1){
    after.demand[s] = round(rnorm(1,6000,2000), 0)
  }else{
    after.demand[s] = round(rgamma(1,shape.lose,scale.lose), 0)
  }
}

profit = function(x=20000, before, after){
  profit.val = c()
  #d: demand realizations
  for(i in 1:S){
    profit.val[i] = (unitprice-unitcost) * min(x,sim.anti[i])
    #確認比賽後還有剩
    if(x-sim.anti[i] > 0){
      if(sim.game==1){
        #win
        profit.val[i] = profit.val[i] + (winPR-unitcost) * min(x-sim.anti[i], after[i]) - unitcost * max(x-sim.anti[i]-after[i],0)
      }else{
        #lose
        profit.val[i] = profit.val[i] + (losePR-unitcost) * min(x-sim.anti[i], after[i]) - unitcost * max(x-sim.anti[i]-after[i],0)
      }
    }
  }
  profit.val
}


sim.profit = matrix(0, nrow=S, ncol=length(x.val))
avg.profit = c()
sd.profit = c()
CVaRq10 = c()

for(i in 1:length(x.val)){
  sim.profit[,i] = profit(x.val[i],sim.anti,after.demand)
  avg.profit[i] = mean(sim.profit[,i])
  sd.profit[i] = sd(sim.profit[,i])
  lowestq10=sim.profit[which(sim.profit[,i]<=quantile(sim.profit[,i],0.1)), i]
  CVaRq10[i] = mean(lowestq10[i])
  cat("production quantity:", x.val[i], "\n")
}
summary(CVaRq10)

options(scipen=20)
#x11(width=12,height=5)
par(mfrow=c(1,3))
plot(x.val,avg.profit,type='l',xlab="production quantity",lwd=3)
plot(x.val[which(avg.profit>120000)],avg.profit[which(avg.profit>120000)],type='l',xlab="production quantity",lwd=3)
plot(x.val,CVaRq10,type='l',xlab="production quantity",lwd=3)


#based on profit
optimal.quantity1 = x.val[which.max(avg.profit)]
#based on CVAR
optimal.quantity2 = x.val[which.max(CVaRq10)]




cu.win = winPR-unitcost
co.win = unitcost-salvage
cu.lose = losePR-unitcost
co.lose = unitcost-salvage
cu
co

dwin = rnorm(1, 6000, 2000)
dlose = rgamma(1,shape.lose,scale.lose)
#Calculate the critical fractile
frac.win = cu.win/(cu.win+co.win)
frac.lose = cu.lose/(cu.lose+co.lose)
perfectq.win = perfectq.lose = profit.win = profit.lose=c()
perfectq.win = quantile(dwin,frac.win,names=FALSE) 
perfectq.win = round(perfectq.win,0)
perfectq.lose = quantile(dlose,frac.lose,names=FALSE)
perfectq.lose = round(perfectq.lose,0)
perfectq.before = quantile(sim.anti, frac.win, names=FALSE)
perfectq.before = round(x.before, 0)


profit.perfectinfo=c()

for(s in 1:S){
  if(sim.result[s] == 1){
    quantity = perfectq.before + x.win
    profit.perfectinfo[s] = (unitprice-unitcost)*min(quantity, sim.anti[s])
    if(quantity - sim.anti[s] > 0){
      profit.perfectinfo[s] = 
        profit.perfectinfo[s]+(unitprice-unitcost)*min(quantity-sim.anti[s],dwin[s])
      -unitcost*max(quantity-sim.anti[s]-dwin[s],0)
    }
  }else{
    quantity = x.before + x.lose
    profit.perfectinfo[s] = (unitprice-unitcost)*min(quantity, sim.anti[s])
    if(quantity - sim.anti[s] > 0){
      profit.perfectinfo[s] = profit.perfectinfo[s]+(losePR-unitcost)*min(quantity-sim.anti[s],dlose[s])-unitcost*max(quantity-sim.anti[s]-dlose[s],0)
    }
  }
}

mean(profit.perfectinfo)
mean(profit.perfectinfo)-max(avg.profit)




