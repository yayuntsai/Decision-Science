#Q1
install.packages("EnvStats")
library(EnvStats)
S=10000
#Demand
D.Glou = round(rtri(S,4000,8000,7000),0)
D.Rock = round(rtri(S,4800,7200,6300),0)
fullboat = 3800
oper.cost = 7200

mu.PRR = 3.65
sigma.PRR = 0.25
#expected price at Glou
mu.PRG = 3.5
#the standard devaition of the price at Glou
sigma.PRG = 0.35
corrPRGxPRR = -0.8
corr.array = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)
length(corr.array)


#Simulated prices
sim.PRR=rnorm(S,mu.PRR,sigma.PRR)
sim.PRG=rnorm(S,mu.PRG,sigma.PRG)

FracRi = runif(S,0.6,0.9)
CapR = fullboat * FracRi
FracM = rtri(S,0.5, 1, 0.75)
CapM = fullboat * FracM


#strategies
#Rick goes to Glou and Morty goes to Rock
RtoG.MtoR=c() 
#Rick goes to Rock and Morty goes to Glou
RtoR.MtoG=c()
# Rick and Morty go to Glou
RMtoG=c()
#Rick and Morty go to Rock
RMtoR=c()


for(cor in 1:length(corr.array)){
  for(s in 1:S){
    RtoG.MtoR[s] = min(CapR[s],D.Glou[s]) * sim.PRG[s] + min(CapM[s],D.Rock[s]) * sim.PRR[s] - (oper.cost * 2)
    RtoR.MtoG[s] = min(CapR[s],D.Rock[s]) * sim.PRR[s] + min(CapM[s],D.Glou[s]) * sim.PRG[s] - (oper.cost * 2)
    RMtoG[s] = min(CapR[s] + CapM[s], D.Glou[s]) * sim.PRG[s]
    RMtoR[s] = min(CapR[s] + CapM[s], D.Rock[s]) * sim.PRR[s]
  }
}

hist(RtoG.MtoR, breaks=200)
hist(RtoR.MtoG, breaks=200)
hist(RMtoG, breaks=200)
hist(RMtoR, breaks=200)






#Q2
cost = 10
winPR = 25
losePR = 12.5
prob.win = 0.4
prob.lose = 1-prob.win

D.anti = rnorm(S,9000,2000)
D.win = rnorm(S,6000,2000)
lose.mean = 2000
lose.stdev = 1000
scale.lose = (lose.stdev)^2/lose.mean
D.lose = 

