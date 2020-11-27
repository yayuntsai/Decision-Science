oper.cost = 10000
G.PR = 3.25
R.PR = rnorm(S,3.65,0.2)
full.boat = 3500
R.demand = sample(c(0,1000,2000,3000,4000,5000,6000),S,
                       prob = c(0.02,0.03,0.05,0.08,0.33,0.29,0.2),
                       replace = TRUE)

#Without uncertainty
S=10000

#toG
revenue.G = 3.25 * full.boat - oper.cost
F =c()
for(s in 1:S){
  #to R
  F[s] = R.PR[s] * min(R.demand[s],full.boat) - oper.cost
}

mean(F)
hist(F, breaks=200)

sum(F > 1375) / S
sum(F < 0) / S

quantile(F,0.975)
quantile(F,0.025)

#CVarR: The "expected shortfall at q% level" is the expected return on the portfolio in the worst q% of cases
worst5q = F[which(F <= quantile(F,0.05))]
mean(worst5q)




#With uncertaincy
library(EnvStats)
G.PR = rnorm(S,3.5,0.5)
R.PR = rnorm(S,3.65,0.2)
G.demand = rtri(S,2000,6000,5000)
R.demand = sample(c(0,1000,2000,3000,4000,5000,6000),S,
                  prob = c(0.02,0.03,0.05,0.08,0.33,0.29,0.2),
                  replace = TRUE)

full.boat = 3500
oper.cost = 10000
frac = runif(S,0.7,1)
fish.Q = round(full.boat*frac,0)
G = c()
R = c()

for(s in 1:S){
  G[s] = G.PR[s] * min(fish.Q[s], G.demand[s]) - oper.cost
  R[s] = R.PR[s] * min(fish.Q[s], R.demand[s]) - oper.cost
}

summary(G)
summary(R)

sum(G>1375)/S
sum(G<0)/S

sum(F>1375)/S
sum(F<0)/S

quantile(G,0.975)
quantile(G,0.025)

quantile(F,0.975)
quantile(F,0.025)

lowestq5G=G[which(G<=quantile(G,0.05))]
CVaRq5G=mean(lowestq5G)
CVaRq5G

lowestq5F=F[which(F<=quantile(F,0.05))]
CVaRq5F=mean(lowestq5F)
CVaRq5F

