oper.cost = 10000
G.PR = 3.25
R.PR = rnorm(S,3.65,0.2)
full.boat = 3500
R.demand = sample(c(0,1000,2000,3000,4000,5000,6000),S,
                       prob = c(0.02,0.03,0.05,0.08,0.33,0.29,0.2),
                       replace = TRUE)

#Without uncertainty
earning = c()
S=10000

#toG
revenue.G = 3.25 * full.boat - oper.cost
revenue.R =c()
for(s in 1:S){
  #to R
  revenue.R[s] = R.PR[s] * min(R.demand[s],full.boat) - oper.cost
}

mean(revenue.R)
hist(revenue.R, breaks=200)

sum(revenue.R > 1375) / S
sum(revenue.R < 0) / S

quantile(revenue.R,0.975)
quantile(revenue.R,0.025)

#CVarR: The "expected shortfall at q% level" is the expected return on the portfolio in the worst q% of cases
worst5q = revenue.R[which(revenue.R <= quantile(revenue.R,0.05))]
