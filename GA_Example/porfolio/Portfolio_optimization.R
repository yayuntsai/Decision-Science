#install.packages("GA")
library(GA)
asset_returns = read.csv("asset_returns.csv")
asset_returns

portfolio_returns = function(x) {
  port.returns = 0
  
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + asset_returns[,i] * x[i]
  }
  
  return (port.returns)
}

sharpe = function(x) {
  port.returns = portfolio_returns(x)
  
  return (mean(port.returns)/sqrt(var(port.returns)))
  
}

constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr + 
      max(c(0,x[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-x[i]))**2     # "x >= 0" constraint
  }
  
  return (boundary_constr)
}

obj = function(x, lambda) {
  # We want the maximum Sharpe ratio
  
  return (sharpe(x)-lambda*constraint(x))
}
lambda = 150
ga_res = ga(type="real-valued", 
            function(x){obj(x, lambda = lambda)}, #ga is to maximize fitness function
            names = c("AAPL","AMZN","FB", "WWE", "NASDAQ", "S&P"),
            lower = rep(0,ncol(asset_returns)), # x_i >= 0
            upper = rep(1,ncol(asset_returns)), # x_i <= 1
            maxiter = 50000,
            run=50, 
            parallel=TRUE,# Exploit multi-core properties of your CPU
            monitor=TRUE,
            seed=1)

ga_res@solution # best solution
ga_res@fitnessValue # best fitness value

