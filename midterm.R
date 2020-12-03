#three best people cost
best.people = 20000

#annual salary
annual.salary = 48000
month.salary = annual.salary/12

##Analyst Position
direct.salary.cost = 4000
indirect.support.cost = 2000
#fixed fee for one analyst's time
fixed.fee = 10000
contribution.productive = 4000
contribution.transferee = 400

##Work Flow Mangement Problems
less.efficient = 0.6
#immediate cost of recruiting too much
monthly.total.cost = month.salary + indirect.support.cost
monthly.total.cost.trans = 9600


##Alternative hiring strategies
#Fixed-start strategy
#Flexible-start strategy
#(1) Start to work on July
start7 = 0.5
#(2) Start to work on September
#start9 = 0.5 * (0.7~1)
#(3) Start December


## Data Gathering
analysts = 63
acceptance.rate = 0.7
sim.accepted = rbinom(S,size=analysts,prob=acceptance.rate)
#analysts.accepted = analysts * acceptance.rate
#analysts.attrition (0%~20%)



## demand levels
mu.demand = 90
std.demand = 0.5

#analysts retention
analysts.retention79 = runif(S,0.8,1)
analysts.retention5678 = runif(S,0.95,1)
analysts.retention = runif(S,0.9,1)

historic.demand=c(75,70,70,110,105,90,65,80,90,120,105,95)
x = rnorm(S, 0, 0.05)
y = rnorm(S, 0, 0.1)

### Model of Demand for Analysis
#Demand for analysis in month
demand.inmonth = c()


### Model of Supply for Analysis
retention.rate = c(0.9,0.95,0.95,0.95,0.975,0.975,0.975,0.975,0.9,0.95,0.95,0.95)



#(a)
for(s in 1:12){
  demand.inmonth[s] = historic.demand[s] * (1+x[s]) * (1+y[s])
}





