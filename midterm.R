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

##Work Flow Management Problems
less.efficient = 0.6
#immediate cost of recruiting too much
monthly.total.cost = month.salary + indirect.support.cost
monthly.total.cost.trans = 9600


##Alternative hiring strategies
#Fixed-start strategy
#Flexible-start strategy
#(1) Start to work on July
start7.frac = 0.5
#(2) Start to work on September
#start9.frac = 0.5 * (0.7~1)
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
analysts.retention19 = analysts.retention5678 = analysts.retention =c()
analysts.retention19 = runif(1,0.8,1)
analysts.retention5678 = runif(1,0.95,1)
analysts.retention = runif(1,0.9,1)

historic.demand=c(75,70,70,110,105,90,65,80,90,120,105,95)
### Model of Supply for Analysis(Start from July)
retention.rate = c(0.95,0.975,0.975,0.975,0.975,0.9,0.95,0.95,0.95,0.9,0.95,0.95)

### Model of Demand for Analysis
#Demand for analysis in month
demand.inmonth = c()
supply.inmonth = c()
profit=c()
demand.inmonth.test=c()
demand.inmonth.test1=c()
demand.inmonth1 = c()
x=y=A=c()



#(a)
sim.revenue.x=function(n){
  S=1000
  #profit=rep(0,S)
  for(n in 11:110){
    num.offer=n
    for(i in 1:S){
      #s 1~12是四月到三月
      
      for(s in 1:12){
        #demand
        x = rnorm(S, 0, 0.05)
        y = rnorm(S, 0, 0.1)
        A = rbinom(1,num.offer,0.7)
        demand.inmonth[s] = round(historic.demand[s] * (1+x) * (1+y),0)
        
        #supply
        if(s==1){
          supply.inmonth[1]= round(63 *analysts.retention,0)
        }else if(s==5){
          supply.inmonth[s] = round(supply.inmonth[s-1] * analysts.retention5678,0)+ A
        }else if(s==6||s==10){
          supply.inmonth[s] = round(supply.inmonth[s-1] * analysts.retention19,0)
        }else if(s==2 || s==3 || s==4){
          supply.inmonth[s] = round(supply.inmonth[s-1] * analysts.retention5678,0) 
        }else if(s==7 || s==8 || s==9 || s==11 || s==12){
          supply.inmonth[s] = round(supply.inmonth[s-1] * analysts.retention,0)
        }
        demand.inmonth
        supply.inmonth
        if(demand.inmonth[s]==supply.inmonth[s]){
          profit[s] = 4000 * demand.inmonth[s]
        }else if(demand.inmonth[s]<supply.inmonth[s]){
          profit[s] = 10000 * demand.inmonth[s] - 6000 * supply.inmonth[s]
        }else{
          profit[s] = 4000 * supply.inmonth[s] - 400 * (demand.inmonth[s]-supply.inmonth[s])
        }
        mean(profit[1:12])
      }
    }
    return(c(mean(profit)))
  }
}





