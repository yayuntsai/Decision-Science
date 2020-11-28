
#Q3

S = 10000
smallest=c()
for(s in 1:S){
  dice1 = sample(c(1,2,3,4,5,6),S,replace=T)
  dice2 = sample(c(1,2,3,4,5,6),S,replace=T)
  dice3 = sample(c(1,2,3,4,5,6),S,
                 prob=c(0.15,0.25,0.15,0.15,0.15,0.15),
                 replace=T)
  smallest[s] =  min(dice1[s],dice2[s],dice3[s])
}
prob.table = table(smallest)/S
exp_smallest = 0
for(i in 1:6){
  exp_smallest = exp_smallest + i * as.numeric(prob.table)[i]
}

exp_smallest




#Q4






#Q5

n=1000
total.days=c()
for(s in 1:n){
  borrow.days = sample(c(4,5,6,7),n,
                       prob=c(0.1,0.2,0.3,0.4),
                       replace=T)
  delay = sample(c(1,0),n,
                 replace=T)
  total.days[s] = borrow.days[s] + 2 * delay[s]
}
prob.wed = sum(total.days=6)/n