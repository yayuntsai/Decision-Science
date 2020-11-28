
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
exp_smallest = sum(smallest)/S
exp_smallest
