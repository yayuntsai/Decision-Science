S=10000
calls=c()
for(s in 1:S){
  k = 0
  totaltime = 0
  while(totaltime<=60){
    totaltime = totaltime + rexp(1,1/10)
    k = k+1
  }
  calls[s]=k-1
  if(s%%1000==0)
    print(s)
}

plot(table(calls)/S)
