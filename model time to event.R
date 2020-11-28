taskt.mean = c(20,50,60,15,65,35,30,10)
taskt.sigma = c(7,10,12,3,30,15,5,3)

shape.est = c()
scale.est = c()
scale.est = taskt.mean^2 / taskt.sigma
shape.est = taskt.mean / scale.est

S = 10000
penaltyperday = 100000
B.reduced = 0

#BT = begin time
#ET = end time
simDays.temp=c()
simPenalty.temp=c()

for(i in 1:S){
  task.time = c()
  for(j in 1:length(taskt.mean)){
    shape.j = shape.est[j]
    scale.j = scale.est[j]
    taskt.i.j = rgamma(1, shape.j, scale.j)
    taskt.i[j] = round(taskt.i.j,0)
  }
  BT.A = BT.C = BT.E = 0
  ET.A = BT.A + taskt.i[1]
  ET.C = BT.C + taskt.i[3]
  ET.E = BT.E + taskt.i[5]
}