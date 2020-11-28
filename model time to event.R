taskt.mean = c(20,50,60,15,65,35,30,10)
taskt.sigma = c(7,10,12,3,30,15,5,3)

shape.est = c()
scale.est = c()
scale.est = taskt.sigma^2 / taskt.mean
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
  
  BT.B=ET.A
  if(B.reduced==0){
    ET.B=BT.B+taskt.i[2]
  }
  ##
  if(B.reduced==1){
    ET.B=BT.B+round(taskt.i[2]*0.8,0)
  }
  #
  BT.D=max(ET.B,ET.C)
  ET.D=BT.D+taskt.i[4]
  #
  BT.F=ET.E
  ET.F=BT.F+taskt.i[6]
  #
  BT.G=ET.D
  ET.G=ET.D+taskt.i[7]
  #
  BT.H=ET.G
  ET.H=BT.H+taskt.i[8]
  #
  simDays.temp[i]=max(ET.H, ET.F)
  delay.i=max(simDays.temp[i]-130,0)
  simPenalty.temp[i]=delay.i*penaltyperday
}

#No crashing activitiy B
simDays.base=simDays.temp
simPenalty.base=simPenalty.temp
#
summary(simDays.base)
sd(simDays.base)
sum(simDays.base<=130)/S
#
summary(simPenalty.base)

#Do crash activitity B
simDays.reduced=simDays.temp
simPenalty.reduced=simPenalty.temp
summary(simDays.reduced)
sd(simDays.reduced)
sum(simDays.reduced<=130)/S
#
summary(simPenalty.reduced)