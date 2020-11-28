taskt.mean = c(20,50,60,15,65,35,30,10)
taskt.sigma = c(7,10,12,3,30,15,5,3)

shape = c()
scale = c()
scale = taskt.mean^2 / taskt.sigma
shape = taskt.mean / scale

S=10000
penaltyperday=100000
#BT = begin time
#ET = end time
simDays.temp=c()
simPenalty.temp=c()