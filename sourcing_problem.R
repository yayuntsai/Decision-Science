price = 150
qs = 5000
cs = 120

qp = 10000
cp = 100
fixP = 50000

set.seed(9527)
S = 10000
D.weak = sample(seq(2000,8000,by=1),S, replace=T)
D.strong = sample(seq(6000,14000,by=1),S, replace=T)

S.profit.weak = S.profit.strong = c()
P.profit.weak = P.profit.strong = c()

for (s in 1:S){
  S.profit.weak[s] = -cs * qs + price * min(D.weak[s], qs)
  S.profit.strong[s] = -cs * qs + price * min(D.strong[s], qs)
  P.profit.weak[s] = -cp * qp + price * min(D.weak[s], qp) - fixP
  P.profit.strong[s] = -cp * qp + price * min(D.strong[s], qp) -fixP
}
summary(S.profit.weak)
summary(S.profit.strong)
summary(P.profit.weak)
summary(P.profit.strong)

##Using E[Demand] for decision-making
-cS*QS+price*min(QS,5000)
-cS*QS+price*min(QS,10000)

-cP*QP-fixcP+price*min(QP,5000)
-cP*QP-fixcP+price*min(QP,10000)

