price = 150
qs = 5000
cs = 120

qp = 10000
cp = 100
fixP = 50000

S = 10000
D.weak = sample(c(2000,8000,by=1),S, replace=T)
D.strong = sample(c(6000,14000,by=1),S, replace=T)

S.profit.weak = S.profit.strong = c()
P.profit.weak = P.profit.strong = c()

for (s in 1:S){
  S.profit.weak[s] = -cs * D.weak[s] + price * min(D.weak[s], qs)
  S.profit.strong[s] = -cs * D.strong[s] + price * min(D.strong[s], qs)
  P.profit.weak[s] = -cp * D.weak[s] + price * min(D.weak[s], qs) - fixP
  P.profit.strong[s] = -cp * D.strong[s] + price * min(D.strong[s], qs) -fixP
}