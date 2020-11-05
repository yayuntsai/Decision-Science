fish.Q = 3500
price.G = 3.25
fixed.operate = 10000
S=10000

#daily earning without uncertaincy
dailyearning.w = fish.Q * price.G - fixed.operate

#daily earning with uncertaincy
fish.R.sim = sample(c(0, 1000, 2000, 3000, 4000, 5000, 6000),
                    size = S,
                    prob = c(0.02, 0.03, 0.05, 0.08, 0.33, 0.29, 0.2),
                    replace=T)
price.R.sim = rnorm(S, 3.65, 0.2)

F=c()
for(i in 1:S){
  F[i] = min(fish.R.sim[i],fish.Q) * price.R.sim[i] - fixed.operate
}

summary(F)
hist(F,breaks=200)
sum(F>1375) / S
sum(F<0) / S

quantile(F,0.975)
quantile(F,0.025)


install.packages("EnvStats")
#Install the package above if needed
library(EnvStats)
# G market price is random
price.G  = rnorm(S, 3.5, 0.5)
frac = runif(S, 0.7,1)
fullload = 3500
fish.Q.sim = round(fullload*frac,0)
demand.G = rtri(S,2000, 6000, 5000) * frac

Frac.fish = c()
for(i in 1:S){
  Frac.fish[i] = price.G[i] * min(demand.G[i],fish.Q.sim[i])-fixed.operate
}
mean(Frac.fish)

