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

