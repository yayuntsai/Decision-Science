pnorm(120,150,30)
curve(pnorm(120,150,30))
1-pnorm(25,75,25)


mean=150; sd=30

x <- seq(-4,4,length=100)*sd + mean
hx <- pnorm(x,mean,sd)

plot(x, hx, type="n", xlab="IQ Values", ylab="",
     main="Normal Distribution", axes=FALSE)


mu1 = 150
sig1 = 30

mu2 = 75
sig2 = 25

S=1000
set.seed(5566)
time1 = rnorm(S, mu1,sig1)
time2 = rnorm(S, mu2,sig2)
toltime = time1+time2

sum(toltime>180)/S
quantile(toltime,0.95)





S=1000

salary = function(){
  cf_salary = 80000/12
  num_of_meal = rnorm(S,3000,1000)
  rev_per_meal = sample(x=c(20-11, 18.5-11, 16.5-11, 15-11),
                        size = 1,
                        prob=c(0.25, 0.35, 0.3, 0.1),
                        replace=T)
  
}
