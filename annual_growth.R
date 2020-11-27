##Estimating correlations/covariances from data
Fund1=c(65, 79, 85, 78, 107, 108, 124, 156, 195, 181, 216)
Fund2=c(47, 61, 73, 60, 89, 86, 104, 120, 140, 134, 175)
Fund3=c(38, 37, 39, 40, 47, 46, 57, 71, 74, 72, 87)
Fund4=c(61, 64, 74, 72, 95, 89, 114, 147, 146, 127, 152)

AnnualGR=matrix(0,nrow=(length(Fund1)-1),ncol=4)

AnnualGR

for(i in 2:length(Fund1)){
  AnnualGR[i-1,1]=Fund1[i]/Fund1[i-1]
  AnnualGR[i-1,2]=Fund2[i]/Fund2[i-1]
  AnnualGR[i-1,3]=Fund3[i]/Fund3[i-1]
  AnnualGR[i-1,4]=Fund4[i]/Fund4[i-1]
}

AnnualGR

Sigma.est=cov(AnnualGR)
Sigma.est
cov2cor(Sigma.est)

mu.est=c(mean(AnnualGR[,1]),mean(AnnualGR[,2]),
         mean(AnnualGR[,3]),mean(AnnualGR[,4]))
mu.est

library(MASS)
MVN.AGR=mvrnorm(50,mu.est,Sigma.est)

Fund1.MVN=MVN.AGR[,1]
Fund2.MVN=MVN.AGR[,2]

#Ignore dependencies
Fund1.N=rnorm(50,mean(AnnualGR[,1]),sd(AnnualGR[,1]))
Fund2.N=rnorm(50,mean(AnnualGR[,2]),sd(AnnualGR[,2]))


x11(width=18,height=5)
par(mfrow=c(1,3))
plot(AnnualGR[,1],AnnualGR[,2],type='p',pch=1,lwd=5)
plot(Fund1.MVN,Fund2.MVN,type='p',pch=2,lwd=4,col='red')
plot(Fund1.N,Fund2.N,type='p',pch=3,lwd=4,col='green')
