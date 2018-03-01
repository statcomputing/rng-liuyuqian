#The number of samples from the mixture distribution
N <- 10000                 

#Variable to store the samples from the mixture distribution                                             
rand.samplesF <- rep(NA,N)

#check for the normalizing constant 
integrandF <- function(x) {sqrt(4+x)*x^(-0.5)*exp(-x)}
C_f <- integrate(integrandF, lower = 0, upper = Inf)
C_f <- C_f$value
C_f <- 1/C_f

#envelope setting (10,20)
alpha <- 5
theta <- 0.5
C <- 1/(2*gamma(theta)+gamma(theta+0.5))
c1 <- 2*C*gamma(theta)
c2 <- C*gamma(theta+0.5)
g_x <- function(x) {c1*dgamma(x,theta,1) + c2*dgamma(x,theta+0.5,1)}

#plot f
x_value <- seq(0,5,.05)
#F <- function(x) {sqrt(4+x)*x^(0.5-1)*exp(-x)}
truthF <- C_f*integrandF(x_value)
plot(x_value,truthF,col="red",lwd=2)
truthG <- c1*dgamma(x_value,theta,1) + c2*dgamma(x_value,theta+0.5,1)
Envelop <- alpha*truthG
lines(x_value,Envelop,col="blue",lwd=2)

#Rejection Sampling from the envelop
for (i in 1:N) {
  while (TRUE) {
    U <- runif(1)
    if(U < c1){
      cand <- rgamma(1,theta,rate = 1)
    }else{
      cand <- rgamma(1,theta+0.5,rate = 1)
    }
    ratio <- F(cand)/(alpha*g_x(cand))
    if (U < ratio) break
  }
  rand.samplesF[i] <- cand
}

#Plotting the true density as a sanity check
hist(rand.samplesF, freq=FALSE, main="Estimation Density vs True Density of the Mixture Model")
lines(x_value,truthF,col="red",lwd=2)
legend("topright",c("True Density","Estimated Density"),col=c("red","black"),lwd=2)

#Plotting the true density as a sanity check
plot(density(rand.samplesF),main="Density Estimate of the Mixture Model",ylim=c(0,3),lwd=2)
#hist(rand.samplesF, freq=FALSE, main="Estimation Density vs True Density of the Mixture Model")
lines(x_value,truthF,col="red",lwd=2)
legend("topright",c("True Density","Estimated Density"),col=c("red","black"),lwd=2)

