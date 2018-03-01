#The number of samples from the mixture distribution
N <- 10000                 

#Variable to store the samples from the mixture distribution                                             
rand.samplesF <- rep(NA,N)

#Setup parameters
theta <- 7
beta <- 5
#theta <- 9
#beta <- 5

#check for the normalizing constant 
integrandq <- function(x) {((x^(theta-1))/(1+x^2))+sqrt(2+x^2)*(1-x)^(beta-1)}
C_f <- integrate(integrandq, lower = 0, upper = 1)
C_f <- C_f$value
C_f <- 1/C_f

#envelope setting (10,20)
alpha1 <- 1/theta
alpha2 <- sqrt(3)/beta
g_x <- function(x) {dbeta(x,theta,1) + dbeta(x,1,beta)}
Envelop <- function(x) {alpha1*dbeta(x,theta,1) + alpha2*dbeta(x,1,beta)}

#plot f
x_value3 <- seq(0,1,.01)
truthF <- C_f*integrandq(x_value3)
plot(x_value3,truthF,col="red",lwd=2)
truthG <- g_x(x_value3)
EnvelopG <- Envelop(x_value3)
lines(x_value3,EnvelopG,col="blue",lwd=2)

#Rejection Sampling from the envelop
for (i in 1:N) {
  while (TRUE) {
    U <- runif(1)
    if(U < alpha1/(alpha1+alpha2)){
      cand <- rbeta(1,theta,1)
    }else{
      cand <- rbeta(1,1,beta)
    }
    ratio <- F(cand)/(Envelop(cand))
    rejection <- runif(1)
    if (rejection < ratio) break
  }
  rand.samplesF[i] <- cand
}

#Plotting the true density as a sanity check
hist(rand.samplesF, freq=FALSE, main="Estimation Density vs True Density of the Mixture Model")
lines(x_value3,truthF,col="red",lwd=2)
legend("topright",c("True Density","Estimated Density"),col=c("red","black"),lwd=2)

#Plotting the true density as a sanity check
plot(density(rand.samplesF),main="Density Estimate of the Mixture Model",ylim=c(0,5),lwd=2)
#hist(rand.samplesF, freq=FALSE, main="Estimation Density vs True Density of the Mixture Model")
lines(x_value3,truthF,col="red",lwd=2)
legend("topright",c("True Density","Estimated Density"),col=c("red","black"),lwd=2)