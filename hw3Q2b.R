#The number of samples from the mixture distribution
N <- 10000                 

#Sample N random uniforms U
U <- runif(N)

#Variable to store the samples from the mixture distribution                                             
rand.samples1 <- rep(NA,N)
rand.samples2 <- rep(NA,N)

#Weights
theta1 <- 0.5
theta2 <- 2
C1 <- 1/(2*gamma(theta1)+gamma(theta1+0.5))
c11 <- 2*C1*gamma(theta1)
c12 <- C1*gamma(theta1+0.5)
C2 <- 1/(2*gamma(theta2)+gamma(theta2+0.5))
c21 <- 2*C2*gamma(theta2)
c22 <- C2*gamma(theta2+0.5)

#Sampling from the mixture
for(i in 1:N){
  if(U[i]<c11){
    rand.samples1[i] <- rgamma(1,theta1,rate = 1)
  }else{
    rand.samples1[i] <- rgamma(1,theta1+0.5,rate = 1)
  }
}

for(i in 1:N){
  if(U[i]<c21){
    rand.samples2[i] <- rgamma(1,theta2,rate = 1)
  }else{
    rand.samples2[i] <- rgamma(1,theta2+0.5,rate = 1)
  }
}
#Density plot of the random samples
plot(density(rand.samples1),main="Density Estimate of the Mixture Model")
hist(rand.samples1, prob = TRUE)

#Plotting the true density as a sanity check
x <- seq(0,10,.1)
truth1 <- c11*dgamma(x,theta1,1) + c12*dgamma(x,theta1+0.5,1)
plot(density(rand.samples1),main="Density Estimate of the Mixture Model",ylim=c(0,2),lwd=2)
#hist(rand.samples, freq=FALSE, main="Estimation Density vs True Density of the Mixture Model")
lines(x,truth1,col="red",lwd=2)
legend("topright",c("True Density","Estimated Density"),col=c("red","black"),lwd=2)


#Plotting the true density as a sanity check
truth2 <- c21*dgamma(x,theta2,1) + c22*dgamma(x,theta2+0.5,1)
plot(density(rand.samples2),main="Density Estimate of the Mixture Model",ylim=c(0,2),lwd=2)
#hist(rand.samples, freq=FALSE, main="Estimation Density vs True Density of the Mixture Model")
lines(x,truth2,col="red",lwd=2)
legend("topright",c("True Density","Estimated Density"),col=c("red","black"),lwd=2)