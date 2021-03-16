##function a simulation, with p=probability of obtaining the outcome
set.seed(1002) ## use the set.seed commend to ensure the outcome keeps unchange
simulation <- function(p) {
  n0=100 ## the sample size we want for group 0
  n1=100 ## the sample size we want for group 1
  x0 <- rbinom(10000,n0,p) ## stimulate to get random draws
  x1 <- rbinom(10000,n1,p)
  mxresults <- rep(NA,10000) ## make an list to contain results from the for loop
  for (i in 1:10000) {
    mx <- matrix(c(n0-x0[i],n1-x1[i], x0[i],x1[i]),ncol = 2, nrow = 2) ## make a contingency table in matrix form
    MXresult <- fisher.test(mx) ## run the fisher's exact test to the 
    mxresults[i] <- MXresult$p.value ## storing p-value in the list we created
  }
  pvals <- mean(mxresults < 0.05) ## want type one error rate
  return(pvals)
}
simulation(0.5)
## Try n=100 and graph the null power function
zset <- seq(0,1, 0.01)
simu100 <- lapply(zset, simulation) ## apply the simulation function to each object in the zset
fisher100 <- as.numeric(simu10)
smoothingSpline = smooth.spline(zset, fisher100, spar=0.5) ## adding smooth line, spar between 0 and 1
plot(zset,fisher100)
lines(smoothingSpline)

### Modify the function so n is not restricted to 100
simulation1 <- function(n,p) {
  x0 <- rbinom(10000,n,p) ## stimulate to get random draws
  x1 <- rbinom(10000,n,p)
  mxresults <- rep(NA,10000) ## make an list to contain results from the for loop
  for (i in 1:10000) {
    mx <- matrix(c(n-x0[i],n-x1[i], x0[i],x1[i]),ncol = 2, nrow = 2) ## make a contingency table in matrix form
    MXresult <- fisher.test(mx) ## run the fisher's exact test to the 
    mxresults[i] <- MXresult$p.value ## storing p-value in the list we created
  }
  pvals <- mean(mxresults < 0.05) ## want type one error rate
  return(pvals)
}
## for n=10
zset <- seq(0,1,0.01)
fisher.test.10 <- function(z)
{ myz10 <- simulation1(z,n=10)
return(myz10)
}
simu1 <- lapply(zset, fisher.test.10)
fisher1 <- as.numeric(simu1)
plot(zset,fisher1)
###function for n=25
fisher.test.25 <- function(z)
{ myz25 <- simulation1(z,n=25)
return(myz25)
}
simu25 <- lapply(zset, fisher.test.25)
fisher25 <- as.numeric(simu25)
###function for n=50
fisher.test.50 <- function(z)
{ myz50 <- simulation1(z,n=50)
return(myz50)
}
simu50 <- lapply(zset, fisher.test.50)
fisher50 <- as.numeric(simu50)
### library packages for ggplot
library(ggplot2)
library(dplyr)
###make a data frame for the results of the function for different n values
fishersimu <- data.frame(zset, fisher1, fisher25,fisher50, fisher100)

##
p = ggplot() + 
  geom_smooth(data = fishersimu, aes(x = zset, y = fisher1), color = "blue", span=0.3, se=FALSE) +
  geom_smooth(data = fishersimu, aes(x = zset, y = fisher25), color = "red", span=0.3, se=FALSE) +
  geom_smooth(data = fishersimu, aes(x = zset, y = fisher50), color = "green", span=0.3, se=FALSE) +
  geom_smooth(data = fishersimu, aes(x = zset, y = fisher100), color = "yellow", span=0.3, se=FALSE) +
  geom_hline(yintercept=0.05, linetype="dashed", color="black")+
  xlab('theta') +
  ylab('null power function')
print(p)
abline(h=0.05)
dev.off()