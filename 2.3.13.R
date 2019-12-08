### Iris data, Example 2.9.
library(MASS)
library(tmvtnorm)### library for truncated multivariate normal
library(truncnorm)### library for truncated normal distribution
data(iris)
iris
### subset only sepal measurements of iris.setosa
V1 <- iris[1:50,1] ### length of sepal
V2 <- iris[1:50,2] ### width of sepal
m1 <- mean(V1)
m2 <- mean(V2)
### boundaries for the one sided truncated normal distribution
a <- c(0,0)
### boundaries for the one sided truncated normal distribution
b <- c(Inf,Inf)
### abc-rejection
abc.rej <- function(N,tol){
rand1 <- rep(NA,N)
rand2 <- rep(NA,N)
rand3 <- rep(NA,N)
rand4 <- rep(NA,N)
rand5 <- rep(NA,N)
for(i in 1:N){
L <- TRUE
while(L){
rand1[i] <- rtruncnorm(1,0,Inf,3,1)
rand2[i] <- rtruncnorm(1,0,Inf,2,1)
rand3[i] <- runif(1,0.02,2)
rand4[i] <- runif(1,0.02,2)
rand5[i] <- runif(1,0,0.9)
S <- matrix(c(rand3[i]^2,rand5[i]*rand3[i]
*rand4[i],rand5[i]*rand3[i]*rand4[i],rand4[i]^2),2,2)
Z <- rtmvnorm(50,c(rand1[i],rand2[i]),S,a,b)
D <- (mean(Z[,1])-m1)^2+(mean(Z[,2])-m2)^2
if(D<tol){
L <- FALSE
}}
}
D <- data.frame(rand1,rand2,rand3,rand4,rand5)
return(D)
}
