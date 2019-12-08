### Gibbs sampler for a beta-binomial distribution, Example 2.7.
gibbs-beta <- function(a,b,n,N)
{
theta <- rep(0,N)
x <- rep(0,N)
theta[1] <- rbeta(1,a,b)
x[1] <- rbinom(1,n,theta[1])
for(i in 2:N){
theta[i] <- rbeta(1,a+x[i-1],b+n-x[i-1])
x[i] <- rbinom(1,n,theta[i])
}
return(data.frame(x,theta))
}
