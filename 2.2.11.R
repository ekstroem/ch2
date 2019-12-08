### Gibbs sampler for bivariate normal variables, Example 2.6.
gibbs-norm<-function(rho,N)
{
x <- rep(0,N)
y <- rep(0,N)
for(i in 1:N){x[i] <- rnorm(1,rho*y[i],(1-rho^2))
y[i] <- rnorm(1,rho*x[i],(1-rho^2))
}
return(data.frame(x,y))
}
