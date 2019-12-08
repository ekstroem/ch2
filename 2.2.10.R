###Random-walk Metropolis, Example 2.5
MCMC <- function(a,seed,N)
{
rand <- rep(NA,N)
rand[1] <- seed
for(i in 2:N) {
rand[i] <- seed+a*runif(1,-1,1)
r <- min(1,exp(0.5*(seed^2-rand[i]^2)))
if (runif(1)<r){seed <- rand[i]}else{rand[i] <- seed}
}
return(rand)
}
M1 <- MCMC(0.01,0,1000)
plot.ts(M1,ylab="",main="Simulation with a=0.01")
acf(M1,lag.max=50) ### autocorrelation graph
