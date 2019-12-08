### Independent MC, Example 2.1
meth1<-function(N)
{
x <- rweibull(N,3,1) ### generates N Weibull random variables
z <- x[x<0.4] ### truncation
hz <- z^3*4/3
return(sum(hz)/N) ### integral value
}
meth1(1000) ### carry out the Monte Carlo approximation
