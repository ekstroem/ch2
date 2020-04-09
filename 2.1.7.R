### Importance sampling, Example 2.3
meth2 <- function(N)
{
y <- rexp(N) ### trial distribution Exp(1)
w <- y^2*3*exp(-y^3+y) ### weights
h <- (y*4/3)[y<0.4]
W <- w[y<0.4]
return(sum(h*W)/sum(w)) ### integral value
}
meth2(1000) ### carry out the method
