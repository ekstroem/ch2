### Hard shell gas model
x <- c(1:4)
y <- c(1:4)
r <- 0.5 ### start: four balls with radius r
s <- sample(1:4)
s1 <- s[1] ### index of the randomly chosen ball
s2 <- s[2:4] ### indices of the not selected balls
xnew <- x[s1]+2*rnorm(1)
ynew <- y[s1]+2*rnorm(1) ### move the picked ball
## test that the shifted ball do not overlap with the other balls
for (k in s2){
if((x[k]-xnew)^2+(y[k]-ynew)^2>4*r^2)
x[s1] <- xnew
y[s1] <- ynew
}
### the procedure:
gasmodel <- function(N,x,y,r,a)
{
n <- length(x)
for (i in 1:N){
L <- TRUE
while(L){
s <- sample(1:n);s1 <- s[1];s2 <- s[2:n]
xnew <- x[s1]+a*rnorm(1)
ynew <- y[s1]+a*rnorm(1)
for (k in s2){
if ((x[k]-xnew)^2+(y[k]-ynew)^2>4*r^2)
{L <- FALSE}}
x[s1] <- xnew
y[s1] <- ynew
}
}
return(data.frame(x,y))
}
