library(MASS)
library(hdi)
tid <- 5 # The index of targeted mediator
n <- 100 # sample size
p <- 200 # the number of mediators
E <- matrix(rexp(n*p,1),n,p)
M <- matrix(0,n,p)
for (i in 1:n){
  M[i,] <- E[i,]/sum(E[i,]) # generate compositional mediators
}
MT <- ilr(target.ID =tid, M) # the ilr-based transformation for M_ID
##### The following codes generate X and Y
c <- 1 # the intercept term in Y
gamma <- 0.5
beta <- matrix(0,1,p-1)
b0 <- c(1,0.25,0.30,0.35,0.55)
L <- length(b0)
beta[1:L] <- b0
X <- matrix(rnorm(n, mean=0, sd=1.5),n,1)
X <- matrix(as.numeric(scale(X)),nrow(scale(X)),ncol(scale(X)))
XM <- cbind(X,MT)
B <- t(t(c(gamma, beta))) # (p) times 1
e <- rnorm(n,0,1)
Y <- c + XM%*%B + t(t(e))
###
thima.fit <- thima(target.ID = tid, X, MT, Y) # using the package THIMA
print(thima.fit)