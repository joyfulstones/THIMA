
##  the lasso projection  based method
thima <- function(ID, X, M,Y){
library(hdi)
library(MASS)
XM <- cbind(X,M)
fit.lasso  <- lasso.proj(x=XM, y=Y)
beta_est <- fit.lasso$bhat[ID+1]
beta_se  <- fit.lasso$se[ID+1]

## the LS estimator for alpha
  lm.fit <- lm(M[,ID]~X)
  lm.out <- summary(lm.fit)
  alpha_est <- lm.out$coefficients[2,1]
  alpha_se <- lm.out$coefficients[2,2]


#### The joint test method
P_x <-  2*(1-pnorm(abs(alpha_est/alpha_se),0,1))
p_y <-  2*(1-pnorm(abs(beta_est/beta_se),0,1))
P_Mac  <- max(P_x,p_y)

##
out_result <- list(index = ID, alpha_hat = alpha_est, beta_hat = beta_est, Pval = P_Mac)
return(out_result)

}








