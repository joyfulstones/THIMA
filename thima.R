
##  the lasso projection  based method
thima <- function(target.ID, X, MT,Y){
library(hdi)
library(MASS)

MT <- matrix(as.numeric(scale(MT)),nrow(scale(MT)),ncol(scale(MT)))
XM <- cbind(X,MT)
fit.lasso  <- lasso.proj(x=XM, y=Y)
beta_est <- fit.lasso$bhat[2]
beta_se  <- fit.lasso$se[2]

## the LS estimator for alpha
  lm.fit <- lm(MT[,1]~X)
  lm.out <- summary(lm.fit)
  alpha_est <- lm.out$coefficients[2,1]
  alpha_se <- lm.out$coefficients[2,2]


#### The joint test method
P_x <-  2*(1-pnorm(abs(alpha_est/alpha_se),0,1))
p_y <-  2*(1-pnorm(abs(beta_est/beta_se),0,1))
P_Mac  <- max(P_x,p_y)

##
out_result <- list(target.ID = target.ID, alpha_hat = alpha_est, alpha_hat_SE = alpha_se,  beta_hat = beta_est, beta_hat_SE = beta_se,  Pval = P_Mac)
return(out_result)

}








