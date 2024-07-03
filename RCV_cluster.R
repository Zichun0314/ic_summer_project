# Load the required package
library(stabs)
library(lars)
library(glmnet)
library(selectiveInference)
library(mvtnorm)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(patchwork)
library(ggh4x)
library(kableExtra)
library(caret)



# Selection algorithm -- Lasso, fixed lambda
Selection = function(y,X, p){
  
  fit = glmnet(X, y, intercept = FALSE,standardize = TRUE)
  n = length(y)
  lam = sqrt(2*log(p)/n)*n   ## universal lambda parameter (liu et.al 2018)
  
  coefs = coef(fit, s = lam/n)[-1] ## get the coefficients excluding the intercept
  sel = which(coefs != 0)  ## M
  out = rep(F, p)
  out[sel] = TRUE
  return(out)
}


# Normal Confidence interval
confint.truesigma <- function(object, parm, level = 0.95, truesigma)
  ## estimate=False : truesigma = the true value of sigma
  ## estimate=True : truesigma = the estimator of sigma
{
  cf = coef(object) ## get the coefficients
  
  pnames = names(cf) ## name of the coefficients
  if(missing(parm)) parm = pnames
  else if(is.numeric(parm)) parm = pnames[parm]
  
  a = (1 - level)/2
  a = c(a, 1 - a)
  pct = format(a, 3)
  
  fac = qnorm(a) # normal quantile
  
  ci = array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  invXtX = solve(crossprod(model.matrix(object)), tol = 1e-50)
  manual = invXtX * (truesigma^2)
  ses = sqrt(diag(manual))[parm]
  ci[] = cf[parm] + ses %o% fac
  
  return(ci)
}


# Estimator of the standard deviation
sigma_hat = function(y, X, method){
  
  if(is.null(dim(X))){
    n = length(X)
    p = 1
  }else{
    n = dim(X)[1]
    p = dim(X)[2]
  }
  
  if (p < n){# low dimensional
    
    X_full = X
    beta_hat_full = solve(t(X_full)%*%X_full)%*%t(X_full)%*%y
    df = n - p
    sigmahat = sqrt(sum((y - X_full%*%beta_hat_full)^2)/df)
    return(sigmahat)
    
  }else{ # high dimensional
    
    sigmahat = estimateSigma2(X, y,method, intercept=FALSE, standardize = TRUE)
    if(method == "RCV"){
      return(list(sigmahat = sigmahat$sigma, F1_1 = sigmahat$F1_1, F1_2 = sigmahat$F1_2))
    }else{
      return(sigmahat)
    }
    
  }
}

# Function to calculate F1 score
calculate_f1 <- function(selected_vars, true_vars) {
  TP <- length(intersect(selected_vars, true_vars))
  FP <- length(setdiff(selected_vars, true_vars))
  FN <- length(setdiff(true_vars, selected_vars))
  
  F1_score <- TP/(TP+0.5*(FP+FN))
  return(F1_score)
}

# Function to tune the lasso to give sensible F1 scores
LASSO_f1 = function(x, y, true_vars){
  
  
  n = dim(x)[1]
  p = dim(x)[2]
  
  # fit lasso model
  fit = glmnet(x, y, alpha = 1, intercept=FALSE,standardize=TRUE)
    
  # Universial threshold value lambda
  #lamb = sqrt(2*log(p)/n)
  #lamb = sqrt(2*log(150)/100)
  #lamb = 0.4
  #lamb = 0.25
  #lamb = 0.2
  lamb = 0.1
  #lamb = 0.05
  
  # get coefficients
  coefs = coef(fit, x = x, y = y, s = lamb, exact = TRUE)[-1]
    
  # get the indices of the non-zero selected coefficients
  selected_vars = which(coefs != 0)
    
  # compute the f1 score
  F1_score = calculate_f1(selected_vars, true_vars)
    
  return(list(M=selected_vars, F1_score = F1_score))
  
}




estimateSigma2 <- function(x, y, method, intercept=FALSE, standardize=TRUE){
  if(method == 'RCV'){
    
    n = dim(x)[1]
    index = sample(1:n,n/2)
    
    ## divide into two subsets
    x1 = x[index,]
    y1 = y[index]
    x2 = x[-index, ]
    y2 = y[-index]
    
    # Assuming 'true_vars' is a vector containing the indices of true relevant variables
    true_vars <- c(1,2,3,4,5,6) 
    
    # apply LASSO to each subset
    LASSO_1 = LASSO_f1(x1, y1, true_vars)
    LASSO_2 = LASSO_f1(x2, y2, true_vars)
    
    
    M1 = LASSO_1$M
    M2 = LASSO_2$M
    
    # estimate variances
    n_half = n / 2
    
    x2_M1 = x2[, M1]
    P_M1 = x2_M1 %*% solve(t(x2_M1) %*% x2_M1, tol = 1e-50 ) %*% t(x2_M1)
    sigma2_1 = t(y2) %*% (diag(n_half) - P_M1) %*% y2 / (n_half - length(M1))
    
    x1_M2 = x1[, M2]
    P_M2 = x1_M2 %*% solve(t(x1_M2) %*% x1_M2, tol = 1e-50 ) %*% t(x1_M2)
    sigma2_2 = t(y1) %*% (diag(n_half) - P_M2) %*% y1 / (n_half - length(M2))
    
    # calculate weighted variance estimator
    weighted_var = (sigma2_1 * (n_half - length(M1)) + sigma2_2 * (n_half - length(M2))) / 
      (n - length(M1) - length(M2))

    sigma = sqrt(weighted_var[1,1])
    
    return(list(sigma = sigma, F1_1 = LASSO_1$F1_score, F1_2 = LASSO_2$F1_score))
  }
}

##simulation function #######################

COV2 = function(n,p,rho,B,f,level,sigma,method=NULL,estimateVar = FALSE){
  
  gamma = sqrt(1/f - 1)
  k = sqrt((1 + gamma^(-2))) ## used when constructing the confidence interval
  
  Sigma_X = matrix(nrow = p, ncol = p) ## Toeplitz covariance for the design matrix
  for (i in 1:p){
    for (j in 1:p){
      Sigma_X[i, j] = rho^(abs(i - j))
    }
  }
  
  beta0_R = c() ## to store the absolute of the true value of the selected coefficients
  COV_R_HD = c() ## to store the coverage probability
  LENGTH_R = c()  ## to store the length of the confidence interval
  if(estimateVar){
    estimated_sigma = c() ## to store the estimator of sigma
    
    if(method == "RCV"){
      F1 = c()
      F2 = c()
    }
  }
  
  
  pb = txtProgressBar(min = 0, max = 100, style = 3, width = 50, char = "=") 
  
  for(b in 1:B){
    set.seed(1112*b)
    
    ########################### DATA ###########################
    beta0 = c(1, -1, 0.5, -0.5, 0.2, -0.2, rep(0, p - 6))    ## First 6 betas are non-zero
    
    X = rmvnorm(n, mean = rep(0, p), sigma = Sigma_X)
    
    y = X%*%beta0 + rnorm(dim(X)[1], sd = sigma)
    
    ## Estimated sigma
    if (estimateVar){
      result <- tryCatch({
        if (method == "RCV"){
          RCVsigma = sigma_hat(y, X, method)
          est_sig = RCVsigma$sigmahat
          F1 = c(F1, RCVsigma$F1_1)
          F2 = c(F2, RCVsigma$F1_2)
          estimated_sigma = c(estimated_sigma, est_sig)
        } else {
          est_sig = sigma_hat(y, X, method)
          estimated_sigma = c(estimated_sigma, est_sig)
        }
        TRUE  
      }, error = function(e){
        message("Error at iteration ", b, ": ", e$message)
        FALSE  
      })
      
      if (!result) {
        next  
      }
    }
    #################### U and V ####################
    
    ## selection using (U,X)
    selection_result <- tryCatch({
      if (estimateVar) {
        w = rnorm(n, sd = est_sig) ## use the estimator of the sigma
      } else {
        w = rnorm(n, sd = sigma) ## use the true sigma
      }
      
      u = y + gamma * w
      S_R = Selection(u, X, p = p)
      TRUE  
    }, error = function(e){
      message("Error at iteration ", b, " during selection: ", e$message)
      
      FALSE 
    })
    
    if (!selection_result) {
      next  
    }
    
    
    
  
    ## projection coefficients inference using (V,X_M)
    if(sum(S_R) != 0){
      X_inf = X[ , S_R] ## X_M
      v = y - w/gamma
      beta_R_HD = solve(t(X_inf)%*%X_inf)%*%t(X_inf)%*%X%*%beta0   ## beta_M :actual partial target coefficient upon model selection
      model = lm(v ~ X_inf - 1)
      
      if(estimateVar){ ## when sigma is unknown, use the estimator of the sigma
        CIs_R_HD = confint.truesigma(model, level= level, truesigma = k*est_sig)
      }else{ ## when sigma is known, use the true value of sigma
        CIs_R_HD = confint.truesigma(model, level = level, truesigma = k*sigma)    
      } 
    }
    
    ######################################
    beta0_R = c(beta0_R, abs(beta0[S_R]))
    ######################################
    
    ## RESULTS
    if (sum(S_R) == 1){
      COV_R_HD = c(COV_R_HD, (CIs_R_HD[1] < beta_R_HD)*(CIs_R_HD[2] > beta_R_HD)) ## the proportion of beta_M lying in the CI
      LENGTH_R = c(LENGTH_R,  CIs_R_HD[2] - CIs_R_HD[1]) 
    }
    
    if (sum(S_R) > 1){
      COV_R_HD = c(COV_R_HD, (CIs_R_HD[ , 1] < beta_R_HD)*(CIs_R_HD[ , 2] > beta_R_HD))
      LENGTH_R = c(LENGTH_R,  CIs_R_HD[ , 2] - CIs_R_HD[ , 1])
    }
    
    setTxtProgressBar(pb, 100*b/B)
    
  } 
  
  close(pb)
  
  if(estimateVar){
    if(method == "RCV"){
      return(list( beta0_R=beta0_R,  COV_R_HD=COV_R_HD,  LENGTH_R=LENGTH_R, estimated_sigma = estimated_sigma,  F1 = F1, F2=F2 ))
    }else{
      return(list( beta0_R=beta0_R,  COV_R_HD=COV_R_HD,  LENGTH_R=LENGTH_R, estimated_sigma = estimated_sigma ))
    }
    
  }else{
    return(list( beta0_R=beta0_R,  COV_R_HD=COV_R_HD,  LENGTH_R=LENGTH_R))
  }
  
}

## EXAMPLE

n = 100; p = 150; rho = 0.5 ;  f = 1/2; level = 0.95; B=10000; sigma=1

## estimated simga -- RCV (failed)
method = 'RCV'
result_obj ='n100p150_RCV8'
assign(result_obj,COV2(n,p,rho,B,f,level,sigma,method,estimateVar = TRUE),envir = parent.frame())


