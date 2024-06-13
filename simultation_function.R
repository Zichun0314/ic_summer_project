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

# Selection algorithm -- Lasso, fixed lambda
Selection = function(y,X, p){
  
  fit = glmnet(X, y, intercept = FALSE, standardize = TRUE)
  n = length(y)
  lam = sqrt(2*log(p)/n)*n   ## universal lambda parameter (liu et.al 2018)
  
  coefs = coef(fit, s = lam/n)[-1] ## get the coefficients excluding the intercept
  sel = which(coefs != 0)  ## M
  out = rep(F, p)
  out[sel] = TRUE
  return(out)
}


# Normal Confidence interval
confint.truesigma <- function(object, parm, level = 0.95, truesigma, df=NULL, estimate=FALSE)
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
  
  if(estimate){
    fac = qt(a, df=df) ## when sigma is unknown
  }else{
    fac = qnorm(a) ## when sigma is known
  }
  
  ci = array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  invXtX = solve(crossprod(model.matrix(object)), tol = 1e-50)
  manual = invXtX * (truesigma^2)
  ses = sqrt(diag(manual))[parm]
  ci[] = cf[parm] + ses %o% fac
  
  return(ci)
}


# Estimator of the standard deviation
sigma_hat = function(y, X){
  
  if(is.null(dim(X))){
    n = length(X)
    p = 1
  }else{
    n = dim(X)[1]
    p = dim(X)[2]
  }
  
  if (p < n/4){# low dimensional
    
    X_full = X
    beta_hat_full = solve(t(X_full)%*%X_full)%*%t(X_full)%*%y
    df = n - p
    sigmahat = sqrt(sum((y - X_full%*%beta_hat_full)^2)/df)
    return(list(sigmahat=sigmahat, df=df))
    
  }else{ # high dimensional
    
    est = estimateSigma2(X, y, intercept=FALSE)
    return(list(sigmahat=est$sigmahat, df=est$df))
  }
}

## estimate Sigma in high dimensional case
estimateSigma2 <- function(x, y, intercept=FALSE, standardize=TRUE){
 
  cvfit=cv.glmnet(x,y,intercept=intercept,standardize=standardize)
  lamhat=cvfit$lambda.min
  fit=glmnet(x,y,standardize=standardize, intercept=intercept)
  yhat=predict(fit,x,s=lamhat)
  nz=sum(predict(fit,s=lamhat, type="coef")[-1, ] !=0)
  
  df = max(length(y)- nz, 1)  ## prevent from dividing by 0
  sigma=sqrt(sum((y-yhat)^2)/(df))
  return(list(sigmahat=sigma, df=df))
}


##simulation function #######################

COV2 = function(n,p,rho,B,f,level,sigma,estimateVar = FALSE){
  
  gamma = sqrt(1/f - 1)
  k = sqrt((1 + gamma^(-2))) ## used when constructing the confidence interval
  
  Sigma_X = matrix(nrow = p, ncol = p) ## Teoplitz covariance for the design matrix
  for (i in 1:p){
    for (j in 1:p){
      Sigma_X[i, j] = rho^(abs(i - j))
    }
  }
  
  
  beta0_R = c() ## to store the absolute of the true value of the selected coefficients
  COV_R_HD = c() ## to store the coverage probability
  LENGTH_R = c()  ## to store the length of the confidence interval
  
  
  pb = txtProgressBar(min = 0, max = 100, style = 3, width = 50, char = "=") 
  
  for(b in 1:B){
    set.seed(1112*b)
    
    ########################### DATA ###########################
    beta0 = c(1, -1, 0.5, -0.5, 0.2, -0.2, rep(0, p - 6))    ## First 6 betas are non-zero
    
    X = rmvnorm(n, mean = rep(0, p), sigma = Sigma_X)
    
    y = X%*%beta0 + rnorm(dim(X)[1], sd = sigma)
    
    ## Estimated sigma
    est_sig = sigma_hat(y, X)
    
    #################### U and V ####################
    
    
    ## selection using (U,X)
    
    if(estimateVar){
      w = rnorm(n, sd = est_sig$sigmahat) ## use the estimator of the sigma
    }else{
      w = rnorm(n, sd = sigma) ## use the true sigma
    }
    
    u = y + gamma * w
    S_R = Selection(u, X, p = p)
    
    
    ## projection coefficients inference using (V,X_M)
    if(sum(S_R) != 0){
      X_inf = X[ , S_R] ## X_M
      v = y - w/gamma
      beta_R_HD = solve(t(X_inf)%*%X_inf)%*%t(X_inf)%*%X%*%beta0   ## beta_M :actual partial target coefficient upon model selection
      model = lm(v ~ X_inf - 1)
      
      if(estimateVar){ ## when sigma is unknown, use the estimator of the sigma
        CIs_R_HD = confint.truesigma(model, level= level, truesigma = k*est_sig$sigmahat, df=est_sig$df, estimate = TRUE)
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
  
  return(list( beta0_R=beta0_R,  COV_R_HD=COV_R_HD,  LENGTH_R=LENGTH_R))
  
  
}


# running function ###########################
run_result_cov2 = function(n, p, rhos, B, fs, level, sigma, estimateVar = FALSE, save=FALSE, load=FALSE){
  
  count = 1
  
  for(j in rhos){
    for (k in fs){
      
      ############ Assign object ##########
      if(estimateVar){
        result_obj = paste0('cov_pj', count, '_est_sigma_',sigma)  ## e.g. cov_pj1_est_sigma_1
      }else{
        result_obj = paste0("cov_pj", count, '_fixed_sigma_', sigma)  ## e.g. cov_pj1_fixed_sigma_1
      }
      
      if(!save & !load){
        assign(result_obj, COV2(n, p = p, rho = j, B, f = k, 
                                level = level, sigma=sigma,
                                estimateVar = estimateVar), envir = parent.frame())
      }
      
      ############ Save object ##########
      if(save){
        if(estimateVar){
          filename = paste0(result_obj, '.Rdata')
        }else{
          filename = paste0(result_obj, '.Rdata')
        }
        save(list=result_obj, file=filename)
      }
      
      ############ Load object ##########
      if(load){
        if(estimateVar){
          filename = paste0(result_obj, '.Rdata')
        }else{
          filename = paste0(result_obj, '.Rdata')
        }
        load(file=filename, verbose=TRUE, envir = .GlobalEnv)
      }
      
      #  count + 1
      count = count + 1
      
    }
  }
}
