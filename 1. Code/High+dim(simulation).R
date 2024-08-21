# 1. n100p200s6

n = 100; p = 200; 
rho = 0.5 ;  f = 1/2; level = 0.95; B=25000; sigma=1
beta0 = c(1, -1, 0.5, -0.5, 0.2, -0.2, rep(0, p - 6)) ##s6

## Fixed
result_obj ='n100p200s6_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())


## Estimated
true_vars = seq(6)

### lam = 0.1--RCV 
lamb = 0.1
result_obj ='n100p200s6_RCV'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())


# 2. n100p300s6

n = 100; p = 300; 
rho = 0.5 ;  f = 1/2; level = 0.95; B=25000; sigma=1
beta0 = c(1, -1, 0.5, -0.5, 0.2, -0.2, rep(0, p - 6)) ##s6

## Fixed
result_obj ='n100p300s6_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())


## Estimated
true_vars = seq(6)

### lam = 0.1--RCV 
lamb = 0.1
result_obj ='n100p300s6_RCV'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())


# 3. n100p500s6

n = 100; p = 500; 
rho = 0.5 ;  f = 1/2; level = 0.95; B=25000; sigma=1
beta0 = c(1, -1, 0.5, -0.5, 0.2, -0.2, rep(0, p - 6)) ##s6

## Fixed
result_obj ='n100p500s6_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())


## Estimated
true_vars = seq(6)

### lam = 0.1--RCV 
lamb = 0.1
result_obj ='n100p500s6_RCV'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())


save.image(file = "high+dimensionality.RData")
