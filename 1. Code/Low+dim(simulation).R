# Low-dim
n = 100 
rho = 0.5 ;  f = 1/2; level = 0.95; B=25000; sigma=1


##1. P10
p = 10
beta0 = c(1, -1, 0.5, -0.5, 0.2, -0.2, rep(0, p - 6))  ##S6

### Fixed
result_obj ='n100p10s6_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())

### Estimated
result_obj ='n100p10s6_Low'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = TRUE),envir = parent.frame())



##2. P20
p = 20
beta0 = c(1, -1, 0.5, -0.5, 0.2, -0.2, rep(0, p - 6))  ##S6

### Fixed
result_obj ='n100p20s6_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())

### Estimated
result_obj ='n100p20s6_Low'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = TRUE),envir = parent.frame())


##3. P40
p = 40
beta0 = c(1, -1, 0.5, -0.5, 0.2, -0.2, rep(0, p - 6))  ##S6

### Fixed
result_obj ='n100p40s6_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())

### Estimated
result_obj ='n100p40s6_Low'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = TRUE),envir = parent.frame())



##4. P80
p = 80
beta0 = c(1, -1, 0.5, -0.5, 0.2, -0.2, rep(0, p - 6))  ##S6

### Fixed
result_obj ='n100p80s6_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())

### Estimated
result_obj ='n100p80s6_Low'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = TRUE),envir = parent.frame())


save.image(file = "low+dimensionality.RData")

