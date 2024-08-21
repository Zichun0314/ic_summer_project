#1. n100p150s6

n = 100; p = 150; 
rho = 0.5 ;  f = 1/2; level = 0.95; B=25000; sigma=1
beta0 = c(1, -1, 0.5, -0.5, 0.2, -0.2, rep(0, p - 6)) ##s6

## Fixed
result_obj ='n100p150s6_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())

## Estimated
true_vars = seq(6)

### lam = 0.2--RCV1
lamb = 0.2
result_obj ='n100p150s6_RCV1'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.1--RCV2
lamb = 0.1
result_obj ='n100p150s6_RCV2'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.05--RCV3
lamb = 0.05
result_obj ='n100p150s6_RCV3'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

###########################################################

#2. n100p150s10

n = 100; p = 150; 
rho = 0.5 ;  f = 1/2; level = 0.95; B=25000; sigma=1
beta0 = c(1, -1, 0.8, -0.8, 0.6, -0.6, 0.4, -0.4, 0.2, -0.2, rep(0, p - 10)) ##s10

## Fixed
result_obj ='n100p150s10_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())


## Estimated
true_vars = seq(10)

### lam = 0.2--RCV1 
lamb = 0.2
result_obj ='n100p150s10_RCV1'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.1--RCV2 
lamb = 0.1
result_obj ='n100p150s10_RCV2'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.05--RCV3 
lamb = 0.05
result_obj ='n100p150s10_RCV3'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())


#########################################################

#3. n100p150s14

n = 100; p = 150; 
rho = 0.5 ;  f = 1/2; level = 0.95; B=25000; sigma=1
beta0 = c(1, -1, 0.8, -0.8,0.7,-0.7, 0.6,-0.6,0.5,-0.5,0.4,-0.4,0.2, -0.2, rep(0, p - 14))

## Fixed
result_obj ='n100p150s14_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())

## Estimated
true_vars = seq(14)

### lam = 0.2--RCV1 
lamb = 0.2
result_obj ='n100p150s14_RCV1'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.1--RCV2 
lamb = 0.1
result_obj ='n100p150s14_RCV2'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.05--RCV3 
lamb = 0.05
result_obj ='n100p150s14_RCV3'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())


#########################################################

#4. n100p150s20

n = 100; p = 150; 
rho = 0.5 ;  f = 1/2; level = 0.95; B=25000; sigma=1
beta0 = c(1, -1, 0.9,-0.9,0.8, -0.8,0.7,-0.7, 0.6,-0.6,0.5,-0.5,0.4,-0.4,0.3,-0.3,0.2, -0.2, 0.1,-
            0.1,rep(0, p - 20))

## Fixed
result_obj ='n100p150s20_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())

## Estimated
true_vars = seq(20)

### lam = 0.2--RCV1 
lamb = 0.2
result_obj ='n100p150s20_RCV1'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.1--RCV2 
lamb = 0.1
result_obj ='n100p150s20_RCV2'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.05--RCV3 
lamb = 0.05
result_obj ='n100p150s20_RCV3'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

#########################################################

# 5.n100p150s50

n = 100; p = 150; 
rho = 0.5 ;  f = 1/2; level = 0.95; B=25000; sigma=1

beta0 = c(1, -1, 0.9,-0.9,0.8, -0.8,0.7,-0.7, 0.6,-0.6,
          0.5,-0.5,0.4,-0.4,0.3,-0.3,0.2, -0.2, 0.1,-0.1,
          runif(30,0,0.1),rep(0, p - 50)) ##s50

## Fixed
result_obj ='n100p150s50_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())

## Estimated
true_vars = seq(50)

## lam = 0.2--RCV1 
lamb = 0.2
result_obj ='n100p150s50_RCV1'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

## lam = 0.1 -- RCV2
lamb = 0.1
result_obj ='n100p150s50_RCV2'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.05--RCV3 
lamb = 0.05
result_obj ='n100p150s50_RCV3'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())


###########################################################

# 6.n100p150s100

n = 100; p = 150; 
rho = 0.5 ;  f = 1/2; level = 0.95; B=25000; sigma=1

beta0 = c(1, -1, 0.9,-0.9,0.8, -0.8,0.7,-0.7, 0.6,-0.6,
          0.5,-0.5,0.4,-0.4,0.3,-0.3,0.2, -0.2, 0.1,-0.1,
          runif(80,0,0.1),rep(0, p - 100)) ##s100

## Fixed
result_obj ='n100p150s100_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())

## Estimated
true_vars = seq(100)

## lam = 0.2--RCV1 
lamb = 0.2
result_obj ='n100p150s100_RCV1'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

## lam = 0.1 -- RCV2 
lamb = 0.1
result_obj ='n100p150s100_RCV2'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.05--RCV3 
lamb = 0.05
result_obj ='n100p150s100_RCV3'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())


###########################################################

# 7.n100p150s150
n = 100; p = 150; 
rho = 0.5 ;  f = 1/2; level = 0.95; B=25000; sigma=1

beta0 = c(1, -1, 0.9,-0.9,0.8, -0.8,0.7,-0.7, 0.6,-0.6,
          0.5,-0.5,0.4,-0.4,0.3,-0.3,0.2, -0.2, 0.1,-0.1,
          runif(130,0,0.1)) ##s150

## Fixed
result_obj ='n100p150s150_Fixed'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,estimateVar = FALSE),envir = parent.frame())

## Estimated
true_vars = seq(150)

### lam = 0.2 -- RCV1
lamb = 0.2
result_obj ='n100p150s150_RCV1'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.1 -- RCV2
lamb = 0.1
result_obj ='n100p150s150_RCV2'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())

### lam = 0.05 -- RCV3
lamb = 0.05
result_obj ='n100p150s150_RCV3'
assign(result_obj,COV2(n,p,beta0,rho,B,f,level,sigma,true_vars,lamb,estimateVar = TRUE),envir = parent.frame())


#######
save.image(file = "high+sparsity.RData")
