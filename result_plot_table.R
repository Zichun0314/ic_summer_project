################################################
### run results 1 -- p30########################
################################################

n = 200; p = 30; rhos = c(0,0.5) ;  fs = c(1/2,3/4); level = 0.95


## sigma = 1

# fixed sigma
run_result_cov2(n,p,rhos,B = 2000, fs, level, sigma=1, estimateVar = FALSE)
run_result_cov2(n,p,rhos,B = 2000, fs, level, sigma=1, estimateVar = FALSE,save = TRUE)
# estimated sigma
run_result_cov2(n,p,rhos,B = 2000, fs, level,  sigma=1, estimateVar = TRUE)
run_result_cov2(n,p,rhos,B = 2000, fs, level,  sigma=1, estimateVar = TRUE,save = TRUE)


### Generate full data frame of results ##########

############# sigma = 1 #############


cov_fixed_sigma1_df = generatedf(rhos, fs, sigma=1, estimateVar=FALSE)
cov_est_sigma1_df = generatedf(rhos, fs, sigma=1, estimateVar=TRUE)


normal_p30_sigma1_dfs = list(est = cov_est_sigma1_df, fixed = cov_fixed_sigma1_df)  ## KEEP THIS ORDER


### Generate output plot+table ###################


normal_p30_sigma1_final_results = generateResult(normal_p30_sigma1_dfs)

normal_p30_sigma1_final_results[1]

normal_p30_sigma1_final_results[2]



################################################
### run results 2 -- p100########################
################################################

n = 200; p = 100; rhos = c(0,0.5) ;  fs = c(1/2,3/4); level = 0.9


## sigma = 1

# fixed sigma
run_result_cov2(n,p,rhos,B = 2000, fs, level, sigma=1, estimateVar = FALSE)
run_result_cov2(n,p,rhos,B = 2000, fs, level, sigma=1, estimateVar = FALSE,save = TRUE)
# estimated sigma
run_result_cov2(n,p,rhos,B = 2000, fs, level,  sigma=1, estimateVar = TRUE)
run_result_cov2(n,p,rhos,B = 2000, fs, level,  sigma=1, estimateVar = TRUE,save = TRUE)


### Generate full data frame of results ###########################

############# sigma = 1 #############


cov_fixed_sigma1_df = generatedf(rhos, fs, sigma=1, estimateVar=FALSE)
cov_est_sigma1_df = generatedf(rhos, fs, sigma=1, estimateVar=TRUE)


normal_p100_sigma1_dfs = list(est = cov_est_sigma1_df, fixed = cov_fixed_sigma1_df)  ## KEEP THIS ORDER


### Generate output plot+table ###########################


normal_p100_sigma1_final_results = generateResult(normal_p100_sigma1_dfs)

normal_p100_sigma1_final_results[1]

normal_p100_sigma1_final_results[2]




################################################
### run results 3 -- n100p150########################
################################################

n = 100; p = 150; rhos = c(0,0.5) ;  fs = c(1/2,3/4); level = 0.9


## sigma = 1

# fixed sigma
run_result_cov2(n,p,rhos,B = 2000, fs, level, sigma=1, estimateVar = FALSE)
run_result_cov2(n,p,rhos,B = 2000, fs, level, sigma=1, estimateVar = FALSE,save = TRUE)
# estimated sigma
run_result_cov2(n,p,rhos,B = 2000, fs, level,  sigma=1, estimateVar = TRUE)
run_result_cov2(n,p,rhos,B = 2000, fs, level,  sigma=1, estimateVar = TRUE,save = TRUE)


### Generate full data frame of results ###########################

############# sigma = 1 #############


cov_fixed_sigma1_df = generatedf(rhos, fs, sigma=1, estimateVar=FALSE)
cov_est_sigma1_df = generatedf(rhos, fs, sigma=1, estimateVar=TRUE)


normal_n100p150_sigma1_dfs = list(est = cov_est_sigma1_df, fixed = cov_fixed_sigma1_df)  ## KEEP THIS ORDER


### Generate output plot+table ###########################


normal_n100p150_sigma1_final_results = generateResult(normal_n100p150_sigma1_dfs)

normal_n100p150_sigma1_final_results[1]

normal_n100p150_sigma1_final_results[2]












