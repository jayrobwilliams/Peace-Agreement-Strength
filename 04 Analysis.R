#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: peace agreement strength ##
## created: May 11, 2016             ##
## updated: December 15, 2018        ##
#######################################

#################
## 04 analysis ##
#################



## print script to identify in log
print(paste('Peace Agreement Measurement Models Started', Sys.time()))

## set seed for replication
set.seed(868549)

## set locale to deal with encoding issues for Lomé Peace Agreement
Sys.setlocale(category = 'LC_ALL', locale = 'en_US.UTF-8') 

library(tidyverse)
library(doParallel)
library(doRNG)
library(mice)
library(rstan)
rstan_options(auto_write = T) # save compiled model to disk for subsequent access

## create output directory
dir.create('Knitr\ Input')



## MI sampler prep ####

## the line of code below dynamically gets the number of cores available to the
## machine and creates half as many doParallel workers so that each worker can
## use two cores to run two chains. this code was not used in the creation of the
## results presented in the paper due to issues with exactly replicating results.
## if you uncomment it, your results may not exactly match those in the paper.
## leaving it commented out will generate warnings about the lack of a parallel
## backend, but the code will execute succesfully in serial (but using 2 cores
## within each foreach() loop via rstan's parallelization)
#registerDoParallel(parallel::detectCores() %/% 2)

## number of iterations for body and appendix models
it <- 9e4
it_app <- 6e3

## compile stan models for sampling in foreach loops
stan_model_full_prob <- stan_model('Stan Models/IRT Full Probability.stan')
stan_model_irt <- stan_model('Stan Models/IRT.stan')
stan_model_lm <- stan_model('Stan Models/Linear Model.stan')



## peace agreements data ####

## read in prepared peace agreement data
PA <- readRDS('PA.RDS')

## subset data to just variables used in analyses before imputation; drop border
## demarcation b/c no variation in civil wars
PA <- PA %>%
  select(PAID, pa_name, Name, Year, CID, COWcode, cease:Co_impl, pa_type,
         Inc, cold_war, Type, intervention_acd, intervention_imi, CumInt,
         sanction, rpr_work, gdppc, aidpct, polity2, mediation, add_ind,
         duration, -Demarcation)

## get names of all provisions
provisions_all <- names(PA %>% select(cease:Co_impl))

## drop provisions that are irrelevant in model w/ all provisions
provisions <- provisions_all[!(provisions_all %in% c('Aut', 'Fed', 'Ind', 'Ref',
                                                     'Shaloc', 'Regdev', 'Cul',
                                                     'Locgov', 'Outlin'))]

## drop peacekeeping provision for robustness check
provisions_npk <- provisions[provisions != 'PKO']

## the identification restriction with the lower row number NEEDS to come first
## because of the way that Stan handles indexing with the code that combines the
## estimated and fixed values in the model

## get row numbers for upper and lower bound identification restrictions
id_lo <- which(PA$pa_name == 'Tripoli Agreement')
id_hi <- which(PA$pa_name == 'DUP/SPLM Sudan Peace Agreement')

## get row numbers for upper and lower bound identification restriction sensitivity
id_lo_1 <- which(PA$pa_name == 'New York Agreement')
id_hi_1 <- which(PA$pa_name == 'Lomé Peace Agreement')
id_lo_2 <- which(PA$pa_name == 'The Lusaka Protocol')
id_hi_2 <- which(PA$pa_name == 'Accra III')
id_lo_3 <- which(PA$pa_name == 'The Oslo Accord')
id_hi_3 <- which(PA$pa_name == 'The General Framework Agreement for Peace in Bosnia and Herzegovina (the Dayton Agreement)')

## set fixed values for first and second identification restriction, ensuring that
## each restriction gets the appropriate sign
theta_lo <- 1
theta_hi <- -1
theta_lo_1 <- -1
theta_hi_1 <- 1
theta_lo_2 <- 1
theta_hi_2 <- -1
theta_lo_3 <- -1
theta_hi_3 <- 1

## save PA names to re-add after imputation
PA_names <- PA %>% select(PAID, CID, Name, pa_name, Year)

## drop PA names and start and end dates
PA <- PA %>% select(-PAID, -Name, -pa_name, -COWcode, -Year, -CID)

## impute missing data
PA_mi <- mice(PA, seed = 79543,
              pred = quickpred(PA, exclude = c('add_ind',
                                               'duration',
                                               'Inc',
                                               provisions_all)))

## extract each imputed dataset from MIDS object
PA_list <- mice::complete(PA_mi, action = 'long')

## split imputed datasets into list
PA_list <- split(PA_list, rep(1:5,each=nrow(PA)))

## re-attach PA names and IDs
PA_list <- lapply(PA_list, function(x) data.frame(PA_names, x))



## simple additive index ####

## form input data and run sampler for each imputed dataset
agmt_add_ind <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_imi
                                   + as.factor(Inc) + CumInt + cold_war,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct', 'rpr_work', 'polity2')]))[, -1]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(PA_list[[i]]$CID))
  
  ## data list for model
  stan_data_list <- list(N = nrow(covariates), K = ncol(covariates),
                         C = length(unique(confs)), confs = confs,
                         X = covariates, y = PA_list[[i]]$add_ind)
  
  ## draw samples from posterior
  sampling(stan_model_lm, data = stan_data_list,
           pars = c('beta', 'mu_delta', 'delta', 'sigma'),
           chains = 2, iter = it_app, warmup = floor(it_app/3), seed = 782940)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_add_ind <- sflist2stanfit(agmt_add_ind)

## save stanfit and remove to free up memory
save(agmt_add_ind, file = 'Knitr Input/agmt_add_ind.RData')
rm(agmt_add_ind)



## full probability model of agreement strength and signing ####

## form input data and run sampler for each imputed dataset; all provisions
agmt_full_prob_all_inds <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions_all])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_imi
                                   + as.factor(Inc) + CumInt + cold_war,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct', 'rpr_work', 'polity2')]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo, id_hi), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- covariates[, -c(1, ncol(covariates))]
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list, pars = 'gamma',
           chains = 2, iter = it_app, warmup = floor(it_app/3), seed = 13495)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_all_inds <- sflist2stanfit(agmt_full_prob_all_inds)

## save stanfit and remove to free up memory
save(agmt_full_prob_all_inds, file = 'Knitr Input/agmt_full_prob_all_inds.RData')
rm(agmt_full_prob_all_inds)

## form input data and run sampler for each imputed dataset; no peacekeeping provision
agmt_full_prob_npk <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions_npk])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_imi
                                   + as.factor(Inc) + CumInt + cold_war,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct', 'rpr_work', 'polity2')]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo, id_hi), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- covariates[, -c(1, ncol(covariates))]
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'mu_delta'), chains = 2, iter = it_app,
           warmup = floor(it_app/3), seed = 92748)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_npk <- sflist2stanfit(agmt_full_prob_npk)

## save stanfit and remove to free up memory
save(agmt_full_prob_npk, file = 'Knitr Input/agmt_full_prob_npk.RData')
rm(agmt_full_prob_npk)

## form input data and run sampler for each imputed dataset; sanction only
agmt_full_prob_sanc <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ sanction, data = PA_list[[i]]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo, id_hi), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- as.matrix(covariates[, -c(1, ncol(covariates))])
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'mu_delta'), chains = 2, iter = it_app,
           warmup = floor(it_app/3), seed = 713495)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_sanc <- sflist2stanfit(agmt_full_prob_sanc)

## save stanfit and remove to free up memory
save(agmt_full_prob_sanc, file = 'Knitr Input/agmt_full_prob_sanc.RData')
rm(agmt_full_prob_sanc)

## form input data and run sampler for each imputed dataset; mediation only
agmt_full_prob_med <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ mediation, data = PA_list[[i]]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo, id_hi), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- as.matrix(covariates[, -c(1, ncol(covariates))])
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'mu_delta'), chains = 2, iter = it_app,
           warmup = floor(it_app/3), seed = 713495)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_med <- sflist2stanfit(agmt_full_prob_med)

## save stanfit and remove to free up memory
save(agmt_full_prob_med, file = 'Knitr Input/agmt_full_prob_med.RData')
rm(agmt_full_prob_med)

## form input data and run sampler for each imputed dataset; intervention only
agmt_full_prob_mil <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ intervention_imi, data = PA_list[[i]]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo, id_hi), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- as.matrix(covariates[, -c(1, ncol(covariates))])
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'mu_delta'), chains = 2, iter = it_app,
           warmup = floor(it_app/3), seed = 713495)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_mil <- sflist2stanfit(agmt_full_prob_mil)

## save stanfit and remove to free up memory
save(agmt_full_prob_mil, file = 'Knitr Input/agmt_full_prob_mil.RData')
rm(agmt_full_prob_mil)

## form input data and run sampler for each imputed dataset; aid only
agmt_full_prob_aid <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ scale(aidpct), data = PA_list[[i]]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo, id_hi), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- as.matrix(covariates[, -c(1, ncol(covariates))])
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'mu_delta'), chains = 2, iter = it_app,
           warmup = floor(it_app/3), seed = 713495)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_aid <- sflist2stanfit(agmt_full_prob_aid)

## save stanfit and remove to free up memory
save(agmt_full_prob_aid, file = 'Knitr Input/agmt_full_prob_aid.RData')
rm(agmt_full_prob_aid)

## form input data and run sampler for each imputed dataset; no controls
agmt_full_prob_nc <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_imi,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct')]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo, id_hi), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- covariates[, -c(1, ncol(covariates))]
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'mu_delta'), chains = 2, iter = it_app,
           warmup = floor(it_app/3), seed = 713495)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_nc <- sflist2stanfit(agmt_full_prob_nc)

## save stanfit and remove to free up memory
save(agmt_full_prob_nc, file = 'Knitr Input/agmt_full_prob_nc.RData')
rm(agmt_full_prob_nc)

## form input data and run sampler for each imputed dataset; no controls w/ ACD intervention
agmt_full_prob_nc_acd <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_acd,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct')]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo, id_hi), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- covariates[, -c(1, ncol(covariates))]
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'mu_delta'), chains = 2, iter = it_app,
           warmup = floor(it_app/3), seed = 21504)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_nc_acd <- sflist2stanfit(agmt_full_prob_nc_acd)

## save stanfit and remove to free up memory
save(agmt_full_prob_nc_acd, file = 'Knitr Input/agmt_full_prob_nc_acd.RData')
rm(agmt_full_prob_nc_acd)

## form input data and run sampler for each imputed dataset; all hierarchical predictors
agmt_full_prob <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_imi
                                   + as.factor(Inc) + CumInt + cold_war,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct', 'rpr_work',
                                             'polity2')]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo, id_hi), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- covariates[, -c(1, ncol(covariates))]
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'gamma', 'alpha', 'theta', 'mu_delta', 'delta'),
           chains = 2, iter = it, warmup = floor(it/3), seed = 13495)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob <- sflist2stanfit(agmt_full_prob)

## append full agreement strength to imputed datasets for presentation
PA_list <- lapply(PA_list, function(x) data.frame(x, full = summary(agmt_full_prob, pars = 'theta')$summary[, c('mean', 'sd')]))

## save stanfit and remove to free up memory
save(agmt_full_prob, file = 'Knitr Input/agmt_full_prob.RData')
rm(agmt_full_prob)

## form input data and run sampler for each imputed dataset; all hierarchical predictors
agmt_full_prob_acd <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_acd
                                   + as.factor(Inc) + CumInt + cold_war,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct', 'rpr_work',
                                             'polity2')]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo, id_hi), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- covariates[, -c(1, ncol(covariates))]
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'gamma', 'alpha', 'theta', 'mu_delta', 'delta'),
           chains = 2, iter = it_app, warmup = floor(it_app/3), seed = 72395)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_acd <- sflist2stanfit(agmt_full_prob_acd)

## save stanfit and remove to free up memory
save(agmt_full_prob_acd, file = 'Knitr Input/agmt_full_prob_acd.RData')
rm(agmt_full_prob_acd)

## form input data and run sampler for each imputed dataset; all hierarchical
## predictors, including agreement comprehensiveness
agmt_full_prob_comp <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_imi
                                   + as.factor(pa_type) + as.factor(Inc)
                                   + CumInt + cold_war,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct', 'rpr_work',
                                             'polity2')]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo, id_hi), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- covariates[, -c(1, ncol(covariates))]
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'gamma', 'alpha', 'theta', 'mu_delta', 'delta'),
           chains = 2, iter = it_app, warmup = floor(it_app/3), seed = 13495)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_comp <- sflist2stanfit(agmt_full_prob_comp)

## append full agreement strength to imputed datasets for presentation
PA_list <- lapply(PA_list, function(x) data.frame(x, full.comp = summary(agmt_full_prob_comp, pars = 'theta')$summary[, c('mean', 'sd')]))

## save stanfit and remove to free up memory
save(agmt_full_prob_comp, file = 'Knitr Input/agmt_full_prob_comp.RData')
rm(agmt_full_prob_comp)

## form input data and run sampler for each imputed dataset; id restriction 1
agmt_full_prob_id_1 <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_imi
                                   + as.factor(Inc) + CumInt + cold_war,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct', 'rpr_work', 'polity2')]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo_1, id_hi_1), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- covariates[, -c(1, ncol(covariates))]
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo_1,
                         theta_hi = theta_hi_1, id_lo = id_lo_1, id_hi = id_hi_1)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'theta', 'mu_delta'), chains = 2,
           iter = it_app, warmup = floor(it_app/3), seed = 13495)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_id_1 <- sflist2stanfit(agmt_full_prob_id_1)

## append full agreement strength to imputed datasets for presentation
PA_list <- lapply(PA_list, function(x) data.frame(x, id_1 = summary(agmt_full_prob_id_1, pars = 'theta')$summary[, c('mean', 'sd')]))

## save stanfit and remove to free up memory
save(agmt_full_prob_id_1, file = 'Knitr Input/agmt_full_prob_id_1.RData')
rm(agmt_full_prob_id_1)

## form input data and run sampler for each imputed dataset; id restriction 2
agmt_full_prob_id_2 <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_imi
                                   + as.factor(Inc) + CumInt + cold_war,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct', 'rpr_work', 'polity2')]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo_2, id_hi_2), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- covariates[, -c(1, ncol(covariates))]
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo_2,
                         theta_hi = theta_hi_2, id_lo = id_lo_2, id_hi = id_hi_2)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'theta', 'mu_delta'), chains = 2,
           iter = it_app, warmup = floor(it_app/3), seed = 13495)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_id_2 <- sflist2stanfit(agmt_full_prob_id_2)

## append full agreement strength to imputed datasets for presentation
PA_list <- lapply(PA_list, function(x) data.frame(x, id_2 = summary(agmt_full_prob_id_2, pars = 'theta')$summary[, c('mean', 'sd')]))

## save stanfit and remove to free up memory
save(agmt_full_prob_id_2, file = 'Knitr Input/agmt_full_prob_id_2.RData')
rm(agmt_full_prob_id_2)

## form input data and run sampler for each imputed dataset; id restriction 3
agmt_full_prob_id_3 <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## covariates for agreement means
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_imi
                                   + as.factor(Inc) + CumInt + cold_war,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct', 'rpr_work', 'polity2')]),
                      PA_list[[i]]$CID)
  
  ## remove fixed observation rows
  covariates <- covariates[-c(id_lo_3, id_hi_3), ]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(covariates[, ncol(covariates)]))
  
  ## remove conflict ID and intercept from covariates b/c grand mean in random int
  covariates <- covariates[, -c(1, ncol(covariates))]
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), K = ncol(covariates),
                         X = indicators.vec, Z = covariates, C = length(unique(confs)),
                         agmt = agreements, inds = inds, confs = confs, theta_lo = theta_lo_3,
                         theta_hi_3 = theta_hi, id_lo = id_lo_3, id_hi = id_hi_3)
  
  ## draw samples from posterior
  sampling(stan_model_full_prob, data = stan_data_list,
           pars = c('beta', 'theta', 'mu_delta'), chains = 2,
           iter = it_app, warmup = floor(it_app/3), seed = 13495)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_full_prob_id_3 <- sflist2stanfit(agmt_full_prob_id_3)

## append full agreement strength to imputed datasets for presentation
PA_list <- lapply(PA_list, function(x) data.frame(x, id_3 = summary(agmt_full_prob_id_3, pars = 'theta')$summary[, c('mean', 'sd')]))

## save stanfit and remove to free up memory
save(agmt_full_prob_id_3, file = 'Knitr Input/agmt_full_prob_id_3.RData')
rm(agmt_full_prob_id_3)



## separate latent agreement strength estimation and signing effects #### 

## form input data and run sampler; only once becuase no missing indicators
agmt_irt <- foreach(i = 1:1, .packages = 'rstan') %dorng% {
  
  ## set stan to use 4 cores per doParallel worker
  library(rstan)
  options(mc.cores = 4)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  indicators.mat <- as.matrix(PA_list[[i]][, provisions])
  
  ## flatten indicators into vector by column major order
  indicators.vec <- c(indicators.mat)
  
  ## create agreement indexing vector
  agreements <- rep(1:nrow(indicators.mat), ncol(indicators.mat))
  
  ## create indicator indexing vector
  inds <- rep(1:ncol(indicators.mat), each = nrow(indicators.mat))
  
  ## data list for model
  stan_data_list <- list(N = length(indicators.vec), O = ncol(indicators.mat),
                         M = nrow(indicators.mat), X = indicators.vec,
                         agmt = agreements, inds = inds, theta_lo = theta_lo,
                         theta_hi = theta_hi, id_lo = id_lo, id_hi = id_hi)
  
  ## draw samples from posterior
  sampling(stan_model_irt, data = stan_data_list, pars = c('gamma', 'theta'),
           chains = 4, iter = it, warmup = floor(it/3), seed = 796803)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_irt <- sflist2stanfit(agmt_irt)

## append mean and SD of agreement strength to imputed datasets for response model
PA_list <- lapply(PA_list, function(x) data.frame(x, irt = summary(agmt_irt, pars = 'theta')$summary[, c('mean', 'sd')]))

## save stanfit and remove to free up memory
save(agmt_irt, file = 'Knitr Input/agmt_irt.RData')
rm(agmt_irt)

## form input data and run sampler for each imputed dataset
agmt_response_nc <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_imi,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct')]))[, -1]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(PA_list[[i]]$CID))
  
  ## data list for model
  stan_data_list <- list(N = nrow(covariates), K = ncol(covariates),
                         C = length(unique(confs)), confs = confs,
                         X = covariates, y = PA_list[[i]]$irt.mean)
  
  ## draw samples from posterior
  sampling(stan_model_lm, data = stan_data_list,
           pars = c('beta', 'mu_delta', 'sigma'),
           chains = 2, iter = it_app, warmup = floor(it_app/3), seed = 394861)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_response_nc <- sflist2stanfit(agmt_response_nc)

## save stanfit and remove to free up memory
save(agmt_response_nc, file = 'Knitr Input/agmt_response_nc.RData')
rm(agmt_response_nc)

## form input data and run sampler for each imputed dataset
agmt_response <- foreach(i = 1:PA_mi$m, .packages = 'rstan') %dorng% {
  
  ## set stan to use 2 cores per doParallel worker
  library(rstan)
  options(mc.cores = 2)
  rstan_options(auto_write = T)
  
  ## observed indicators of agreement strength
  covariates <- cbind(model.matrix(~ sanction + mediation + intervention_imi
                                   + as.factor(Inc) + CumInt + cold_war,
                                   data = PA_list[[i]]),
                      scale(PA_list[[i]][, c('aidpct', 'rpr_work',
                                             'polity2')]))[, -1]
  
  ## conflict ID vector
  confs <- as.numeric(as.factor(PA_list[[i]]$CID))
  
  ## data list for model
  stan_data_list <- list(N = nrow(covariates), K = ncol(covariates),
                         C = length(unique(confs)), confs = confs,
                         X = covariates, y = PA_list[[i]]$irt.mean)
  
  ## draw samples from posterior
  sampling(stan_model_lm, data = stan_data_list,
           pars = c('beta', 'mu_delta', 'delta', 'sigma'),
           chains = 2, iter = it_app, warmup = floor(it_app/3), seed = 782940)
  
}

## pool chains from all 5 imputed datasets into one stanfit
agmt_response <- sflist2stanfit(agmt_response)

## save stanfit and remove to free up memory
save(agmt_response, file = 'Knitr Input/agmt_response.RData')
rm(agmt_response)



## knitr export ####

## save data and models for use in knitr to generate paper
save.image(file = 'Knitr Input/knitr input.RData')

## print secession info for log
sessionInfo()

## print script to verify completion in log
print(paste('Peace Agreement Measurement Models Completed', Sys.time()))

## quit R
quit(save = 'no')



###################
## End of Script ##
###################