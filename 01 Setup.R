#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: peace agreement strength ##
## created: January 31, 2016         ##
## updated: October 27, 2018         ##
#######################################

##############
## 01 setup ##
##############



## clear environment
rm(list = ls())

## modify makevars if on CentOS to allow compiling rstan from source
if (grepl('CentOS Linux', sessionInfo()$running)) {
  
  dotR <- file.path(Sys.getenv('HOME'), '.R')
  if (!file.exists(dotR)) dir.create(dotR)
  M <- file.path(dotR, 'Makevars')
  if (!file.exists(M)) file.create(M)
  cat('\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC -Wno-unused-variable -Wno-unused-function',
      'CXX14=g++ -std=c++1y',
      file = M, sep = '\n', append = T)
  
}

## required packages
packages <- c('rio', 'tidyverse', 'data.table', 'countrycode', 'reshape2', 'lubridate',
              'rstan', 'coda', 'bayesplot', 'ggmcmc', 'ggridges', 'ggrepel', 'WDI',
              'corrplot', 'xtable', 'texreg', 'mice', 'doParallel', 'doRNG')

## check if any required packages not present
if (length(setdiff(packages, rownames(installed.packages())))) {
  
  ## install any required packages not present
  install.packages(setdiff(packages, rownames(installed.packages())),
                   repos = 'http://cran.us.r-project.org', type = 'source')
  
}

library(dplyr) # data selection and filtering
library(data.table) # between test; needs to be loaded first b/c lots of function overlap
library(countrycode) # convert country names to COW codes
library(reshape2) # collapse observations
library(lubridate) # handle mixed format dates
library(WDI) # world bank API access



## extract data ####
untar('Directories.tar.gz')



## create directory to hold output ####
dir.create('Knitr\ Input')



## define functions ####

## create COW dyad IDs using input of two COW code vectors
COW.dyadid <- function (ccode1, ccode2) {
  
  ## multiply the smaller COW code by 1,000 and add the larger COW code
  temp <- apply(cbind(ccode1, ccode2), 1, min) * 1000 + apply(cbind(ccode1, ccode2), 1, max)
  
}

## check for duplicates and code first occurence as true, not just subsequent ones
dup.bidirec <- function (x) {

  ## check for duplicates from beginning and end
  duplicated(x) | duplicated(x, fromLast = T)

}

## adjust century after lubridate parses 2 and 4 digit years
## from https://stackoverflow.com/a/33223165
adjust.century <- function (d, threshold = 1930) {
  
  y <- year(d) %% 100
  if (y > threshold %% 100) year(d) <- 1900 + y
  d
  
}

## set seed for replication ####
set.seed(7912305)



###################
## End of Script ##
###################