#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: peace agreement strength ##
## created: January 31, 2016         ##
## updated: October 27, 2018         ##
#######################################

##################
## 00 execution ##
##################



## print script to identify in log
print(paste('Peace Agreement Data Preparation Started', Sys.time()))

source('01 Setup.R')
source('02 Data Access.R')
source('03 Data Manipulation.R')

## print secession info for log
sessionInfo()

## print script to verify completion in log
print(paste('Peace Agreement Data Preparation Completed', Sys.time()))

## quit R session
quit(save = 'no')



###################
## End of Script ##
###################