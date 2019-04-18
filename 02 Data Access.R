#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: peace agreement strength ##
## created: January 31, 2016         ##
## updated: June 11, 2018            ##
#######################################

####################
## 02 data access ##
####################



## read in ACD data. note this is 2013 version b/c later versions not compatible w/ agreements data
ACD <- read.csv('Datasets/ucdp.prio.armed.conflict.v4.2013.csv')

## read in peace agreements data
PA <- rio::import('Datasets/124926_1peace-agreements-1975-2011.xls')

## read in mediation data
CWM <- read.csv('Datasets/CWM.csv')

## read in our coding of regional mediators
CWM_reg <- read.csv('Datasets/CWM regional.csv')

## read in (corrected) TIES data
TIES <- read.csv('Datasets/TIES.csv')

## read in RPC data
RPC <- rio::import('Datasets/RPC2015.xlsx')

## read in trade as a percentage of GDP data
WB <- WDI(country = 'all',
          indicator = c('NE.TRD.GNFS.ZS', 'TX.VAL.FUEL.ZS.UN', 'NY.GDP.PCAP.KD',
                        'DT.ODA.ODAT.GN.ZS'), start = '1960', end = '2013')

## read in polity iv data
polity <- rio::import('Datasets/p4v2015.xls')

## read in intervention data
IMI <- rio::import('Datasets/MergedIMIData1947-2005.xls')



###################
## End of Script ##
###################