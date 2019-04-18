#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: peace agreement strength ##
## created: January 31, 2016         ##
## updated: June 20, 2018            ##
#######################################

##########################
## 03 data manipulation ##
##########################



## data coverage ####

## left censoring: peace agreements data begin in 1975
data_start <- 1975

## right censoring: TIES data end in 2005? has ongoing as recent as 2013
data_end <- 2005



## ACD operations ####

## drop cases outside date range, limit to intrastate conflicts, create ACD
## intervention dummy, add COW code, and drop unneeded variables
ACD <- ACD %>%
  filter(YEAR >= data_start, YEAR <= data_end, Type >= 3) %>%
  mutate(intervention_acd = ifelse(Type == 3, 0, 1),
         COWcode = countrycode(Location, 'country.name', 'cown', warn = T)) %>%
  select(ID, YEAR, Type, intervention_acd, Incomp, Location, Int, CumInt, COWcode)



## peace agreements operations ####

## drop agreements outside of data range, drop unneeded variables, create and
## cold war dummies, replace '/' with '-' in Duration
PA <- PA %>%
  filter(Year <= data_end) %>%
  select(PAID, CID, GWNO, Name, pa_name, Inc, Year, Region, pa_type, cease,
         Intarmy, DDR, Withd, pp, Intgov, Intciv, Elections, Interrim, Natalks,
         Shagov, Aut, Fed, Ind, Ref, Shaloc, Regdev, Cul, Demarcation, Locgov,
         Amn, pris, Recon, Return, Reaffirm, Outlin, PKO, Co_impl, pa_date,
         ended, Duration) %>%
  mutate(agmt = 1, cold_war = ifelse(Year >= 1989, 1, 0),
         Duration = gsub('/', '-', Duration))

## one termination date is june 31, which does not exist, recode to june 30
PA$Duration[which(PA$Duration == '1979-06-31')] <- '1979-06-30'

## combine multiple date formats in duration -- dates w/o days or months default to previous
PA$Duration <- as.Date(parse_date_time(PA$Duration, c('mdy', 'ymd', 'ym', 'y')))
PA$pa_date <- as.Date(parse_date_time(PA$pa_date, c('mdy', 'ymd', 'ym', 'y')))

## add COW code to PA data, drop anything after : or - to isolate target of peacekeeping
PA$COWcode <- countrycode(sub("-.*$", "", sub(":.*$", "", PA$Name)),
                          'country.name', 'cown',warn = T)

## rename start and end dates of agreements
names(PA)[names(PA) == 'pa_date'] <- 'agmt_start'
names(PA)[names(PA) == 'Duration'] <- 'agmt_end'

## check to see if any agreements end on censoring date
any(PA$agmt_end == as.Date('2013-12-31'))

## recode right-censored data to end date
PA$agmt_end[which(is.na(PA$agmt_end))] <- as.Date('2005-12-31')

## create duration variable for survival analysis
PA$duration <- abs((PA$agmt_start %--% PA$agmt_end) %/% months(1))
PA$duration[PA$duration == 0] <- 1



## TIES operations ####

## drop unneeded variables
TIES <- TIES[, c('targetstate','sender1', 'startyear', 'endyear', 'ongoingasofyear',
                 'institution', 'imposition', 'anticipatedtargetcosts')]

## create vector of unique COW codes for countries in ACD
cow_codes <- unique(ACD$COWcode)

## vector to hold results
TIES_keep <- numeric()

## check whether the target state of sanction i is in ACD
for (i in 1:nrow(TIES)) {

  TIES_keep[i] <- any(TIES$targetstate[i] == cow_codes)

}

## recode NAs to 0s
TIES_keep[is.na(TIES_keep)] <- 0

## multiply by sequence of rows in TIES to get index that drops countries not in ACD
TIES_keep <- TIES_keep * seq(1:nrow(TIES))

## drop TIES cases not in ACD
TIES <- TIES[TIES_keep,]

## recode ongoing sanctions as concluded one year after ongoing mention
for (i in 1:nrow(TIES)) {

  ## check to see that sanction i is listed as ongoing
  if (!is.na(TIES$ongoingasofyear[i])) {

    ## check to see if sanction i's ongoing date is less than most recent data
    if (TIES$ongoingasofyear[i] < max(TIES$ongoingasofyear, na.rm = T)) {

      ## code sanction i end year as one year after last mention
      TIES$endyear[i] <- TIES$ongoingasofyear[i] + 1

    } else {

      ## code sanction i end year as last recent mention
      TIES$endyear[i] <- TIES$ongoingasofyear[i]

    }

  }

}

## drop sanctions with no end or ongoing date
TIES <- TIES[which(!is.na(TIES$endyear)), ]

## drop ongoingasofyear variable
TIES[, 'ongoingasofyear'] <- NULL

## create object to code whether state i is subject to a sanction in year t
TIES_year <- data.frame()

## convert TIES data from sanction to sanction-year
for (i in 1:nrow(TIES)) {

  date_range <- TIES$endyear[i] - TIES$startyear[i] + 1

  date_seq <- seq(TIES$startyear[i], TIES$endyear[i])

  ## extract row i into a dataframe
  temp <- TIES[i, ]

  ## repeat row i for each year of the sanction
  temp <- temp[rep(seq_len(nrow(temp)), date_range), ]

  ##
  temp$year <- date_seq

  ##
  TIES_year <- rbind(TIES_year, temp)

}

## drop start and end year
TIES_year[, c('startyear', 'endyear')] <- list(NULL)

## code dummy for sanction present
TIES_year$sanction <- 1

## reorder columns for easier recoding
TIES_year <- TIES_year[, c('year', 'targetstate', 'sanction', 'institution', 'sender1',
                           'imposition', 'anticipatedtargetcosts')]

## sort by year and target state before collapsing and recoding to one observation per state-year
TIES_year <- TIES_year[order(TIES_year$year, TIES_year$targetstate), ]

## recode institution NAs to 0s
TIES_year[which(is.na(TIES_year$institution)), 'institution'] <- 0

## list of each year in data
sanction_years <- unique(TIES_year$year)

## create object to hold output
TIES_data <- data.frame()

## collapse to one observation per state-year, coding count of sanctions
for (i in 1:length(sanction_years)) {

  ## create temporary object for sanction targets in year i
  temp <- TIES_year[which(TIES_year$year == sanction_years[i]), ]

  ## check to see if  any states are targeted by more than one sanction
  if (any(duplicated(temp$targetstate))) {

    ## create object of states with more than one sanction
    temp_dup <- temp[which(dup.bidirec(temp$targetstate)), ]

    ## list of each duplicated target in year i
    sanction_targets_dup <- unique(temp_dup$targetstate)

    ## create object to hold data for duplicated sanctions in year i
    output_dup <- data.frame()

    ## collapse each duplicated target into one entry and code sanctions count
    for (j in 1:length(sanction_targets_dup)) {

      temp_target <- temp_dup[which(temp_dup$targetstate == sanction_targets_dup[j]), ]

      ## extract year, target, and sanction from first row of temporary object (same in all rows)
      temp_output <- temp_target[1 , c('year', 'targetstate', 'sender1', 'sanction')]

      ## code count of sanctions
      temp_output$sanc_count <- nrow(temp_target)

      ## check if any sanctions were imposed through an institution
      if (any(temp_target$institution == 1)) {

        temp_output$sanc_mul <- 1

      } else {

        temp_output$sanc_mul <- 0

      }

      ## check if any sanctions were imposed unilaterally
      if (any(temp_target$institution == 0)) {

        temp_output$sanc_uni <- 1

      } else {

        temp_output$sanc_uni <- 0

      }
      
      ## check if any sanctions were actually imposed
      if (any(temp_target$imposition == 1)) {
        
        temp_output$imposition <- 1
        
      } else {
        
        temp_output$imposition <- 0
        
      }
      
      output_dup <- rbind(output_dup, temp_output)

    }

    ## perform coding operations on non-duplicated targets
    temp_sing <- temp[which(!dup.bidirec(temp$targetstate)), ]

    ## create object to hold data for unduplicated sanctions in year i
    output_sing <- data.frame()

    ## collapse each duplicated target into one entry and code sanctions count
    for (j in 1:nrow(temp_sing)) {

      ## extract year, target, and sanction from temporary object for sanction j in year i
      temp_output <- temp_sing[j, c('year', 'targetstate', 'sender1', 'sanction')]

      ## code sanction count as one
      temp_output$sanc_count <- 1

      ## check if sanction j was imposed through an institution
      if (temp_sing$institution[j] == 1) {

        temp_output$sanc_mul <- 1

      } else {

        temp_output$sanc_mul <- 0

      }

      ## check if sanction j was imposed unilaterally
      if (temp_sing$institution[j] == 0) {

        temp_output$sanc_uni <- 1

      } else {

        temp_output$sanc_uni <- 0

      }
      
      ## check if sanction j was actually imposed
      if (temp_sing$imposition[j] == 1) {
        
        temp_output$imposition <- 1
        
      } else {
        
        temp_output$imposition <- 0
        
      }
      
      ## append sanction j to object for all sanctions in year i
      output_sing <- rbind(output_sing, temp_output)

    }

    ## combine output from duplicated and single sanctions
    output <- rbind(output_dup, output_sing)

    ## append sanctions in year i to main dataframe
    TIES_data <- rbind(TIES_data, output)

  } else {

    ## create object to hold data for sanctions in year i
    output <- data.frame()

    ## code variables for each target in year i
    for (j in 1:nrow(temp)) {

      ## extract year, target, and sanction from temporary object for sanction j in year i
      temp_output <- temp[j, c('year', 'targetstate', 'sender1', 'sanction')]

      ## code sanction count as one
      temp_output$sanc_count <- 1

      ## check if sanction j was imposed through an institution
      if (temp$institution[j] == 1) {

        temp_output$sanc_mul <- 1

      } else {

        temp_output$sanc_mul <- 0

      }

      ## check if sanction j was imposed unilaterally
      if (temp$institution[j] == 0) {

        temp_output$sanc_uni <- 1

      } else {

        temp_output$sanc_uni <- 0

      }
      
      ## check if sanction j was actually imposed
      if (temp$imposition[j] == 1) {
        
        temp_output$imposition <- 1
        
      } else {
        
        temp_output$imposition <- 0
        
      }
      
      ## append sanction j to object for all sanctions in year i
      output <- rbind(output, temp_output)

    }

    ## append all sanctions in year i to main dataframe
    TIES_data <- rbind(TIES_data, output)

  }

}

## re-sort by year and target state b/c states w/ multiple sanctions are clustered in each year
TIES_data <- TIES_data[order(TIES_data$year, TIES_data$targetstate), ]

## create dyadid variable to code whether sanctions are sent by allies using ATOP data
TIES_data$dyadid <- COW.dyadid(TIES_data$targetstate, TIES_data$sender1)

## remove temporary objects
rm(cow_codes, TIES_keep, TIES, TIES_year, output, output_dup, output_sing, temp, temp_dup, temp_sing, temp_output, temp_target, date_range, date_seq, sanction_years, sanction_targets_dup)



## civil war mediation operations ####

## add our coding of regional mediators onto mediation data
CWM <- cbind(CWM, CWM_reg)

## append countrycode to CWM data
CWM <- data.frame(COWcode = countrycode(CWM$country, origin = 'country.name',
                                        destination = 'cown'), CWM)

## drop all observations with no mediation
CWM <- CWM[-which(CWM$Med_yes.no == 0), ]

## drop question marks in start and end dates
CWM[, c('med.begins', 'med.ends')] <- lapply(CWM[, c('med.begins', 'med.ends')],
                                             function(x) gsub('\\?', '', x))

## drop periods in start and end dates
CWM[, c('med.begins', 'med.ends')] <- lapply(CWM[, c('med.begins', 'med.ends')],
                                             function(x) gsub('\\.', '', x))

## drop double slash representing unknown day and/or month
CWM[, c('med.begins', 'med.ends')] <- lapply(CWM[, c('med.begins', 'med.ends')],
                                             function(x) gsub('//', '/', x))

## drop observations with no start and end date
CWM <- CWM[-which(grepl('^$', CWM$med.begins) & grepl('^$', CWM$med.ends)), ]

## fix specific mistyped observations:
## simple typo; intent is clear
CWM$med.begins[which(CWM$med.begins == '3/2/1882')] <- '3/2/1982'
## simple typo; intent is clear
CWM$med.begins[which(CWM$med.begins == '10/1/993')] <- '10/1/1993'
## pick later and more complete date; more conservative coding
CWM$med.begins[which(CWM$med.begins == '7/98 8/1/98 ')] <- '8/1/1998'

## convert med start and end dates to date
CWM[, c('med.begins', 'med.ends')] <- lapply(CWM[, c('med.begins', 'med.ends')],
                                             function(x) parse_date_time(x, orders = c('mdy',
                                                                                      'my', 'y
                                                                                      ')))

## get average mediation duration
CWM_no_date_NAs <- (CWM[which(!is.na(CWM$med.begins) & !is.na(CWM$med.ends)),])
CWM_no_date_NAs$med.begins <- as.POSIXct(sapply(CWM_no_date_NAs$med.begins, adjust.century), origin = '1970-01-01')
CWM_no_date_NAs$med.ends <- as.POSIXct(sapply(CWM_no_date_NAs$med.ends, adjust.century), origin = '1970-01-01')
med_dur <- list(mean = seconds_to_period(mean(CWM_no_date_NAs$med.ends -
                                                CWM_no_date_NAs$med.begins))@day,
                median =  seconds_to_period(median(CWM_no_date_NAs$med.ends -
                                                   CWM_no_date_NAs$med.begins))@day,
                tail = seconds_to_period(head(sort(CWM_no_date_NAs$med.ends -
                                                     CWM_no_date_NAs$med.begins,
                                                   decreasing = T)))@day[1:2],
                n = nrow(CWM_no_date_NAs))
save(med_dur, file = 'Knitr Input/med_dur.RData')
rm(CWM_no_date_NAs)

## code missing start days median duration before end, and vice versa
CWM$med.begins[which(is.na(CWM$med.begins)) ]  <- CWM$med.ends[which(is.na(CWM$med.begins)) ] - days(med_dur$median)
CWM$med.ends[which(is.na(CWM$med.ends)) ]  <- CWM$med.begins[which(is.na(CWM$med.ends)) ] + days(med_dur$median)

## adjust dates miscoded to 21st century instead of 20th
CWM$med.begins <- as.Date(as.POSIXct(sapply(CWM$med.begins, adjust.century),
                                     origin = '1970-01-01'))
CWM$med.ends <- as.Date(as.POSIXct(sapply(CWM$med.ends, adjust.century),
                                   origin = '1970-01-01'))

## drop observations before data start or after data end
CWM <- CWM[-which(year(CWM$med.begins) < data_start | year(CWM$med.ends) > data_end), ]

## Regional organization variable
CWM$regional_org <- ifelse(is.na(CWM$regional_org) == T, 0, CWM$regional_org)



## RPC data operations ####

## remove unneeded variables and lag RPC
RPC <- RPC %>% select(cowcode, year, rpr_work) %>% mutate(year = year - 1)



## world bank trade data operations ####

## add COW codes, rename variables, convert to proportions, log, code fuel
## exporter dummy using >= 1/3 rule from Fearon & Laitin 2003
WB <- WB %>%
  mutate(COWcode = countrycode(iso2c, 'iso2c', 'cown', warn = T)) %>%
  select(year,
         tradepct = NE.TRD.GNFS.ZS,
         fuelex = TX.VAL.FUEL.ZS.UN,
         gdppc = NY.GDP.PCAP.KD,
         aidpct = DT.ODA.ODAT.GN.ZS,
         COWcode) %>%
  mutate(gdppc = log(gdppc),
         tradepct = log1p(tradepct / 100),
         aidpct = log1p(aidpct / 100),
         fuelex = ifelse(fuelex / 100 >= 1/3, 1, 0))



## polity iv data operations ####

## drop unneeded variables from polity
polity <- polity %>% select(ccode, year, democ, autoc, polity, polity2)



## intervention operations ####

## recode ongoing interventions to end on december 31, 2005 when data end and drop
## observations without start and end date
IMI <- IMI %>%
  mutate_at(vars(end), function(x) ifelse(x == 88888888, 20051231, x)) %>%
  mutate_at(vars(start:end), parse_date_time, orders = 'ymd') %>%
  filter(!(is.na(start) & is.na(end)), year(start) <= data_end, year(end) >= data_start) %>% 
  select(target:end)

## get average intervention duration
IMI_no_date_NAs <- (IMI[which(!is.na(IMI$start) & !is.na(IMI$end)),])
IMI_no_date_NAs$start <- as.POSIXct(sapply(IMI_no_date_NAs$start, adjust.century), origin = '1970-01-01')
IMI_no_date_NAs$end <- as.POSIXct(sapply(IMI_no_date_NAs$end, adjust.century), origin = '1970-01-01')
int_dur <- list(mean = seconds_to_period(mean(IMI_no_date_NAs$end -
                                                IMI_no_date_NAs$start))@day,
                median =  seconds_to_period(median(IMI_no_date_NAs$end -
                                                     IMI_no_date_NAs$start))@day,
                tail = seconds_to_period(head(sort(IMI_no_date_NAs$end -
                                                     IMI_no_date_NAs$start,
                                                   decreasing = T)))@day[1:2],
                n = nrow(IMI_no_date_NAs))
save(int_dur, file = 'Knitr Input/int_dur.RData')
rm(IMI_no_date_NAs)

## code missing start days median duration before end, and vice versa
IMI$start[which(is.na(IMI$start)) ]  <- IMI$end[which(is.na(IMI$start)) ] - days(int_dur$median)
IMI$end[which(is.na(IMI$end)) ]  <- IMI$start[which(is.na(IMI$end)) ] + days(int_dur$median)



## data merging ####

## some agreements coded as ended but don't have end date, so end up with ended on 12/31/2013
PA$ended[PA$agmt_end == as.Date('2013-12-31')]

## drop COWcode from ACD b/c agmts and conflicts don't match 1-to-1, to avoid duplication
ACD[, 'COWcode'] <- NULL

## merge armed conflict data onto peace agreements data
PA <- merge(PA, ACD, by.x = c('Year', 'CID'), by.y = c('YEAR', 'ID'), all.x = T, all.y = F)

## merge TIES data onto main data
PA <- merge(PA, TIES_data, by.x = c('COWcode', 'Year'), by.y = c('targetstate', 'year'), all = F, all.x = T, all.y = F)
## merge RPC data onto main data
PA <- merge(PA, RPC, by.x = c('COWcode', 'Year'), by.y = c('cowcode', 'year'), all.x = T)

## merge WB data onto main data
PA <- merge(PA, WB, by.x = c('COWcode', 'Year'), by.y = c('COWcode', 'year'), all.x = T)

## merge polity data onto main data
PA <- merge(PA, polity, by.x = c('COWcode', 'Year'), by.y = c('ccode', 'year'), all.x = T)

## drop Incomp from PA since it's duplicated by Inc, which includes values for off years
PA$Incomp <- NULL

## recode sanctions NAs generated by merge w/ 0s b/c just means no sanction when signed
PA$sanction[is.na(PA$sanction)] <- 0
PA$sanc_count[is.na(PA$sanc_count)] <- 0
PA$sanc_mul[is.na(PA$sanc_mul)] <- 0
PA$sanc_uni[is.na(PA$sanc_uni)] <- 0
PA$imposition[is.na(PA$imposition)] <- 0

## reorder duration data by signing date
PA <- PA[order(PA$agmt_start),]

## this tests whether the government of a state was involved in an intrastate
## conflict mediation when an agreement was signed; NOT whether the mediation
## was for that specific conflict

## set placeholder mediation value of 0
PA$mediation <- 0
PA$mediation_reg <- 0

## loop over peace agreements
for (i in 1:nrow(PA)) {
  
  ## loop over civil war mediation episodes
  for (j in 1:nrow(CWM)) {
    
    ## check if mediation episode j occurred in agreement i's country
    if (PA$COWcode[i] == CWM$COWcode[j]) {
      
      ## check if agreement i is signed w/in mediation j's duration
      if (PA[i, 'agmt_start'] %between% CWM[j, c('med.begins', 'med.ends')]) {
        
        ## yes, code mediation as 1, and return to next level
        PA$mediation[i] <- 1
        PA$mediation_reg[i] <- CWM$regional_org[j]
        #break
        
      }
      
    } else {
      
      ## no mediation episodes j occur w/in country i, move to next country
      #break
      
    }
    
  }
  
}

## set placeholder intervention value of 0
PA$intervention_imi <- 0

## loop over peace agreements
for (i in 1:nrow(PA)) {
  
  ## loop over civil war mediation episodes
  for (j in 1:nrow(IMI)) {
    
    ## check if mediation episode j occurred in agreement i's country
    if (PA$COWcode[i] == IMI$target[j]) {
      
      ## check if agreement i is signed w/in mediation j's duration
      if (PA[i, 'agmt_start'] %between% IMI[j, c('start', 'end')]) {
        
        ## yes, code mediation as 1, and return to next level
        PA$intervention_imi[i] <- 1
        #break
        
      }
      
    } else {
      
      ## no interventions j occur w/in country i, move to next country
      #break
      
    }
    
  }
  
}

## three point categorial dummy for no sanctions, sanctions, and multilateral sanctions
PA$sanction <- as.factor(ifelse(PA$sanction == 0, 0,
                                ifelse(PA$sanction == 1 & PA$sanc_mul == 0, 1, 2)))

## three point categorial dummy for no mediation, mediation, and regional mediation
PA$mediation <- as.factor(ifelse(PA$mediation == 0, 0,
                                 ifelse(PA$mediation == 1 & PA$mediation_reg == 0, 1, 2)))

## create dummy for intrastate conflict; data use '-' for interstate, ':' for intra
PA$intrastate <- ifelse(grepl('-', PA$Name), 0, 1)

## subset peace agreements to just intrastate conflicts
PA <- PA[which(PA$Type >= 3), ]

## create additive index of provisions for baseline comparison
PA$add_ind <- apply(PA[, 11:38], 1, sum)

## export data for analysis ####
saveRDS(PA, 'PA.RDS')



###################
## End of Script ##
###################