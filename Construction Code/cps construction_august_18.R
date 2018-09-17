####################################################
### CPS Dataset Construction
### Ryan Larson, UMN
##################################################
library(ff)
library(dplyr)
library(tidyr)
library(stringr)
library(csp)

#setwd("C:/Users/DELL/Documents/FE 2017") #set to google drive local folder

#read in cps_basic data
cps <- as.data.frame(read.csv.ffdf(file="cps_basic.csv", header=T, VERBOSE = T, first.rows=10000, next.rows=50000,
                     sep=",", colClasses = NA)) 
class(cps)
str(cps)

cps <- cps %>% select(YEAR, MONTH, CPSID, STATEFIP, WTFINL, AGE, SEX, RACE, MARST, 
                      EMPSTAT, WNLOOK, EDUC) %>% filter(AGE>=16) 

#making ASEC supplement data
asec <- read.csv(file = "cps_asec.csv", header=T) %>% select(YEAR, CPSID, STATEFIP, ASECWT, INCSSI, INCDISAB, DISABWRK)
write.csv(asec, file="asec.csv")
rm(asec) #used later


#adding in state names
states <- read.csv(file="fips.csv", header=T, stringsAsFactors = F)
states <- states[1:75,] #removing error NA case at end of data
states$STATEFIP <- states$ï..STATEFIP
cps <- left_join(cps, states, by="STATEFIP")
rm(states)

#############################
### Demographic Recodes
############################

#prime-age binary
cps$prime <- ifelse(cps$AGE>=25 & cps$AGE<=54, "Prime", "Non-Prime")

#working age binary
cps$work.age <- ifelse(cps$AGE>=18 & cps$AGE<=65, "Working Age", "Non-Working Age")

#gender binary
cps$gender <- ifelse(cps$SEX==1, "Male", "Female")

#recoded race variable - black non-black
cps$race <- rep(NA, length(cps$RACE))
cps$race[cps$RACE==100] <- "White" #white
cps$race[cps$RACE==200] <- "Black" #black
cps$race[cps$RACE>200] <- "Other" #other
cps$race <- as.factor(cps$race)
table(cps$race, cps$RACE)

#education
cps$degree <- ifelse(cps$EDUC>=111, "Yes", "No")

#relationship status - cohab dropped
cps$marry <- ifelse(cps$MARST>2, "No", "Yes")
#cps$cohab <- ifelse(cps$PECOHAB>0 & cps$marry=="No", "Yes", "No") #only since 2007
#table(cps$marry, cps$cohab) 

#########################
###  Y1: Not employed share 

cps$employed.share <- rep(NA, length(cps$EMPSTAT))
cps$employed.share[cps$EMPSTAT==1|cps$EMPSTAT==10|cps$EMPSTAT==12] <- "Employed"
cps$employed.share[cps$EMPSTAT >= 20] <- "Not Employed"
cps$employed.share <- as.factor(cps$employed.share)
table(cps$EMPSTAT, cps$employed.share)

########################
### Y2: unemployment rate

#employment three category - employed (+AF), unemployed, not in labor force
cps$employed <- ifelse(cps$EMPSTAT==01|cps$EMPSTAT==10|cps$EMPSTAT==12, "Employed", 
                         ifelse(cps$EMPSTAT==20|cps$EMPSTAT==21|cps$EMPSTAT==22,"Unemployed",
                                ifelse(cps$EMPSTAT >=30, "Not in Labor Force", NA)))
table(cps$EMPSTAT, cps$employed)

#########################################
### Idleness (NILF & not in school)

cps$idle <- rep(NA, length(cps$EMPSTAT))
cps$idle[cps$EMPSTAT==1|cps$EMPSTAT==10|cps$EMPSTAT==12|cps$EMPSTAT==33|cps$EMPSTAT==20|cps$EMPSTAT==21|cps$EMPSTAT==22] <- "Not Idle"
cps$idle[cps$EMPSTAT >= 30 & cps$EMPSTAT != 33] <- "Idle"
cps$idle <- as.factor(cps$idle)
table(cps$EMPSTAT, cps$idle)





########################
### Population Share
cps$pop.share <- rep(NA, length(cps$AGE))
cps$pop.share[cps$AGE >= 16 & cps$AGE <=25] <- "p.16_25"
cps$pop.share[cps$AGE >= 26 & cps$AGE <=35] <- "p.26_35"
cps$pop.share[cps$AGE >= 36 & cps$AGE <=45] <- "p.36_45"
cps$pop.share[cps$AGE >= 46 & cps$AGE <=55] <- "p.46_55"
cps$pop.share[cps$AGE >= 56 & cps$AGE <=65] <- "p.56_65"
cps$pop.share[cps$AGE >= 66 ] <- "p.66_plus"
cps$pop.share <- as.factor(cps$pop.share)
#table(cps$pop.share, cps$AGE)

#############################################
#cps.emp - BASE DATASET
#cps.emp will be anchoring dataset for merges
cps.emp <- cps %>% select(YEAR, STATENAME, WTFINL) %>%
  group_by(YEAR, STATENAME) %>% summarize(population.16=sum(WTFINL, na.rm=T))


#population variable - share of 16+ popularion overall
cps.popshare <- cps  %>% select(YEAR, STATENAME, pop.share, WTFINL) %>% 
  group_by(YEAR, STATENAME, pop.share) %>% summarize(pop=sum(WTFINL, na.rm=T)) %>%
  spread(pop.share, pop) 

cps.emp <- left_join(cps.emp, cps.popshare, by=c("YEAR","STATENAME")) %>% 
  mutate(popshare.16.25 = 100*(p.16_25/population.16), popshare.26.35 = 100*(p.26_35/population.16), popshare.36.45 = 100*(p.36_45/population.16),
         popshare.46.55 = 100*(p.46_55/population.16), popshare.56.65 = 100*(p.56_65/population.16), popshare.66.plus = 100*(p.66_plus/population.16))

#to check proportions
cps.emp$total <- cps.emp$popshare.16.25+cps.emp$popshare.26.35+cps.emp$popshare.36.45+cps.emp$popshare.46.55+cps.emp$popshare.56.65+cps.emp$popshare.66.plus
cps.emp$total <- NULL

########################################
### Lagged Unconditional Unemployment Rates
#total in each employment three category by state-year and rate
cps.unemp <- cps %>% select(YEAR, STATENAME, employed, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>%
  spread(employed, overall) %>% mutate(t.unemp.rate = Unemployed/(Employed+Unemployed)*100) %>%
  select(YEAR, STATENAME, t.unemp.rate) %>% group_by(STATENAME) %>%
  mutate(t_1.unemp.rate = dplyr::lag(t.unemp.rate, n = 1, default = NA, order_by=YEAR), 
        t_2.unemp.rate = dplyr::lag(t.unemp.rate, n = 2, default = NA, order_by=YEAR),
        t_3.unemp.rate = dplyr::lag(t.unemp.rate, n = 3, default = NA, order_by=YEAR)) 


cps.emp <- left_join(cps.emp, cps.unemp, by = c("YEAR","STATENAME"))
  
#prelim checks
#mean(cps.emp$t.unemp.rate[cps.emp$YEAR == 2010])


####################################
#OVerall
#########################################
### P1: Prime-age men no-BA - not employed share
#########################################

#p1
cps.p1 <- cps %>% select(YEAR, STATENAME, degree, prime, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% spread(degree_age_gen, overall) %>%
  rename(p1.pop=No_Prime_Male) %>% select(YEAR, STATENAME,p1.pop)

cps.emp <- left_join(cps.emp, cps.p1, by = c("YEAR","STATENAME"))

#p1.female
cps.p1.f <- cps %>% select(YEAR, STATENAME, degree, prime, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% spread(degree_age_gen, overall) %>%
  rename(p1.pop.female=No_Prime_Female) %>% select(YEAR, STATENAME,p1.pop.female)

cps.emp <- left_join(cps.emp, cps.p1.f, by = c("YEAR","STATENAME"))


#p1.y1
cps.p1.y1 <- cps %>% select(YEAR, STATENAME, degree,employed.share, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed.share) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Not Employed') %>% 
  rename(p1.y1.raw = 'No_Prime_Male_Not Employed')

cps.emp <- left_join(cps.emp, cps.p1.y1, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y1.notemployed.rate = 100*(p1.y1.raw/p1.pop))

#p1.y1.female
cps.p1.y1.f <- cps %>% select(YEAR, STATENAME, degree,employed.share, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed.share) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Female_Not Employed') %>% 
  rename(p1.y1.raw.female = 'No_Prime_Female_Not Employed')

cps.emp <- left_join(cps.emp, cps.p1.y1.f, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y1.notemployed.rate.female = 100*(p1.y1.raw.female/p1.pop.female))


#p1.y2 
cps.p1.y2 <- cps %>% select(YEAR, STATENAME, degree, employed, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Employed', 'No_Prime_Male_Unemployed' ) %>% 
  mutate(p1.y2.unemployed.rate = 100*(No_Prime_Male_Unemployed/(No_Prime_Male_Employed+No_Prime_Male_Unemployed))) %>%
  rename(p1.y2.raw = No_Prime_Male_Unemployed) %>%
  select(YEAR, STATENAME, p1.y2.raw, p1.y2.unemployed.rate) 

cps.emp <- left_join(cps.emp, cps.p1.y2, by = c("YEAR","STATENAME"))

#p1.y2.female
cps.p1.y2.f <- cps %>% select(YEAR, STATENAME, degree, employed, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Female_Employed', 'No_Prime_Female_Unemployed' ) %>% 
  mutate(p1.y2.unemployed.rate.female = 100*(No_Prime_Female_Unemployed/(No_Prime_Female_Employed+No_Prime_Female_Unemployed))) %>%
  rename(p1.y2.raw.female = No_Prime_Female_Unemployed) %>%
  select(YEAR, STATENAME, p1.y2.raw.female, p1.y2.unemployed.rate.female) 

cps.emp <- left_join(cps.emp, cps.p1.y2.f, by = c("YEAR","STATENAME"))


#p1.y3
cps.p1.y3 <- cps %>% select(YEAR, STATENAME, degree,idle, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, idle) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Idle') %>% 
  rename(p1.y3.raw = 'No_Prime_Male_Idle')

cps.emp <- left_join(cps.emp, cps.p1.y3, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y3.idle.rate = 100*(p1.y3.raw/p1.pop))

#p1.y3.female
cps.p1.y3.f <- cps %>% select(YEAR, STATENAME, degree,idle, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, idle) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Female_Idle') %>% 
  rename(p1.y3.raw.female = 'No_Prime_Female_Idle')

cps.emp <- left_join(cps.emp, cps.p1.y3.f, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y3.idle.rate.female = 100*(p1.y3.raw.female/p1.pop.female))









#marriage 
cps.p1.marry <- cps %>% select(YEAR, STATENAME, degree,marry, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_marry, degree_age_gen, marry) %>%
  spread(degree_age_gen_marry, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.marry.raw = No_Prime_Male_Yes) 

cps.emp <- left_join(cps.emp, cps.p1.marry, by = c("YEAR","STATENAME")) %>%
  mutate(p1.marriage.rate = 100*(p1.marry.raw/p1.pop))

#marry.female
cps.p1.marry.f <- cps %>% select(YEAR, STATENAME, degree,marry, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_marry, degree_age_gen, marry) %>%
  spread(degree_age_gen_marry, overall) %>% select(YEAR, STATENAME, No_Prime_Female_Yes) %>% rename(p1.marry.raw.female = No_Prime_Female_Yes) 

cps.emp <- left_join(cps.emp, cps.p1.marry.f, by = c("YEAR","STATENAME")) %>%
  mutate(p1.marriage.rate.female = 100*(p1.marry.raw.female/p1.pop.female))


#cps.p1.cohab <- cps %>% select(YEAR, STATENAME, degree,cohab, prime, gender, WTSUPP) %>% 
 # group_by(YEAR, STATENAME, cohab, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  #unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_cohab, degree_age_gen, cohab) %>%
  #spread(degree_age_gen_cohab, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.cohab.raw = No_Prime_Male_Yes)

#cohab rate - prime age males without BA
#cps.emp <- left_join(cps.emp, cps.p1.cohab, by = c("YEAR","STATENAME")) %>%
  #mutate(p1.cohab.rate = 100*(p1.cohab.raw/p1.pop))


#########################
###  P2: Working-age male no-BA 
#########################

##### P2 ALL Addition ###########################################

cps.p2all <- cps %>% select(YEAR, STATENAME, work.age,WTFINL) %>% 
  group_by(YEAR, STATENAME, work.age) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  spread(work.age, overall) %>%
  rename(p2all.pop='Working Age') %>% select(YEAR, STATENAME, p2all.pop)

cps.emp <- left_join(cps.emp, cps.p2all, by = c("YEAR","STATENAME"))


#p2.y1.all
cps.p2.y1.all <- cps %>% select(YEAR, STATENAME, employed.share, work.age, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, work.age) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, work.age, employed.share) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Working Age_Not Employed') %>% 
  rename(p2.y1.all.raw = 'Working Age_Not Employed')

cps.emp <- left_join(cps.emp, cps.p2.y1.all, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y1.notemployed.rate.all = 100*(p2.y1.all.raw/p2all.pop))

#p2.marry.all
cps.p2.marry.all <- cps %>% select(YEAR, STATENAME, marry, work.age, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry,  work.age) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(marry_age, marry, work.age) %>%
  spread(marry_age, overall) %>% select(YEAR, STATENAME, 'Yes_Working Age') %>% rename(p2.marry.raw.all = 'Yes_Working Age') 

cps.emp <- left_join(cps.emp, cps.p2.marry.all, by = c("YEAR","STATENAME")) %>%
  mutate(p2.marriage.rate.all = 100*(p2.marry.raw.all/p2all.pop))

#################################################################

#p2
cps.p2 <- cps %>% select(YEAR, STATENAME, degree, work.age, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% spread(degree_age_gen, overall) %>%
  rename(p2.pop='No_Working Age_Male') %>% select(YEAR, STATENAME,p2.pop)

cps.emp <- left_join(cps.emp, cps.p2, by = c("YEAR","STATENAME")) 

#p2.female
cps.p2.f <- cps %>% select(YEAR, STATENAME, degree, work.age, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% spread(degree_age_gen, overall) %>%
  rename(p2.pop.female='No_Working Age_Female') %>% select(YEAR, STATENAME, p2.pop.female)

cps.emp <- left_join(cps.emp, cps.p2.f, by = c("YEAR","STATENAME"))

#p2.y1
cps.p2.y1 <- cps %>% select(YEAR, STATENAME, degree,employed.share, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed.share) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Not Employed') %>% 
  rename(p2.y1.raw = 'No_Working Age_Male_Not Employed')

cps.emp <- left_join(cps.emp, cps.p2.y1, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y1.notemployed.rate = 100*(p2.y1.raw/p2.pop))

#p2.y2 
cps.p2.y2 <- cps %>% select(YEAR, STATENAME, degree, employed, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Employed', 'No_Working Age_Male_Unemployed') %>%
  rename(p2.y2.raw = 'No_Working Age_Male_Unemployed', p2.y2.raw.e = 'No_Working Age_Male_Employed') %>%
  mutate(p2.y2.unemployed.rate = 100*(p2.y2.raw/(p2.y2.raw+p2.y2.raw.e))) %>%
  select(YEAR, STATENAME, p2.y2.raw, p2.y2.unemployed.rate) 

cps.emp <- left_join(cps.emp, cps.p2.y2, by = c("YEAR","STATENAME"))

#p2.y2.female
cps.p2.y2.f <- cps %>% select(YEAR, STATENAME, degree, employed, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Female_Employed', 'No_Working Age_Female_Unemployed') %>%
  rename(p2.y2.raw.female = 'No_Working Age_Female_Unemployed', p2.y2.raw.e.female = 'No_Working Age_Female_Employed') %>%
  mutate(p2.y2.unemployed.rate.female = 100*(p2.y2.raw.female/(p2.y2.raw.female+p2.y2.raw.e.female))) %>%
  select(YEAR, STATENAME, p2.y2.raw.female, p2.y2.unemployed.rate.female) 

cps.emp <- left_join(cps.emp, cps.p2.y2.f, by = c("YEAR","STATENAME"))


#p2.y3
cps.p2.y3 <- cps %>% select(YEAR, STATENAME, degree,idle, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, idle) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Idle') %>% 
  rename(p2.y3.raw = 'No_Working Age_Male_Idle')

cps.emp <- left_join(cps.emp, cps.p2.y3, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y3.idle.rate = 100*(p2.y3.raw/p2.pop))

#p2.y3.female
cps.p2.y3.f <- cps %>% select(YEAR, STATENAME, degree,idle, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, idle) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Female_Idle') %>% 
  rename(p2.y3.raw.female = 'No_Working Age_Female_Idle')

cps.emp <- left_join(cps.emp, cps.p2.y3.f, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y3.idle.rate.female = 100*(p2.y3.raw.female/p2.pop.female))



#marriage and cohabitation rates
cps.p2.marry <- cps %>% select(YEAR, STATENAME, degree,marry, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_marry, degree_age_gen, marry) %>%
  spread(degree_age_gen_marry, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.marry.raw = 'No_Working Age_Male_Yes') 

cps.emp <- left_join(cps.emp, cps.p2.marry, by = c("YEAR","STATENAME")) %>%
  mutate(p2.marriage.rate = 100*(p2.marry.raw/p2.pop))

#marry.female

#cps.p2.cohab <- cps %>% select(YEAR, STATENAME, degree,cohab, work.age, gender, WTSUPP) %>% 
 # group_by(YEAR, STATENAME, cohab, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  #unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_cohab, degree_age_gen, cohab) %>%
  #spread(degree_age_gen_cohab, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.cohab.raw = 'No_Working Age_Male_Yes')

#cps.emp <- left_join(cps.emp, cps.p2.cohab, by = c("YEAR","STATENAME")) %>%
 # mutate(p2.cohab.rate = 100*(p2.cohab.raw/p2.pop))


#########################
###  P3: prime-age all men
#########################

#p3
cps.p3 <- cps %>% select(YEAR, STATENAME, prime, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% spread(age_gen, overall) %>%
  rename(p3.pop=Prime_Male) %>% select(YEAR, STATENAME,p3.pop)

cps.emp <- left_join(cps.emp, cps.p3, by = c("YEAR","STATENAME"))

#p3.female
cps.p3.f <- cps %>% select(YEAR, STATENAME, prime, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% spread(age_gen, overall) %>%
  rename(p3.pop.female=Prime_Female) %>% select(YEAR, STATENAME,p3.pop.female)

cps.emp <- left_join(cps.emp, cps.p3.f, by = c("YEAR","STATENAME"))


#p3.y1
cps.p3.y1 <- cps %>% select(YEAR, STATENAME, employed.share, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, employed.share) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Not Employed') %>% 
  rename(p3.y1.raw = 'Prime_Male_Not Employed')

cps.emp <- left_join(cps.emp, cps.p3.y1, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y1.notemployed.rate = 100*(p3.y1.raw/p3.pop))

#p3.y1.female
cps.p3.y1 <- cps %>% select(YEAR, STATENAME, employed.share, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, employed.share) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Female_Not Employed') %>% 
  rename(p3.y1.raw.female = 'Prime_Female_Not Employed')

cps.emp <- left_join(cps.emp, cps.p3.y1, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y1.notemployed.rate.female = 100*(p3.y1.raw.female/p3.pop.female))

#p3.y2 
cps.p3.y2 <- cps %>% select(YEAR, STATENAME, employed, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, employed) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Employed', 'Prime_Male_Unemployed') %>%
  rename(p3.y2.raw = 'Prime_Male_Unemployed', p3.y2.raw.e = 'Prime_Male_Employed') %>%
  mutate(p3.y2.unemployed.rate = 100*(p3.y2.raw/(p3.y2.raw+p3.y2.raw.e))) %>%
  select(YEAR, STATENAME, p3.y2.raw, p3.y2.unemployed.rate) 


cps.emp <- left_join(cps.emp, cps.p3.y2, by = c("YEAR","STATENAME"))

#p3.y2.female
cps.p3.y2 <- cps %>% select(YEAR, STATENAME, employed, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, employed) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Female_Employed', 'Prime_Female_Unemployed') %>%
  rename(p3.y2.raw.female = 'Prime_Female_Unemployed', p3.y2.raw.e.female = 'Prime_Female_Employed') %>%
  mutate(p3.y2.unemployed.rate.female = 100*(p3.y2.raw.female/(p3.y2.raw.female+p3.y2.raw.e.female))) %>%
  select(YEAR, STATENAME, p3.y2.raw.female, p3.y2.unemployed.rate.female) 

cps.emp <- left_join(cps.emp, cps.p3.y2, by = c("YEAR","STATENAME"))


#p3.y3
cps.p3.y3 <- cps %>% select(YEAR, STATENAME, idle, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, idle) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Idle') %>% 
  rename(p3.y3.raw = 'Prime_Male_Idle')

cps.emp <- left_join(cps.emp, cps.p3.y3, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y3.idle.rate = 100*(p3.y3.raw/p3.pop))

#p3.y3.female
cps.p3.y3 <- cps %>% select(YEAR, STATENAME, idle, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, idle) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Female_Idle') %>% 
  rename(p3.y3.raw.female = 'Prime_Female_Idle')

cps.emp <- left_join(cps.emp, cps.p3.y3, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y3.idle.rate.female = 100*(p3.y3.raw.female/p3.pop.female))




#marriage and cohabitation rates
cps.p3.marry <- cps %>% select(YEAR, STATENAME, marry, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_marry, age_gen, marry) %>%
  spread(age_gen_marry, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.marry.raw = Prime_Male_Yes) 

cps.emp <- left_join(cps.emp, cps.p3.marry, by = c("YEAR","STATENAME")) %>%
  mutate(p3.marriage.rate = 100*(p3.marry.raw/p3.pop))

#marriage.female
cps.p3.marry.f <- cps %>% select(YEAR, STATENAME, marry, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_marry, age_gen, marry) %>%
  spread(age_gen_marry, overall) %>% select(YEAR, STATENAME, Prime_Female_Yes) %>% rename(p3.marry.raw.female = Prime_Female_Yes) 

cps.emp <- left_join(cps.emp, cps.p3.marry.f, by = c("YEAR","STATENAME")) %>%
  mutate(p3.marriage.rate.female = 100*(p3.marry.raw.female/p3.pop.female))


#cps.p3.cohab <- cps %>% select(YEAR, STATENAME, cohab, prime, gender, WTSUPP) %>% 
  #group_by(YEAR, STATENAME, cohab, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  #unite(age_gen, prime, gender) %>% unite(age_gen_cohab, age_gen, cohab) %>%
  #spread(age_gen_cohab, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.cohab.raw = Prime_Male_Yes)

#cohab rate - prime age males without BA
#cps.emp <- left_join(cps.emp, cps.p3.cohab, by = c("YEAR","STATENAME")) %>%
  #mutate(p3.cohab.rate = 100*(p3.cohab.raw/p3.pop))



#########################
### p4: Prime-age all  
#########################

#p4
cps.p4 <- cps %>% select(YEAR, STATENAME, prime,WTFINL) %>% 
  group_by(YEAR, STATENAME, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  spread(prime, overall) %>%
  rename(p4.pop=Prime) %>% select(YEAR, STATENAME,p4.pop)

cps.emp <- left_join(cps.emp, cps.p4, by = c("YEAR","STATENAME"))


#p4.y1
cps.p4.y1 <- cps %>% select(YEAR, STATENAME, employed.share, prime, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, prime, employed.share) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Not Employed') %>% 
  rename(p4.y1.raw = 'Prime_Not Employed')

cps.emp <- left_join(cps.emp, cps.p4.y1, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y1.notemployed.rate = 100*(p4.y1.raw/p4.pop))

#p4.y2 
cps.p4.y2 <- cps %>% select(YEAR, STATENAME, employed, prime, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, prime, employed) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Employed', 'Prime_Unemployed') %>%
  rename(p4.y2.raw = 'Prime_Unemployed', p4.y2.raw.e = 'Prime_Employed') %>%
  mutate(p4.y2.unemployed.rate = 100*(p4.y2.raw/(p4.y2.raw+p4.y2.raw.e))) %>%
  select(YEAR, STATENAME, p4.y2.raw, p4.y2.unemployed.rate) 

cps.emp <- left_join(cps.emp, cps.p4.y2, by = c("YEAR","STATENAME"))



#p4.y3
cps.p4.y3 <- cps %>% select(YEAR, STATENAME, idle, prime, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, prime, idle) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Idle') %>% 
  rename(p4.y3.raw = 'Prime_Idle')

cps.emp <- left_join(cps.emp, cps.p4.y3, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y3.idle.rate = 100*(p4.y3.raw/p4.pop))




#marriage and cohabitation rates
cps.p4.marry <- cps %>% select(YEAR, STATENAME, marry, prime, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_marry, prime, marry) %>%
  spread(age_marry, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.marry.raw = Prime_Yes) 

cps.emp <- left_join(cps.emp, cps.p4.marry, by = c("YEAR","STATENAME")) %>%
  mutate(p4.marriage.rate = 100*(p4.marry.raw/p4.pop))

#cps.p4.cohab <- cps %>% select(YEAR, STATENAME, cohab, prime, WTSUPP) %>% 
 # group_by(YEAR, STATENAME, cohab, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  #unite(age_cohab, prime, cohab) %>%
  #spread(age_cohab, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.cohab.raw = Prime_Yes)

#cohab rate - prime age males without BA
#cps.emp <- left_join(cps.emp, cps.p4.cohab, by = c("YEAR","STATENAME")) %>%
  #mutate(p4.cohab.rate = 100*(p4.cohab.raw/p4.pop))

write.csv(cps.emp, file="FE_overall.csv")


####################################
#White
#########################################
### P1: Prime-age men no-BA - not employed share
#########################################

cps.w <- cps %>% filter(race=="White")

#p1
cps.p1.w <- cps.w %>% select(YEAR, STATENAME, degree, prime, gender,WTSUPP) %>% 
  group_by(YEAR, STATENAME, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% spread(degree_age_gen, overall) %>%
  rename(p1.pop.w=No_Prime_Male) %>% select(YEAR, STATENAME,p1.pop.w)

cps.emp <- left_join(cps.emp, cps.p1.w, by = c("YEAR","STATENAME"))


#p1.y1
cps.p1.y1.w <- cps.w %>% select(YEAR, STATENAME, degree,employed.share, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed.share, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed.share) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Not Employed') %>% 
  rename(p1.y1.raw.w = 'No_Prime_Male_Not Employed')

cps.emp <- left_join(cps.emp, cps.p1.y1.w, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y1.notemployed.rate.w = 100*(p1.y1.raw.w/p1.pop.w))

#p1.y2 
cps.p1.y2.w <- cps.w %>% select(YEAR, STATENAME, degree, employed, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Employed', 'No_Prime_Male_Unemployed' ) %>% 
  mutate(p1.y2.unemployed.rate.w = 100*(No_Prime_Male_Unemployed/(No_Prime_Male_Employed+No_Prime_Male_Unemployed))) %>%
  rename(p1.y2.raw.w = No_Prime_Male_Unemployed) %>%
  select(YEAR, STATENAME, p1.y2.raw.w, p1.y2.unemployed.rate.w) #4 missings are DC, will be filtered below

# 1995 Nebraska
sum(cps[cps.w$YEAR==1995 & cps.w$STATENAME=="Nebraska" & cps.w$employed=="Unemployed" & cps.w$gender=="Male" & cps.w$degree=="No" & cps.w$prime=="Prime",]$WTSUPP, na.rm=T)
cps.p1.y2.w[cps.p1.y2.w$YEAR==1995 & cps.p1.y2.w$STATENAME=="Nebraska",]$p1.y2.raw.w <- 0
cps.p1.y2.w[cps.p1.y2.w$YEAR==1995 & cps.p1.y2.w$STATENAME=="Nebraska",]$p1.y2.unemployed.rate.w <- 0

cps.emp <- left_join(cps.emp, cps.p1.y2.w, by = c("YEAR","STATENAME"))

#p1.y3
cps.p1.y3.w <- cps.w %>% select(YEAR, STATENAME, degree,idle, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, idle, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, idle) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Idle') %>% 
  rename(p1.y3.raw.w = 'No_Prime_Male_Idle') #3 missings are DC, will be dropped in DC filter below

cps.emp <- left_join(cps.emp, cps.p1.y3.w, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y3.idle.rate.w = 100*(p1.y3.raw.w/p1.pop.w))

#p1.y4
cps.p1.y4.w <- cps.w %>% select(YEAR, STATENAME, degree,cfw, prime, gender, DWWT) %>% 
  group_by(YEAR, STATENAME, cfw, degree, prime, gender) %>% summarize(overall=sum(DWWT, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, cfw) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_cfw') %>% 
  rename(p1.y4.raw.w = 'No_Prime_Male_cfw')

cps.p1.y4.w[is.na(cps.p1.y4.w$p1.y4.raw.w) & cps.p1.y4.w$YEAR %in% c(1996,1998,2000,2002,2004,2006,2008,2010),]$p1.y4.raw.w <- 0

cps.emp <- left_join(cps.emp, cps.p1.y4.w, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y4.cfw.rate.w = 100*(p1.y4.raw.w/p1.pop.w))

#p1 matching CPS covariates

#diability  
cps.p1.dis.w <- cps.w %>% select(YEAR, STATENAME, degree,disabin, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, disabin, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_dis, degree_age_gen, disabin) %>%
  spread(degree_age_gen_dis, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.disab.raw.w = No_Prime_Male_Yes)

#disability rate 
cps.emp <- left_join(cps.emp, cps.p1.dis.w, by = c("YEAR","STATENAME")) %>%
  mutate(p1.disab.rate.w = 100*(p1.disab.raw.w/p1.pop.w))


#marriage and cohabitation rates
cps.p1.marry.w <- cps.w %>% select(YEAR, STATENAME, degree,marry, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, marry, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_marry, degree_age_gen, marry) %>%
  spread(degree_age_gen_marry, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.marry.raw.w = No_Prime_Male_Yes) 

cps.emp <- left_join(cps.emp, cps.p1.marry.w, by = c("YEAR","STATENAME")) %>%
  mutate(p1.marriage.rate.w = 100*(p1.marry.raw.w/p1.pop.w))

cps.p1.cohab.w <- cps.w %>% select(YEAR, STATENAME, degree,cohab, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, cohab, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_cohab, degree_age_gen, cohab) %>%
  spread(degree_age_gen_cohab, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.cohab.raw.w = No_Prime_Male_Yes)

#cohab rate - prime age males without BA
cps.emp <- left_join(cps.emp, cps.p1.cohab.w, by = c("YEAR","STATENAME")) %>%
  mutate(p1.cohab.rate.w = 100*(p1.cohab.raw.w/p1.pop.w))


#########################
###  P2: Working-age male no-BA 
#########################

#p2
cps.p2.w <- cps.w %>% select(YEAR, STATENAME, degree, work.age, gender,WTSUPP) %>% 
  group_by(YEAR, STATENAME, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% spread(degree_age_gen, overall) %>%
  rename(p2.pop.w='No_Working Age_Male') %>% select(YEAR, STATENAME,p2.pop.w)

cps.emp <- left_join(cps.emp, cps.p2.w, by = c("YEAR","STATENAME"))


#p2.y1
cps.p2.y1.w <- cps.w %>% select(YEAR, STATENAME, degree,employed.share, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed.share, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed.share) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Not Employed') %>% 
  rename(p2.y1.raw.w = 'No_Working Age_Male_Not Employed')

cps.emp <- left_join(cps.emp, cps.p2.y1.w, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y1.notemployed.rate.w = 100*(p2.y1.raw.w/p2.pop.w))

#p2.y2 
cps.p2.y2.w <- cps.w %>% select(YEAR, STATENAME, degree, employed, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Employed', 'No_Working Age_Male_Unemployed') %>%
  rename(p2.y2.raw.w = 'No_Working Age_Male_Unemployed', p2.y2.raw.e.w = 'No_Working Age_Male_Employed') %>%
  mutate(p2.y2.unemployed.rate.w = 100*(p2.y2.raw.w/(p2.y2.raw.w+p2.y2.raw.e.w))) %>%
  select(YEAR, STATENAME, p2.y2.raw.w, p2.y2.unemployed.rate.w) #2 missings are DC, will be filtered below

cps.emp <- left_join(cps.emp, cps.p2.y2.w, by = c("YEAR","STATENAME"))

#p2.y3
cps.p2.y3.w <- cps.w %>% select(YEAR, STATENAME, degree,idle, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, idle, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, idle) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Idle') %>% 
  rename(p2.y3.raw.w = 'No_Working Age_Male_Idle')

cps.emp <- left_join(cps.emp, cps.p2.y3.w, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y3.idle.rate.w = 100*(p2.y3.raw.w/p2.pop.w))

#p2.y4
cps.p2.y4.w <- cps.w %>% select(YEAR, STATENAME, degree,cfw, work.age, gender, DWWT) %>% 
  group_by(YEAR, STATENAME, cfw, degree, work.age, gender) %>% summarize(overall=sum(DWWT, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, cfw) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_cfw') %>% 
  rename(p2.y4.raw.w = 'No_Working Age_Male_cfw')

cps.p2.y4.w[is.na(cps.p2.y4.w$p2.y4.raw.w) & cps.p2.y4.w$YEAR %in% c(1996,1998,2000,2002,2004,2006,2008,2010),]$p2.y4.raw.w <- 0


cps.emp <- left_join(cps.emp, cps.p2.y4.w, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y4.cfw.rate.w = 100*(p2.y4.raw.w/p2.pop.w))

#p2 matching CPS covariates

#diability
cps.p2.dis.w <- cps.w %>% select(YEAR, STATENAME, degree,disabin, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, disabin, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_dis, degree_age_gen, disabin) %>%
  spread(degree_age_gen_dis, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.disab.raw.w = 'No_Working Age_Male_Yes')

#disability rate - prime age males without BA
cps.emp <- left_join(cps.emp, cps.p2.dis.w, by = c("YEAR","STATENAME")) %>%
  mutate(p2.disab.rate.w = 100*(p2.disab.raw.w/p2.pop.w))


#marriage and cohabitation rates
cps.p2.marry.w <- cps.w %>% select(YEAR, STATENAME, degree,marry, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, marry, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_marry, degree_age_gen, marry) %>%
  spread(degree_age_gen_marry, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.marry.raw.w = 'No_Working Age_Male_Yes') 

cps.emp <- left_join(cps.emp, cps.p2.marry.w, by = c("YEAR","STATENAME")) %>%
  mutate(p2.marriage.rate.w = 100*(p2.marry.raw.w/p2.pop.w))

cps.p2.cohab.w <- cps.w %>% select(YEAR, STATENAME, degree,cohab, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, cohab, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_cohab, degree_age_gen, cohab) %>%
  spread(degree_age_gen_cohab, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.cohab.raw.w = 'No_Working Age_Male_Yes')

cps.emp <- left_join(cps.emp, cps.p2.cohab.w, by = c("YEAR","STATENAME")) %>%
  mutate(p2.cohab.rate.w = 100*(p2.cohab.raw.w/p2.pop.w))


#########################
###  P3: prime-age all men
#########################

#p3
cps.p3.w <- cps.w %>% select(YEAR, STATENAME, prime, gender,WTSUPP) %>% 
  group_by(YEAR, STATENAME, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% spread(age_gen, overall) %>%
  rename(p3.pop.w=Prime_Male) %>% select(YEAR, STATENAME,p3.pop.w)

cps.emp <- left_join(cps.emp, cps.p3.w, by = c("YEAR","STATENAME"))


#p3.y1
cps.p3.y1.w <- cps.w %>% select(YEAR, STATENAME, employed.share, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed.share, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, employed.share) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Not Employed') %>% 
  rename(p3.y1.raw.w = 'Prime_Male_Not Employed')

cps.emp <- left_join(cps.emp, cps.p3.y1.w, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y1.notemployed.rate.w = 100*(p3.y1.raw.w/p3.pop.w))

#p3.y2 
cps.p3.y2.w <- cps.w %>% select(YEAR, STATENAME, employed, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, employed) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Employed', 'Prime_Male_Unemployed') %>%
  rename(p3.y2.raw.w = 'Prime_Male_Unemployed', p3.y2.raw.e.w = 'Prime_Male_Employed') %>%
  mutate(p3.y2.unemployed.rate.w = 100*(p3.y2.raw.w/(p3.y2.raw.w+p3.y2.raw.e.w))) %>%
  select(YEAR, STATENAME, p3.y2.raw.w, p3.y2.unemployed.rate.w) 

# 1995 Nebraska
cps.p3.y2.w[cps.p3.y2.w$YEAR==1995 & cps.p3.y2.w$STATENAME=="Nebraska",]$p3.y2.raw.w <- 0
cps.p3.y2.w[cps.p3.y2.w$YEAR==1995 & cps.p3.y2.w$STATENAME=="Nebraska",]$p3.y2.unemployed.rate.w <- 0

cps.emp <- left_join(cps.emp, cps.p3.y2.w, by = c("YEAR","STATENAME"))

#p3.y3
cps.p3.y3.w <- cps.w %>% select(YEAR, STATENAME, idle, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, idle, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, idle) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Idle') %>% 
  rename(p3.y3.raw.w = 'Prime_Male_Idle')

cps.emp <- left_join(cps.emp, cps.p3.y3.w, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y3.idle.rate.w = 100*(p3.y3.raw.w/p3.pop.w))

#p3.y4
cps.p3.y4.w <- cps.w %>% select(YEAR, STATENAME, cfw, prime, gender, DWWT) %>% 
  group_by(YEAR, STATENAME, cfw, prime, gender) %>% summarize(overall=sum(DWWT, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, cfw) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_cfw') %>% 
  rename(p3.y4.raw.w = 'Prime_Male_cfw')

cps.p3.y4.w[is.na(cps.p3.y4.w$p3.y4.raw.w) & cps.p3.y4.w$YEAR %in% c(1996,1998,2000,2002,2004,2006,2008,2010),]$p3.y4.raw.w <- 0

cps.emp <- left_join(cps.emp, cps.p3.y4.w, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y4.cfw.rate.w = 100*(p3.y4.raw.w/p3.pop.w))

#p3 matching CPS covariates

#diability
cps.p3.dis.w <- cps.w %>% select(YEAR, STATENAME, disabin, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, disabin, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_dis, age_gen, disabin) %>%
  spread(age_gen_dis, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.disab.raw.w = Prime_Male_Yes)

#disability rate - prime age males without BA
cps.emp <- left_join(cps.emp, cps.p3.dis.w, by = c("YEAR","STATENAME")) %>%
  mutate(p3.disab.rate.w = 100*(p3.disab.raw.w/p3.pop.w))


#marriage and cohabitation rates
cps.p3.marry.w <- cps.w %>% select(YEAR, STATENAME, marry, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, marry, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_marry, age_gen, marry) %>%
  spread(age_gen_marry, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.marry.raw.w = Prime_Male_Yes) 

cps.emp <- left_join(cps.emp, cps.p3.marry.w, by = c("YEAR","STATENAME")) %>%
  mutate(p3.marriage.rate.w = 100*(p3.marry.raw.w/p3.pop.w))

cps.p3.cohab.w <- cps.w %>% select(YEAR, STATENAME, cohab, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, cohab, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_cohab, age_gen, cohab) %>%
  spread(age_gen_cohab, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.cohab.raw.w = Prime_Male_Yes)

#cohab rate - prime age males without BA
cps.emp <- left_join(cps.emp, cps.p3.cohab.w, by = c("YEAR","STATENAME")) %>%
  mutate(p3.cohab.rate.w = 100*(p3.cohab.raw.w/p3.pop.w))



#########################
### p4: Prime-age all  
#########################

#p4
cps.p4.w <- cps.w %>% select(YEAR, STATENAME, prime,WTSUPP) %>% 
  group_by(YEAR, STATENAME, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  spread(prime, overall) %>%
  rename(p4.pop.w=Prime) %>% select(YEAR, STATENAME,p4.pop.w)

cps.emp <- left_join(cps.emp, cps.p4.w, by = c("YEAR","STATENAME"))


#p4.y1
cps.p4.y1.w <- cps.w %>% select(YEAR, STATENAME, employed.share, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed.share, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_emp, prime, employed.share) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Not Employed') %>% 
  rename(p4.y1.raw.w = 'Prime_Not Employed')

cps.emp <- left_join(cps.emp, cps.p4.y1.w, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y1.notemployed.rate.w = 100*(p4.y1.raw.w/p4.pop.w))

#p4.y2 
cps.p4.y2.w <- cps.w %>% select(YEAR, STATENAME, employed, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_emp, prime, employed) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Employed', 'Prime_Unemployed') %>%
  rename(p4.y2.raw.w = 'Prime_Unemployed', p4.y2.raw.e.w = 'Prime_Employed') %>%
  mutate(p4.y2.unemployed.rate.w = 100*(p4.y2.raw.w/(p4.y2.raw.w+p4.y2.raw.e.w))) %>%
  select(YEAR, STATENAME, p4.y2.raw.w, p4.y2.unemployed.rate.w) 

cps.emp <- left_join(cps.emp, cps.p4.y2.w, by = c("YEAR","STATENAME"))

#p4.y3
cps.p4.y3.w <- cps.w %>% select(YEAR, STATENAME, idle, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, idle, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_emp, prime, idle) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Idle') %>% 
  rename(p4.y3.raw.w = 'Prime_Idle')

cps.emp <- left_join(cps.emp, cps.p4.y3.w, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y3.idle.rate.w = 100*(p4.y3.raw.w/p4.pop.w))

#p4.y4
cps.p4.y4.w <- cps.w %>% select(YEAR, STATENAME, cfw, prime, DWWT) %>% 
  group_by(YEAR, STATENAME, cfw, prime) %>% summarize(overall=sum(DWWT, na.rm=T)) %>% 
  unite(age_emp, prime, cfw) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_cfw') %>% 
  rename(p4.y4.raw.w = 'Prime_cfw')

cps.p4.y4.w[is.na(cps.p4.y4.w$p4.y4.raw.w) & cps.p4.y4.w$YEAR %in% c(1996,1998,2000,2002,2004,2006,2008,2010),]$p4.y4.raw.w <- 0


cps.emp <- left_join(cps.emp, cps.p4.y4.w, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y4.cfw.rate.w = 100*(p4.y4.raw.w/p4.pop.w))

#p4 matching CPS covariates

#diability
cps.p4.dis.w <- cps.w %>% select(YEAR, STATENAME, disabin, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, disabin, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_dis, prime, disabin) %>%
  spread(age_dis, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.disab.raw.w = Prime_Yes)

#disability rate - prime age males without BA
cps.emp <- left_join(cps.emp, cps.p4.dis.w, by = c("YEAR","STATENAME")) %>%
  mutate(p4.disab.rate.w = 100*(p4.disab.raw.w/p4.pop.w))


#marriage and cohabitation rates
cps.p4.marry.w <- cps.w %>% select(YEAR, STATENAME, marry, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, marry, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_marry, prime, marry) %>%
  spread(age_marry, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.marry.raw.w = Prime_Yes) 

cps.emp <- left_join(cps.emp, cps.p4.marry.w, by = c("YEAR","STATENAME")) %>%
  mutate(p4.marriage.rate.w = 100*(p4.marry.raw.w/p4.pop.w))

cps.p4.cohab.w <- cps.w %>% select(YEAR, STATENAME, cohab, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, cohab, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_cohab, prime, cohab) %>%
  spread(age_cohab, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.cohab.raw.w = Prime_Yes)

#cohab rate - prime age males without BA
cps.emp <- left_join(cps.emp, cps.p4.cohab.w, by = c("YEAR","STATENAME")) %>%
  mutate(p4.cohab.rate.w = 100*(p4.cohab.raw.w/p4.pop.w))

####################################
#Black
#########################################
### P1: Prime-age men no-BA - not employed share
#########################################
cps.b <- cps %>% filter(race=="Black")

#p1
cps.p1.b <- cps.b %>% select(YEAR, STATENAME, degree, prime, gender,WTSUPP) %>% 
  group_by(YEAR, STATENAME, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% spread(degree_age_gen, overall) %>%
  rename(p1.pop.b=No_Prime_Male) %>% select(YEAR, STATENAME,p1.pop.b) %>% mutate(p1.pop.b = ifelse(is.na(p1.pop.b), 0, p1.pop.b))

cps.emp <- left_join(cps.emp, cps.p1.b, by = c("YEAR","STATENAME"))
cps.emp$p1.pop.b[is.na(cps.emp$p1.pop.b)] <- 0 #fix ten cases that have 0 black prime no degree males

#p1.y1
cps.p1.y1.b <- cps.b %>% select(YEAR, STATENAME, degree,employed.share, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed.share, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed.share) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Not Employed') %>% 
  rename(p1.y1.raw.b = 'No_Prime_Male_Not Employed') %>% mutate(p1.y1.raw.b = ifelse(is.na(p1.y1.raw.b), 0, p1.y1.raw.b))

cps.emp <- left_join(cps.emp, cps.p1.y1.b, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y1.notemployed.rate.b = 100*(p1.y1.raw.b/p1.pop.b))

cps.emp$p1.y1.raw.b[is.na(cps.emp$p1.y1.raw.b)] <- 0 #fix ten cases that have 0 black prime no degree males
cps.emp$p1.y1.notemployed.rate.b[is.na(cps.emp$p1.y1.notemployed.rate.b)] <- 0 #fix ten cases that have 0 black prime no degree males

cps.emp <- cps.emp %>% mutate(p1.y1.notemployed.rate.b = ifelse(is.nan(p1.y1.notemployed.rate.b), 0, p1.y1.notemployed.rate.b)) #fix NaN



#p1.y2 
cps.p1.y2.b <- cps.b %>% select(YEAR, STATENAME, degree, employed, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Employed', 'No_Prime_Male_Unemployed' ) %>% 
  mutate(p1.y2.unemployed.rate.b = 100*(No_Prime_Male_Unemployed/(No_Prime_Male_Employed+No_Prime_Male_Unemployed))) %>%
  rename(p1.y2.raw.b = No_Prime_Male_Unemployed) %>%
  select(YEAR, STATENAME, p1.y2.raw.b, p1.y2.unemployed.rate.b) 

cps.p1.y2.b$p1.y2.raw.b[is.na(cps.p1.y2.b$p1.y2.raw.b)] <- 0 
cps.p1.y2.b$p1.y2.unemployed.rate.b[is.na(cps.p1.y2.b$p1.y2.unemployed.rate.b)] <- 0 

cps.emp <- left_join(cps.emp, cps.p1.y2.b, by = c("YEAR","STATENAME"))

cps.emp$p1.y2.raw.b[is.na(cps.emp$p1.y2.raw.b)] <- 0 #fix ten cases that have 0 black prime no degree males
cps.emp$p1.y2.unemployed.rate.b[is.na(cps.emp$p1.y2.unemployed.rate.b)] <- 0 #fix ten cases that have 0 black prime no degree males



#p1.y3
cps.p1.y3.b <- cps.b %>% select(YEAR, STATENAME, degree,idle, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, idle, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, idle) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Idle') %>% 
  rename(p1.y3.raw.b = 'No_Prime_Male_Idle') %>% mutate(p1.y3.raw.b = ifelse(is.na(p1.y3.raw.b), 0, p1.y3.raw.b))

cps.emp <- left_join(cps.emp, cps.p1.y3.b, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y3.idle.rate.b = 100*(p1.y3.raw.b/p1.pop.b)) %>% mutate(p1.y3.idle.rate.b = ifelse(is.na(p1.y3.idle.rate.b), 0, p1.y3.idle.rate.b)) %>%
  mutate(p1.y3.raw.b = ifelse(is.na(p1.y3.raw.b), 0, p1.y3.raw.b))

#p1.y4
cps.p1.y4.b <- cps.b %>% select(YEAR, STATENAME, degree,cfw, prime, gender, DWWT) %>% 
  group_by(YEAR, STATENAME, cfw, degree, prime, gender) %>% summarize(overall=sum(DWWT, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, cfw) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_cfw') %>% 
  rename(p1.y4.raw.b = 'No_Prime_Male_cfw')

cps.p1.y4.b[is.na(cps.p1.y4.b$p1.y4.raw.b) & cps.p1.y4.b$YEAR %in% c(1996,1998,2000,2002,2004,2006,2008,2010),]$p1.y4.raw.b <- 0

cps.emp <- left_join(cps.emp, cps.p1.y4.b, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y4.cfw.rate.b = 100*(p1.y4.raw.b/p1.pop.b)) %>% 
  mutate(p1.y4.cfw.rate.b = ifelse(is.nan(p1.y4.cfw.rate.b), 0, p1.y4.cfw.rate.b))

#p1 matching CPS covariates

#diability  
cps.p1.dis.b <- cps.b %>% select(YEAR, STATENAME, degree,disabin, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, disabin, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_dis, degree_age_gen, disabin) %>%
  spread(degree_age_gen_dis, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.disab.raw.b = No_Prime_Male_Yes) %>%
  mutate(p1.disab.raw.b = ifelse(is.na(p1.disab.raw.b), 0, p1.disab.raw.b))

#disability rate 
cps.emp <- left_join(cps.emp, cps.p1.dis.b, by = c("YEAR","STATENAME")) %>%
  mutate(p1.disab.rate.b = 100*(p1.disab.raw.b/p1.pop.b)) %>% 
  mutate(p1.disab.rate.b = ifelse(is.na(p1.disab.rate.b ), 0, p1.disab.rate.b )) %>%
  mutate(p1.disab.rate.b  = ifelse(is.nan(p1.disab.rate.b ), 0, p1.disab.rate.b )) %>% 
  mutate(p1.disab.raw.b = ifelse(is.na(p1.disab.raw.b), 0, p1.disab.raw.b))

#marriage and cohabitation rates
cps.p1.marry.b <- cps.b %>% select(YEAR, STATENAME, degree,marry, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, marry, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_marry, degree_age_gen, marry) %>%
  spread(degree_age_gen_marry, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.marry.raw.b = No_Prime_Male_Yes) %>%
  mutate(p1.marry.raw.b = ifelse(is.na(p1.marry.raw.b), 0, p1.marry.raw.b))

cps.emp <- left_join(cps.emp, cps.p1.marry.b, by = c("YEAR","STATENAME")) %>%
  mutate(p1.marriage.rate.b = 100*(p1.marry.raw.b/p1.pop.b)) %>%
  mutate(p1.marriage.rate.b = ifelse(is.na(p1.marriage.rate.b), 0, p1.marriage.rate.b )) %>%
  mutate(p1.marriage.rate.b = ifelse(is.nan(p1.marriage.rate.b ), 0, p1.marriage.rate.b )) %>% 
  mutate(p1.marry.raw.b = ifelse(is.na(p1.marry.raw.b), 0, p1.marry.raw.b))

cps.p1.cohab.b <- cps.b %>% select(YEAR, STATENAME, degree,cohab, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, cohab, degree, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_cohab, degree_age_gen, cohab) %>%
  spread(degree_age_gen_cohab, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.cohab.raw.b = No_Prime_Male_Yes)


#cohab rate - prime age males without BA
cps.emp <- left_join(cps.emp, cps.p1.cohab.b, by = c("YEAR","STATENAME")) %>%
  mutate(p1.cohab.rate.b = 100*(p1.cohab.raw.b/p1.pop.b)) %>%
  mutate(p1.cohab.raw.b = ifelse(YEAR >= 2007 & is.na(p1.cohab.raw.b), 0, p1.cohab.raw.b)) %>%
  mutate(p1.cohab.rate.b = ifelse(YEAR >= 2007 & is.na(p1.cohab.rate.b), 0, p1.cohab.rate.b))
#########################
###  P2: Working-age male no-BA 
#########################

#p2
cps.p2.b <- cps.b %>% select(YEAR, STATENAME, degree, work.age, gender,WTSUPP) %>% 
  group_by(YEAR, STATENAME, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% spread(degree_age_gen, overall) %>%
  rename(p2.pop.b='No_Working Age_Male') %>% select(YEAR, STATENAME,p2.pop.b) %>% mutate(p2.pop.b = ifelse(is.na(p2.pop.b), 0, p2.pop.b))

cps.emp <- left_join(cps.emp, cps.p2.b, by = c("YEAR","STATENAME"))
cps.emp$p2.pop.b[is.na(cps.emp$p2.pop.b)] <- 0 #fix ten cases that have 0 black prime no degree males


#p2.y1
cps.p2.y1.b <- cps.b %>% select(YEAR, STATENAME, degree,employed.share, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed.share, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed.share) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Not Employed') %>% 
  rename(p2.y1.raw.b = 'No_Working Age_Male_Not Employed') %>% mutate(p2.y1.raw.b = ifelse(is.na(p2.y1.raw.b), 0, p2.y1.raw.b))

cps.emp <- left_join(cps.emp, cps.p2.y1.b, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y1.notemployed.rate.b = 100*(p2.y1.raw.b/p2.pop.b))

cps.emp$p2.y1.raw.b[is.na(cps.emp$p2.y1.raw.b)] <- 0 #fix ten cases that have 0 black prime no degree males
cps.emp$p2.y1.notemployed.rate.b[is.na(cps.emp$p2.y1.notemployed.rate.b)] <- 0 #fix ten cases that have 0 black prime no degree males

cps.emp <- cps.emp %>% mutate(p2.y1.notemployed.rate.b = ifelse(is.nan(p2.y1.notemployed.rate.b), 0, p2.y1.notemployed.rate.b)) #fix NaN


#p2.y2 
cps.p2.y2.b <- cps.b %>% select(YEAR, STATENAME, degree, employed, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Employed', 'No_Working Age_Male_Unemployed') %>%
  rename(p2.y2.raw.b = 'No_Working Age_Male_Unemployed', p2.y2.raw.e.b = 'No_Working Age_Male_Employed') %>%
  mutate(p2.y2.unemployed.rate.b = 100*(p2.y2.raw.b/(p2.y2.raw.b+p2.y2.raw.e.b))) %>%
  select(YEAR, STATENAME, p2.y2.raw.b, p2.y2.unemployed.rate.b) #2 missings are DC, will be filtered below

cps.p2.y2.b$p2.y2.raw.b[is.na(cps.p2.y2.b$p2.y2.raw.b)] <- 0 
cps.p2.y2.b$p2.y2.unemployed.rate.b[is.na(cps.p2.y2.b$p2.y2.unemployed.rate.b)] <- 0 

cps.emp <- left_join(cps.emp, cps.p2.y2.b, by = c("YEAR","STATENAME"))

cps.emp$p2.y2.raw.b[is.na(cps.emp$p2.y2.raw.b)] <- 0 #fix ten cases that have 0 black prime no degree males
cps.emp$p2.y2.unemployed.rate.b[is.na(cps.emp$p2.y2.unemployed.rate.b)] <- 0 #fix ten cases that have 0 black prime no degree males

#p2.y3
cps.p2.y3.b <- cps.b %>% select(YEAR, STATENAME, degree,idle, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, idle, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, idle) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Idle') %>% 
  rename(p2.y3.raw.b = 'No_Working Age_Male_Idle')%>% mutate(p2.y3.raw.b = ifelse(is.na(p2.y3.raw.b), 0, p2.y3.raw.b))

cps.emp <- left_join(cps.emp, cps.p2.y3.b, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y3.idle.rate.b = 100*(p2.y3.raw.b/p2.pop.b)) %>% mutate(p2.y3.idle.rate.b = ifelse(is.na(p2.y3.idle.rate.b), 0, p2.y3.idle.rate.b)) %>%
  mutate(p2.y3.raw.b = ifelse(is.na(p2.y3.raw.b), 0, p2.y3.raw.b))

#p2.y4
cps.p2.y4.b <- cps.b %>% select(YEAR, STATENAME, degree,cfw, work.age, gender, DWWT) %>% 
  group_by(YEAR, STATENAME, cfw, degree, work.age, gender) %>% summarize(overall=sum(DWWT, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, cfw) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_cfw') %>% 
  rename(p2.y4.raw.b = 'No_Working Age_Male_cfw')

cps.p2.y4.b[is.na(cps.p2.y4.b$p2.y4.raw.b) & cps.p2.y4.b$YEAR %in% c(1996,1998,2000,2002,2004,2006,2008,2010),]$p2.y4.raw.b <- 0

cps.emp <- left_join(cps.emp, cps.p2.y4.b, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y4.cfw.rate.b = 100*(p2.y4.raw.b/p2.pop.b)) %>% 
  mutate(p2.y4.cfw.rate.b = ifelse(is.nan(p2.y4.cfw.rate.b), 0, p2.y4.cfw.rate.b))

#p2 matching CPS covariates

#diability
cps.p2.dis.b <- cps.b %>% select(YEAR, STATENAME, degree,disabin, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, disabin, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_dis, degree_age_gen, disabin) %>%
  spread(degree_age_gen_dis, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.disab.raw.b = 'No_Working Age_Male_Yes') %>%
  mutate(p2.disab.raw.b = ifelse(is.na(p2.disab.raw.b), 0, p2.disab.raw.b))

#disability rate 
cps.emp <- left_join(cps.emp, cps.p2.dis.b, by = c("YEAR","STATENAME")) %>%
  mutate(p2.disab.rate.b = 100*(p2.disab.raw.b/p2.pop.b)) %>% 
  mutate(p2.disab.rate.b = ifelse(is.na(p2.disab.rate.b ), 0, p2.disab.rate.b )) %>%
  mutate(p2.disab.rate.b  = ifelse(is.nan(p2.disab.rate.b ), 0, p2.disab.rate.b )) %>% 
  mutate(p2.disab.raw.b = ifelse(is.na(p2.disab.raw.b), 0, p2.disab.raw.b))

#marriage and cohabitation rates
cps.p2.marry.b <- cps.b %>% select(YEAR, STATENAME, degree,marry, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, marry, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_marry, degree_age_gen, marry) %>%
  spread(degree_age_gen_marry, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.marry.raw.b = 'No_Working Age_Male_Yes') %>%
  mutate(p2.marry.raw.b = ifelse(is.na(p2.marry.raw.b), 0, p2.marry.raw.b))

cps.emp <- left_join(cps.emp, cps.p2.marry.b, by = c("YEAR","STATENAME")) %>%
  mutate(p2.marriage.rate.b = 100*(p2.marry.raw.b/p2.pop.b)) %>%
  mutate(p2.marriage.rate.b = ifelse(is.na(p2.marriage.rate.b), 0, p2.marriage.rate.b )) %>%
  mutate(p2.marriage.rate.b = ifelse(is.nan(p2.marriage.rate.b ), 0, p2.marriage.rate.b )) %>% 
  mutate(p2.marry.raw.b = ifelse(is.na(p2.marry.raw.b), 0, p2.marry.raw.b))

cps.p2.cohab.b <- cps.b %>% select(YEAR, STATENAME, degree,cohab, work.age, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, cohab, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_cohab, degree_age_gen, cohab) %>%
  spread(degree_age_gen_cohab, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.cohab.raw.b = 'No_Working Age_Male_Yes')

#cohab rate - prime age males without BA
cps.emp <- left_join(cps.emp, cps.p2.cohab.b, by = c("YEAR","STATENAME")) %>%
  mutate(p2.cohab.rate.b = 100*(p2.cohab.raw.b/p2.pop.b)) %>%
  mutate(p2.cohab.raw.b = ifelse(YEAR >= 2007 & is.na(p2.cohab.raw.b), 0, p2.cohab.raw.b)) %>%
  mutate(p2.cohab.rate.b = ifelse(YEAR >= 2007 & is.na(p2.cohab.rate.b), 0, p2.cohab.rate.b))


#########################
###  P3: prime-age all men
#########################

#p3
cps.p3.b <- cps.b %>% select(YEAR, STATENAME, prime, gender,WTSUPP) %>% 
  group_by(YEAR, STATENAME, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% spread(age_gen, overall) %>%
  rename(p3.pop.b=Prime_Male) %>% select(YEAR, STATENAME,p3.pop.b)%>% mutate(p3.pop.b = ifelse(is.na(p3.pop.b), 0, p3.pop.b))

cps.emp <- left_join(cps.emp, cps.p3.b, by = c("YEAR","STATENAME"))
cps.emp$p3.pop.b[is.na(cps.emp$p3.pop.b)] <- 0 #fix ten cases that have 0 black prime no degree males

#p3.y1
cps.p3.y1.b <- cps.b %>% select(YEAR, STATENAME, employed.share, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed.share, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, employed.share) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Not Employed') %>% 
  rename(p3.y1.raw.b = 'Prime_Male_Not Employed')%>% mutate(p3.y1.raw.b = ifelse(is.na(p3.y1.raw.b), 0, p3.y1.raw.b))

cps.emp <- left_join(cps.emp, cps.p3.y1.b, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y1.notemployed.rate.b = 100*(p3.y1.raw.b/p3.pop.b))

cps.emp$p3.y1.raw.b[is.na(cps.emp$p3.y1.raw.b)] <- 0 #fix ten cases that have 0 black prime no degree males
cps.emp$p3.y1.notemployed.rate.b[is.na(cps.emp$p3.y1.notemployed.rate.b)] <- 0 #fix ten cases that have 0 black prime no degree males

cps.emp <- cps.emp %>% mutate(p3.y1.notemployed.rate.b = ifelse(is.nan(p3.y1.notemployed.rate.b), 0, p3.y1.notemployed.rate.b)) #fix NaN

#p3.y2 
cps.p3.y2.b <- cps.b %>% select(YEAR, STATENAME, employed, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, employed) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Employed', 'Prime_Male_Unemployed') %>%
  rename(p3.y2.raw.b = 'Prime_Male_Unemployed', p3.y2.raw.e.b = 'Prime_Male_Employed') %>%
  mutate(p3.y2.unemployed.rate.b = 100*(p3.y2.raw.b/(p3.y2.raw.b+p3.y2.raw.e.b))) %>%
  select(YEAR, STATENAME, p3.y2.raw.b, p3.y2.unemployed.rate.b) 

cps.p3.y2.b$p3.y2.raw.b[is.na(cps.p3.y2.b$p3.y2.raw.b)] <- 0 
cps.p3.y2.b$p3.y2.unemployed.rate.b[is.na(cps.p3.y2.b$p3.y2.unemployed.rate.b)] <- 0 

cps.emp <- left_join(cps.emp, cps.p3.y2.b, by = c("YEAR","STATENAME"))

cps.emp$p3.y2.raw.b[is.na(cps.emp$p3.y2.raw.b)] <- 0 #fix ten cases that have 0 black prime no degree males
cps.emp$p3.y2.unemployed.rate.b[is.na(cps.emp$p3.y2.unemployed.rate.b)] <- 0 #fix ten cases that have 0 black prime no degree males

#p3.y3
cps.p3.y3.b <- cps.b %>% select(YEAR, STATENAME, idle, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, idle, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, idle) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Idle') %>% 
  rename(p3.y3.raw.b = 'Prime_Male_Idle')%>% mutate(p3.y3.raw.b = ifelse(is.na(p3.y3.raw.b), 0, p3.y3.raw.b))

cps.emp <- left_join(cps.emp, cps.p3.y3.b, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y3.idle.rate.b = 100*(p3.y3.raw.b/p3.pop.b)) %>% mutate(p3.y3.idle.rate.b = ifelse(is.na(p3.y3.idle.rate.b), 0, p3.y3.idle.rate.b)) %>%
  mutate(p3.y3.raw.b = ifelse(is.na(p3.y3.raw.b), 0, p3.y3.raw.b))

#p3.y4
cps.p3.y4.b <- cps.b %>% select(YEAR, STATENAME, cfw, prime, gender, DWWT) %>% 
  group_by(YEAR, STATENAME, cfw, prime, gender) %>% summarize(overall=sum(DWWT, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, cfw) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_cfw') %>% 
  rename(p3.y4.raw.b = 'Prime_Male_cfw')

cps.p3.y4.b[is.na(cps.p3.y4.b$p3.y4.raw.b) & cps.p3.y4.b$YEAR %in% c(1996,1998,2000,2002,2004,2006,2008,2010),]$p3.y4.raw.b <- 0

cps.emp <- left_join(cps.emp, cps.p3.y4.b, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y4.cfw.rate.b = 100*(p3.y4.raw.b/p3.pop.b)) %>% 
  mutate(p3.y4.cfw.rate.b = ifelse(is.nan(p3.y4.cfw.rate.b), 0, p3.y4.cfw.rate.b))

#p3 matching CPS covariates

#diability
cps.p3.dis.b <- cps.b %>% select(YEAR, STATENAME, disabin, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, disabin, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_dis, age_gen, disabin) %>%
  spread(age_gen_dis, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.disab.raw.b = Prime_Male_Yes) %>%
  mutate(p3.disab.raw.b = ifelse(is.na(p3.disab.raw.b), 0, p3.disab.raw.b))

#disability rate 
cps.emp <- left_join(cps.emp, cps.p3.dis.b, by = c("YEAR","STATENAME")) %>%
  mutate(p3.disab.rate.b = 100*(p3.disab.raw.b/p3.pop.b)) %>% 
  mutate(p3.disab.rate.b = ifelse(is.na(p3.disab.rate.b ), 0, p3.disab.rate.b )) %>%
  mutate(p3.disab.rate.b  = ifelse(is.nan(p3.disab.rate.b ), 0, p3.disab.rate.b )) %>% 
  mutate(p3.disab.raw.b = ifelse(is.na(p3.disab.raw.b), 0, p3.disab.raw.b))

#marriage and cohabitation rates
cps.p3.marry.b <- cps.b %>% select(YEAR, STATENAME, marry, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, marry, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_marry, age_gen, marry) %>%
  spread(age_gen_marry, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.marry.raw.b = Prime_Male_Yes) %>%
  mutate(p3.marry.raw.b = ifelse(is.na(p3.marry.raw.b), 0, p3.marry.raw.b))

cps.emp <- left_join(cps.emp, cps.p3.marry.b, by = c("YEAR","STATENAME")) %>%
  mutate(p3.marriage.rate.b = 100*(p3.marry.raw.b/p3.pop.b)) %>%
  mutate(p3.marriage.rate.b = ifelse(is.na(p3.marriage.rate.b), 0, p3.marriage.rate.b )) %>%
  mutate(p3.marriage.rate.b = ifelse(is.nan(p3.marriage.rate.b ), 0, p3.marriage.rate.b )) %>% 
  mutate(p3.marry.raw.b = ifelse(is.na(p3.marry.raw.b), 0, p3.marry.raw.b))

cps.p3.cohab.b <- cps.b %>% select(YEAR, STATENAME, cohab, prime, gender, WTSUPP) %>% 
  group_by(YEAR, STATENAME, cohab, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_cohab, age_gen, cohab) %>%
  spread(age_gen_cohab, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.cohab.raw.b = Prime_Male_Yes)

#cohab rate - prime males
cps.emp <- left_join(cps.emp, cps.p3.cohab.b, by = c("YEAR","STATENAME")) %>%
  mutate(p3.cohab.rate.b = 100*(p3.cohab.raw.b/p3.pop.b)) %>%
  mutate(p3.cohab.raw.b = ifelse(YEAR >= 2007 & is.na(p3.cohab.raw.b), 0, p3.cohab.raw.b)) %>%
  mutate(p3.cohab.rate.b = ifelse(YEAR >= 2007 & is.na(p3.cohab.rate.b), 0, p3.cohab.rate.b))



#########################
### p4: Prime-age all  
#########################

#p4
cps.p4.b <- cps.b %>% select(YEAR, STATENAME, prime,WTSUPP) %>% 
  group_by(YEAR, STATENAME, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  spread(prime, overall) %>%
  rename(p4.pop.b=Prime) %>% select(YEAR, STATENAME,p4.pop.b)%>% mutate(p4.pop.b = ifelse(is.na(p4.pop.b), 0, p4.pop.b))

cps.emp <- left_join(cps.emp, cps.p4.b, by = c("YEAR","STATENAME"))
cps.emp$p4.pop.b[is.na(cps.emp$p4.pop.b)] <- 0 #fix ten cases that have 0 


#p4.y1
cps.p4.y1.b <- cps.b %>% select(YEAR, STATENAME, employed.share, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed.share, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_emp, prime, employed.share) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Not Employed') %>% 
  rename(p4.y1.raw.b = 'Prime_Not Employed')%>% mutate(p4.y1.raw.b = ifelse(is.na(p4.y1.raw.b), 0, p4.y1.raw.b))

cps.emp <- left_join(cps.emp, cps.p4.y1.b, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y1.notemployed.rate.b = 100*(p4.y1.raw.b/p4.pop.b))

cps.emp$p4.y1.raw.b[is.na(cps.emp$p4.y1.raw.b)] <- 0 #fix ten cases that have 0 black prime no degree males
cps.emp$p4.y1.notemployed.rate.b[is.na(cps.emp$p4.y1.notemployed.rate.b)] <- 0 #fix ten cases that have 0 black prime no degree males

cps.emp <- cps.emp %>% mutate(p4.y1.notemployed.rate.b = ifelse(is.nan(p4.y1.notemployed.rate.b), 0, p4.y1.notemployed.rate.b)) #fix 

#p4.y2 
cps.p4.y2.b <- cps.b %>% select(YEAR, STATENAME, employed, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, employed, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_emp, prime, employed) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Employed', 'Prime_Unemployed') %>%
  rename(p4.y2.raw.b = 'Prime_Unemployed', p4.y2.raw.e.b = 'Prime_Employed') %>%
  mutate(p4.y2.unemployed.rate.b = 100*(p4.y2.raw.b/(p4.y2.raw.b+p4.y2.raw.e.b))) %>%
  select(YEAR, STATENAME, p4.y2.raw.b, p4.y2.unemployed.rate.b) 

cps.p4.y2.b$p4.y2.raw.b[is.na(cps.p4.y2.b$p4.y2.raw.b)] <- 0 
cps.p4.y2.b$p4.y2.unemployed.rate.b[is.na(cps.p4.y2.b$p4.y2.unemployed.rate.b)] <- 0 

cps.emp <- left_join(cps.emp, cps.p4.y2.b, by = c("YEAR","STATENAME"))

cps.emp$p4.y2.raw.b[is.na(cps.emp$p4.y2.raw.b)] <- 0 #fix ten cases that have 0 black prime no degree males
cps.emp$p4.y2.unemployed.rate.b[is.na(cps.emp$p4.y2.unemployed.rate.b)] <- 0 #fix ten cases that have 0 black prime no degree males

#p4.y3
cps.p4.y3.b <- cps.b %>% select(YEAR, STATENAME, idle, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, idle, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_emp, prime, idle) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Idle') %>% 
  rename(p4.y3.raw.b = 'Prime_Idle')%>% mutate(p4.y3.raw.b = ifelse(is.na(p4.y3.raw.b), 0, p4.y3.raw.b))

cps.emp <- left_join(cps.emp, cps.p4.y3.b, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y3.idle.rate.b = 100*(p4.y3.raw.b/p4.pop.b)) %>% mutate(p4.y3.idle.rate.b = ifelse(is.na(p4.y3.idle.rate.b), 0, p4.y3.idle.rate.b)) %>%
  mutate(p4.y3.raw.b = ifelse(is.na(p4.y3.raw.b), 0, p4.y3.raw.b))

#p4.y4
cps.p4.y4.b <- cps.b %>% select(YEAR, STATENAME, cfw, prime, DWWT) %>% 
  group_by(YEAR, STATENAME, cfw, prime) %>% summarize(overall=sum(DWWT, na.rm=T)) %>% 
  unite(age_emp, prime, cfw) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_cfw') %>% 
  rename(p4.y4.raw.b = 'Prime_cfw')

cps.p4.y4.b[is.na(cps.p4.y4.b$p4.y4.raw.b) & cps.p4.y4.b$YEAR %in% c(1996,1998,2000,2002,2004,2006,2008,2010),]$p4.y4.raw.b <- 0


cps.emp <- left_join(cps.emp, cps.p4.y4.b, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y4.cfw.rate.b = 100*(p4.y4.raw.b/p4.pop.b)) %>% 
  mutate(p4.y4.cfw.rate.b = ifelse(is.nan(p4.y4.cfw.rate.b), 0, p4.y4.cfw.rate.b))

#p4 matching CPS covariates

#diability
cps.p4.dis.b <- cps.b %>% select(YEAR, STATENAME, disabin, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, disabin, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_dis, prime, disabin) %>%
  spread(age_dis, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.disab.raw.b = Prime_Yes)

#disability rate 
cps.emp <- left_join(cps.emp, cps.p4.dis.b, by = c("YEAR","STATENAME")) %>%
  mutate(p4.disab.rate.b = 100*(p4.disab.raw.b/p4.pop.b)) %>% 
  mutate(p4.disab.rate.b = ifelse(is.na(p4.disab.rate.b ), 0, p4.disab.rate.b )) %>%
  mutate(p4.disab.rate.b  = ifelse(is.nan(p4.disab.rate.b ), 0, p4.disab.rate.b )) %>% 
  mutate(p4.disab.raw.b = ifelse(is.na(p4.disab.raw.b), 0, p4.disab.raw.b))


#marriage and cohabitation rates
cps.p4.marry.b <- cps.b %>% select(YEAR, STATENAME, marry, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, marry, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_marry, prime, marry) %>%
  spread(age_marry, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.marry.raw.b = Prime_Yes) %>%
  mutate(p4.marry.raw.b = ifelse(is.na(p4.marry.raw.b), 0, p4.marry.raw.b))

cps.emp <- left_join(cps.emp, cps.p4.marry.b, by = c("YEAR","STATENAME")) %>%
  mutate(p4.marriage.rate.b = 100*(p4.marry.raw.b/p4.pop.b)) %>%
  mutate(p4.marriage.rate.b = ifelse(is.na(p4.marriage.rate.b), 0, p4.marriage.rate.b )) %>%
  mutate(p4.marriage.rate.b = ifelse(is.nan(p4.marriage.rate.b ), 0, p4.marriage.rate.b )) %>% 
  mutate(p4.marry.raw.b = ifelse(is.na(p4.marry.raw.b), 0, p4.marry.raw.b))

cps.p4.cohab.b <- cps.b %>% select(YEAR, STATENAME, cohab, prime, WTSUPP) %>% 
  group_by(YEAR, STATENAME, cohab, prime) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
  unite(age_cohab, prime, cohab) %>%
  spread(age_cohab, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.cohab.raw.b = Prime_Yes)

cps.emp <- left_join(cps.emp, cps.p4.cohab.b, by = c("YEAR","STATENAME")) %>%
  mutate(p4.cohab.rate.b = 100*(p4.cohab.raw.b/p4.pop.b)) %>%
  mutate(p4.cohab.raw.b = ifelse(YEAR >= 2007 & is.na(p4.cohab.raw.b), 0, p4.cohab.raw.b)) %>%
  mutate(p4.cohab.rate.b = ifelse(YEAR >= 2007 & is.na(p4.cohab.rate.b), 0, p4.cohab.rate.b))

#############################
### Reconnect State FIPS
############################

#reconnect numerical FIPS code
states <- read.csv(file="fips.csv", header=T)
states <- states[1:75,] #removing error NA case at end of data
states$STATEFIP <- states$?..STATEFIP
states$?..STATEFIP <- NULL
cps.emp <- left_join(cps.emp, states, by="STATENAME")

#########################
### 5-year period indicator
##########################

cps.emp$time.indicator <- rep(NA, length(cps.emp$YEAR)) 
cps.emp$time.indicator[cps.emp$YEAR >= 1980 & cps.emp$YEAR <=1984] <- "1980-1984"
cps.emp$time.indicator[cps.emp$YEAR > 1984 & cps.emp$YEAR <=1989] <- "1985-1989"
cps.emp$time.indicator[cps.emp$YEAR > 1989 & cps.emp$YEAR <=1994] <- "1990-1994"
cps.emp$time.indicator[cps.emp$YEAR > 1994 & cps.emp$YEAR <=1999] <- "1995-1999"
cps.emp$time.indicator[cps.emp$YEAR > 1999 & cps.emp$YEAR <=2004] <- "2000-2004"
cps.emp$time.indicator[cps.emp$YEAR > 2004 & cps.emp$YEAR <=2010] <- "2005-2010"
table(cps.emp$YEAR, cps.emp$time.indicator)


######################################
### Merge State-Year Felon Estimates
######################################

felon <- read.csv(file="Shannon_etal_2017_state_estimates_1980-2010_revised.csv", header=T) %>% 
  select(?..State, Year, pctexfel, pctexpris, blkpctexfel, blkpctexpris) %>% rename(STATENAME=?..State, YEAR=Year) %>% 
  mutate(STATENAME=replace(as.character(STATENAME), STATENAME=="New Hamp","New Hampshire")) %>% 
  rename(pctexfel.b=blkpctexfel, pctexpris.b=blkpctexpris)

cps.emp <- left_join(cps.emp, felon, by=c("YEAR","STATENAME"))

############################
### SSI/SSDI Rates
########################

ssdi <- read.csv(file="SSA-SA-FYWL.csv")
ssdi <- ssdi %>% select(State.Code, Date, Percent.of.Adult.Population.Receiving.SSA.Adult.Disability.Benefits) %>%
  rename(YEAR = Date, stateabb = State.Code, ssdi.rate = Percent.of.Adult.Population.Receiving.SSA.Adult.Disability.Benefits) %>%
  mutate(stateabb=str_trim(stateabb), STATENAME = state.name[match(stateabb,state.abb)]) %>% select(-stateabb)

cps.emp <- left_join(cps.emp, ssdi, by=c("YEAR","STATENAME"))

ssi <- read.csv(file="SSI.csv")
ssi <- ssi %>% rename(YEAR = Year, STATENAME = State, ssi.rec = SSI_recip)

cps.emp <- left_join(cps.emp, ssi, by=c("YEAR","STATENAME")) %>% mutate(ssi.rate = ssi.rec/population.16)

#################################
### UKCPR Variables
################################

states2 <- as.data.frame(cbind(state.name, state.abb)) %>% rename(state_name=state.abb)


uk <-  read.csv(file="UKCPR.csv", header=T) %>% left_join(states2, by="state_name") %>% 
  select(state.name, year,Federal.Minimum.Wage, State.Minimum.Wage, 
          AFDC.TANF.Benefit.for.2.Person.family,AFDC.TANF.Benefit.for.3.person.family,
          AFDC.TANF.benefit.for.4.person.family) %>%
  rename(fed.wage=Federal.Minimum.Wage, state.wage=State.Minimum.Wage, 
         TANF.2=AFDC.TANF.Benefit.for.2.Person.family, TANF.3=AFDC.TANF.Benefit.for.3.person.family,
         TANF.4=AFDC.TANF.benefit.for.4.person.family, YEAR=year, STATENAME=state.name) %>%
  mutate(effective.wage= ifelse(fed.wage>state.wage, fed.wage, state.wage), 
         TANF.mu = (TANF.2+TANF.3+TANF.4)/3)
  
cps.emp <- left_join(cps.emp, uk, by=c("YEAR","STATENAME"))

##################################
### Merging Correlates of State Policy - CSP
#####################################

data(csp, package = "csp")

#clean to make enough memory, keep cps.emp
rm(list=setdiff(ls(), c("cps.emp")))

#example code to reshape with chosen covariates
csp.subset <- csp %>% filter(year>= 1980 & year<= 2010) %>%
  select(year, state, variable, value) %>% spread(variable, value) %>% 
  rename(YEAR=year, STATENAME=state) %>% select(YEAR, STATENAME,z_labor_unemployment_compensatio)

#statemin = State minimum wage in dollars (1980-2014). Tax Policy Center
#z_tanf_maxpayment = What is the maximum level of benefis under the Temporary Aid for
    #Needy Families program for a family of three with no income? NOW USING DATA FROM UKCPR
#z_labor_unemployment_compensation = What is the maximum weekly amount of unemployment benefits? (1937-2014)

#merge with CPS
cps.emp <- left_join(cps.emp, csp.subset, by = c("YEAR","STATENAME"))



########################################
#### Writing final data
#################################

#removing district of columbia
cps.emp <- cps.emp %>% filter(STATENAME != "District of Columbia")

#number of missing cases per variable
colSums(is.na(cps.emp))

#write final csv
write.csv(cps.emp, file="FE_prelim.csv") #final.data when CSP appended
