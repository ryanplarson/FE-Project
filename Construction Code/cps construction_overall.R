####################################################
### CPS Dataset Construction
### Ryan Larson, UMN
##################################################
library(ff)
library(dplyr)
library(tidyr)
library(stringr)
library(csp)

#read in cps_asec data
cps <- as.data.frame(read.csv.ffdf(file="cps_basic.csv", header=T, VERBOSE = T, first.rows=10000, next.rows=50000,
                     sep=",", colClasses = NA)) 
class(cps)
str(cps)

cps <- cps %>% select(YEAR, MONTH, CPSID, STATEFIP, WTFINL, AGE, SEX, RACE, MARST, 
                      EMPSTAT, WNLOOK, EDUC) %>% filter(AGE>=16) 




#adding in state names
states <- read.csv(file="fips.csv", header=T, stringsAsFactors = F)
states <- states[1:75,] #removing error NA case at end of data
states <- states %>% rename(STATEFIP = ï..STATEFIP) 
cps <- left_join(cps, states, by="STATEFIP")
rm(states)

#############################
### Demographic Recodes
############################

#prime-age binary
cps$prime <- ifelse(cps$AGE>=25 & cps$AGE<=54, "Prime", "Non-Prime")

#working age binary
cps$work.age <- ifelse(cps$AGE>=18 & cps$AGE<=65, "Working Age", "Non-Working Age")

#young adult binary
cps$ya <- ifelse(cps$AGE>=18 & cps$AGE<=25, "Young-Adult", "Adult")

#p6 age designation
cps$psix <- ifelse(cps$AGE>=18 & cps$AGE<=54, "Yes", "No")

#p7 age designation
cps$pseven <- ifelse(cps$AGE>=18 & cps$AGE<=40, "Yes", "No")

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

#######################################
### Can't Find Work (NILF & WNLOOK 1-5)

cps$cfw <- cps$employed
cps$cfw[cps$EMPSTAT >= 30 & cps$WNLOOK==1|cps$WNLOOK==2|cps$WNLOOK==3|cps$WNLOOK==4|cps$WNLOOK==5] <- "cfw"
cps$cfw <- as.factor(cps$cfw)
table(cps$EMPSTAT, cps$cfw)


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


#CPS Basic Monthly Adjustment
cps <- cps %>% mutate(WTFINL = (WTFINL/12))

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

#p1.y4
cps.p1.y4 <- cps %>% select(YEAR, STATENAME, degree,cfw, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, cfw) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_cfw') %>% 
  rename(p1.y4.raw = 'No_Prime_Male_cfw')


cps.emp <- left_join(cps.emp, cps.p1.y4, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y4.cfw.rate = 100*(p1.y4.raw/p1.pop))

#p1.y4.female
cps.p1.y4 <- cps %>% select(YEAR, STATENAME, degree,cfw, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, cfw) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Female_cfw') %>% 
  rename(p1.y4.raw.female = 'No_Prime_Female_cfw')

cps.emp <- left_join(cps.emp, cps.p1.y4, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y4.cfw.rate.female = 100*(p1.y4.raw.female/p1.pop.female))








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

#p2.y4
cps.p2.y4 <- cps %>% select(YEAR, STATENAME, degree,cfw, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, cfw) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_cfw') %>% 
  rename(p2.y4.raw = 'No_Working Age_Male_cfw')


cps.emp <- left_join(cps.emp, cps.p2.y4, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y4.cfw.rate = 100*(p2.y4.raw/p2.pop))

#p2.y4.female
cps.p2.y4 <- cps %>% select(YEAR, STATENAME, degree,cfw, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, cfw) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Female_cfw') %>% 
  rename(p2.y4.raw.female = 'No_Working Age_Female_cfw')


cps.emp <- left_join(cps.emp, cps.p2.y4, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y4.cfw.rate.female = 100*(p2.y4.raw.female/p2.pop.female))



#marriage and cohabitation rates
cps.p2.marry <- cps %>% select(YEAR, STATENAME, degree,marry, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_marry, degree_age_gen, marry) %>%
  spread(degree_age_gen_marry, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.marry.raw = 'No_Working Age_Male_Yes') 

cps.emp <- left_join(cps.emp, cps.p2.marry, by = c("YEAR","STATENAME")) %>%
  mutate(p2.marriage.rate = 100*(p2.marry.raw/p2.pop))

cps.p2.marry.f <- cps %>% select(YEAR, STATENAME, degree,marry, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_marry, degree_age_gen, marry) %>%
  spread(degree_age_gen_marry, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Female_Yes') %>% rename(p2.marry.raw.female = 'No_Working Age_Female_Yes') 

cps.emp <- left_join(cps.emp, cps.p2.marry.f, by = c("YEAR","STATENAME")) %>%
  mutate(p2.marriage.rate.female = 100*(p2.marry.raw.female/p2.pop.female))

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


#p3.y4
cps.p3.y4 <- cps %>% select(YEAR, STATENAME, cfw, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, cfw) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_cfw') %>% 
  rename(p3.y4.raw = 'Prime_Male_cfw')

cps.emp <- left_join(cps.emp, cps.p3.y4, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y4.cfw.rate = 100*(p3.y4.raw/p3.pop))

#p3.y4.female
cps.p3.y4 <- cps %>% select(YEAR, STATENAME, cfw, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, cfw) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Female_cfw') %>% 
  rename(p3.y4.raw.female = 'Prime_Female_cfw')

cps.emp <- left_join(cps.emp, cps.p3.y4, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y4.cfw.rate.female = 100*(p3.y4.raw.female/p3.pop.female))

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


#p4.y4
cps.p4.y4 <- cps %>% select(YEAR, STATENAME, cfw, prime, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, prime, cfw) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_cfw') %>% 
  rename(p4.y4.raw = 'Prime_cfw')

cps.emp <- left_join(cps.emp, cps.p4.y4, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y4.cfw.rate = 100*(p4.y4.raw/p4.pop))




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

#########################
### p5: yuong-adult (18-25) all  
#########################

#p5
cps.p5 <- cps %>% select(YEAR, STATENAME, ya, WTFINL) %>% 
  group_by(YEAR, STATENAME, ya) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  spread(ya, overall) %>%
  rename(p5.pop='Young-Adult') %>% select(YEAR, STATENAME,p5.pop)

cps.emp <- left_join(cps.emp, cps.p5, by = c("YEAR","STATENAME"))


#p5.y1
cps.p5.y1 <- cps %>% select(YEAR, STATENAME, employed.share, ya, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, ya) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, ya, employed.share) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Young-Adult_Not Employed') %>% 
  rename(p5.y1.raw = 'Young-Adult_Not Employed')

cps.emp <- left_join(cps.emp, cps.p5.y1, by = c("YEAR","STATENAME")) %>%
  mutate(p5.y1.notemployed.rate = 100*(p5.y1.raw/p5.pop))

#p5.y2 
cps.p5.y2 <- cps %>% select(YEAR, STATENAME, employed, ya, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, ya) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, ya, employed) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Young-Adult_Employed', 'Young-Adult_Unemployed') %>%
  rename(p5.y2.raw = 'Young-Adult_Unemployed', p5.y2.raw.e = 'Young-Adult_Employed') %>%
  mutate(p5.y2.unemployed.rate = 100*(p5.y2.raw/(p5.y2.raw+p5.y2.raw.e))) %>%
  select(YEAR, STATENAME, p5.y2.raw, p5.y2.unemployed.rate) 

cps.emp <- left_join(cps.emp, cps.p5.y2, by = c("YEAR","STATENAME"))



#p5.y3
cps.p5.y3 <- cps %>% select(YEAR, STATENAME, idle, ya, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, ya) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, ya, idle) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Young-Adult_Idle') %>% 
  rename(p5.y3.raw = 'Young-Adult_Idle')

cps.emp <- left_join(cps.emp, cps.p5.y3, by = c("YEAR","STATENAME")) %>%
  mutate(p5.y3.idle.rate = 100*(p5.y3.raw/p5.pop))


#p5.y4
cps.p5.y4 <- cps %>% select(YEAR, STATENAME, cfw, ya, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, ya) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, ya, cfw) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Young-Adult_cfw') %>% 
  rename(p5.y4.raw = 'Young-Adult_cfw')

cps.emp <- left_join(cps.emp, cps.p5.y4, by = c("YEAR","STATENAME")) %>%
  mutate(p5.y4.cfw.rate = 100*(p5.y4.raw/p5.pop))

#marriage and cohabitation rates
cps.p5.marry <- cps %>% select(YEAR, STATENAME, marry, ya, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, ya) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_marry, ya, marry) %>%
  spread(age_marry, overall) %>% select(YEAR, STATENAME, 'Young-Adult_Yes') %>% rename(p5.marry.raw = 'Young-Adult_Yes') 

cps.emp <- left_join(cps.emp, cps.p5.marry, by = c("YEAR","STATENAME")) %>%
  mutate(p5.marriage.rate = 100*(p5.marry.raw/p5.pop))


#########################
### p6: 18-54 all  
#########################

#p6
cps.p6 <- cps %>% select(YEAR, STATENAME, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  spread(psix, overall) %>%
  rename(p6.pop='Yes') %>% select(YEAR, STATENAME,p6.pop)

cps.emp <- left_join(cps.emp, cps.p6, by = c("YEAR","STATENAME"))


#p6.y1
cps.p6.y1 <- cps %>% select(YEAR, STATENAME, employed.share, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, psix, employed.share) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Not Employed') %>% 
  rename(p6.y1.raw = 'Yes_Not Employed')

cps.emp <- left_join(cps.emp, cps.p6.y1, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y1.notemployed.rate = 100*(p6.y1.raw/p6.pop))

#p6.y2 
cps.p6.y2 <- cps %>% select(YEAR, STATENAME, employed, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, psix, employed) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Employed', 'Yes_Unemployed') %>%
  rename(p6.y2.raw = 'Yes_Unemployed', p6.y2.raw.e = 'Yes_Employed') %>%
  mutate(p6.y2.unemployed.rate = 100*(p6.y2.raw/(p6.y2.raw+p6.y2.raw.e))) %>%
  select(YEAR, STATENAME, p6.y2.raw, p6.y2.unemployed.rate) 

cps.emp <- left_join(cps.emp, cps.p6.y2, by = c("YEAR","STATENAME"))



#p6.y3
cps.p6.y3 <- cps %>% select(YEAR, STATENAME, idle, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, psix, idle) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Idle') %>% 
  rename(p6.y3.raw = 'Yes_Idle')

cps.emp <- left_join(cps.emp, cps.p6.y3, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y3.idle.rate = 100*(p6.y3.raw/p6.pop))


#p6.y4
cps.p6.y4 <- cps %>% select(YEAR, STATENAME, cfw, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, psix, cfw) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_cfw') %>% 
  rename(p6.y4.raw = 'Yes_cfw')

cps.emp <- left_join(cps.emp, cps.p6.y4, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y4.cfw.rate = 100*(p6.y4.raw/p6.pop))




#marriage and cohabitation rates
cps.p6.marry <- cps %>% select(YEAR, STATENAME, marry, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_marry, psix, marry) %>%
  spread(age_marry, overall) %>% select(YEAR, STATENAME, 'Yes_Yes') %>% rename(p6.marry.raw = 'Yes_Yes') 

cps.emp <- left_join(cps.emp, cps.p6.marry, by = c("YEAR","STATENAME")) %>%
  mutate(p6.marriage.rate = 100*(p6.marry.raw/p6.pop))

#overall p6 education
cps.p6.degree <- cps %>% select(YEAR, STATENAME, degree, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_degree, psix, degree) %>%
  spread(age_degree, overall) %>% select(YEAR, STATENAME, 'Yes_Yes') %>% rename(p6.degree.raw = 'Yes_Yes') 

cps.emp <- left_join(cps.emp, cps.p6.degree, by = c("YEAR","STATENAME")) %>%
  mutate(p6.degree.rate = 100*(p6.degree.raw/p6.pop))


#######
# p6 gender breakdowns



#p6.male
cps.p6.m <- cps %>% select(YEAR, STATENAME, psix, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>% spread(age_gen, overall) %>%
  rename(p6.pop.m=Yes_Male) %>% select(YEAR, STATENAME,p6.pop.m)

cps.emp <- left_join(cps.emp, cps.p6.m, by = c("YEAR","STATENAME"))

#p6.female
cps.p6.f <- cps %>% select(YEAR, STATENAME, psix, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>% spread(age_gen, overall) %>%
  rename(p6.pop.female=Yes_Female) %>% select(YEAR, STATENAME,p6.pop.female)

cps.emp <- left_join(cps.emp, cps.p6.f, by = c("YEAR","STATENAME"))


#p6.y1.male
cps.p6.y1.m <- cps %>% select(YEAR, STATENAME, employed.share, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>%
  unite(age_gen_emp, age_gen, employed.share) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Male_Not Employed') %>% 
  rename(p6.y1.raw.m = 'Yes_Male_Not Employed')

cps.emp <- left_join(cps.emp, cps.p6.y1.m, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y1.notemployed.rate.m = 100*(p6.y1.raw.m/p6.pop.m))

#p6.y1.female
cps.p6.y1.f <- cps %>% select(YEAR, STATENAME, employed.share, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>%
  unite(age_gen_emp, age_gen, employed.share) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Female_Not Employed') %>% 
  rename(p6.y1.raw.female = 'Yes_Female_Not Employed')

cps.emp <- left_join(cps.emp, cps.p6.y1.f, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y1.notemployed.rate.female = 100*(p6.y1.raw.female/p6.pop.female))

#p6.y2 .male
cps.p6.y2.m <- cps %>% select(YEAR, STATENAME, employed, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>%
  unite(age_gen_emp, age_gen, employed) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Male_Employed', 'Yes_Male_Unemployed') %>%
  rename(p6.y2.raw.m = 'Yes_Male_Unemployed', p6.y2.raw.e = 'Yes_Male_Employed') %>%
  mutate(p6.y2.unemployed.rate.m = 100*(p6.y2.raw.m/(p6.y2.raw.m+p6.y2.raw.e))) %>%
  select(YEAR, STATENAME, p6.y2.raw.m, p6.y2.unemployed.rate.m) 


cps.emp <- left_join(cps.emp, cps.p6.y2.m, by = c("YEAR","STATENAME"))

#p6.y2.female
cps.p6.y2.f <- cps %>% select(YEAR, STATENAME, employed, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>%
  unite(age_gen_emp, age_gen, employed) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Female_Employed', 'Yes_Female_Unemployed') %>%
  rename(p6.y2.raw.female = 'Yes_Female_Unemployed', p6.y2.raw.e.female = 'Yes_Female_Employed') %>%
  mutate(p6.y2.unemployed.rate.female = 100*(p6.y2.raw.female/(p6.y2.raw.female+p6.y2.raw.e.female))) %>%
  select(YEAR, STATENAME, p6.y2.raw.female, p6.y2.unemployed.rate.female) 

cps.emp <- left_join(cps.emp, cps.p6.y2.f, by = c("YEAR","STATENAME"))


#p6.y3.male
cps.p6.y3.m <- cps %>% select(YEAR, STATENAME, idle, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>%
  unite(age_gen_emp, age_gen, idle) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Male_Idle') %>% 
  rename(p6.y3.raw.m = 'Yes_Male_Idle')

cps.emp <- left_join(cps.emp, cps.p6.y3.m, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y3.idle.rate.m = 100*(p6.y3.raw.m/p6.pop.m))

#p6.y3.female
cps.p6.y3.f <- cps %>% select(YEAR, STATENAME, idle, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>%
  unite(age_gen_emp, age_gen, idle) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Female_Idle') %>% 
  rename(p6.y3.raw.female = 'Yes_Female_Idle')

cps.emp <- left_join(cps.emp, cps.p6.y3.f, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y3.idle.rate.female = 100*(p6.y3.raw.female/p6.pop.female))


#p6.y4.male
cps.p6.y4.m <- cps %>% select(YEAR, STATENAME, cfw, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>%
  unite(age_gen_emp, age_gen, cfw) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Male_cfw') %>% 
  rename(p6.y4.raw.m = 'Yes_Male_cfw')

cps.emp <- left_join(cps.emp, cps.p6.y4.m, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y4.cfw.rate.m = 100*(p6.y4.raw.m/p6.pop.m))

#p6.y4.female
cps.p6.y4.f <- cps %>% select(YEAR, STATENAME, cfw, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>%
  unite(age_gen_emp, age_gen, cfw) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Female_cfw') %>% 
  rename(p6.y4.raw.female = 'Yes_Female_cfw')

cps.emp <- left_join(cps.emp, cps.p6.y4.f, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y4.cfw.rate.female = 100*(p6.y4.raw.female/p6.pop.female))

#marriage and cohabitation rates.male
cps.p6.marry.m <- cps %>% select(YEAR, STATENAME, marry, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>% unite(age_gen_marry, age_gen, marry) %>%
  spread(age_gen_marry, overall) %>% select(YEAR, STATENAME, Yes_Male_Yes) %>% rename(p6.marry.raw.m = Yes_Male_Yes) 

cps.emp <- left_join(cps.emp, cps.p6.marry.m, by = c("YEAR","STATENAME")) %>%
  mutate(p6.marriage.rate.m = 100*(p6.marry.raw.m/p6.pop.m))

#marriage.female
cps.p6.marry.f <- cps %>% select(YEAR, STATENAME, marry, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>% unite(age_gen_marry, age_gen, marry) %>%
  spread(age_gen_marry, overall) %>% select(YEAR, STATENAME, Yes_Female_Yes) %>% rename(p6.marry.raw.female = Yes_Female_Yes) 

cps.emp <- left_join(cps.emp, cps.p6.marry.f, by = c("YEAR","STATENAME")) %>%
  mutate(p6.marriage.rate.female = 100*(p6.marry.raw.female/p6.pop.female))

#p6 education male
cps.p6.degree.m <- cps %>% select(YEAR, STATENAME, degree, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>% unite(age_gen_degree, age_gen, degree) %>%
  spread(age_gen_degree, overall) %>% select(YEAR, STATENAME, Yes_Male_Yes) %>% rename(p6.degree.raw.m = Yes_Male_Yes) 

cps.emp <- left_join(cps.emp, cps.p6.degree.m, by = c("YEAR","STATENAME")) %>%
  mutate(p6.degree.rate.m = 100*(p6.degree.raw.m/p6.pop.m))

#p6 education female
cps.p6.degree.f <- cps %>% select(YEAR, STATENAME, degree, psix, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, psix, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>% unite(age_gen_degree, age_gen, degree) %>%
  spread(age_gen_degree, overall) %>% select(YEAR, STATENAME, Yes_Female_Yes) %>% rename(p6.degree.raw.female = Yes_Female_Yes) 

cps.emp <- left_join(cps.emp, cps.p6.degree.f, by = c("YEAR","STATENAME")) %>%
  mutate(p6.degree.rate.female = 100*(p6.degree.raw.female/p6.pop.female))


##################################################################
#P7: male/Female 18-40

#p7
cps.p7 <- cps %>% select(YEAR, STATENAME, pseven, WTFINL) %>% 
  group_by(YEAR, STATENAME, pseven) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  spread(pseven, overall) %>%
  rename(p7.pop='Yes') %>% select(YEAR, STATENAME,p7.pop)

cps.emp <- left_join(cps.emp, cps.p7, by = c("YEAR","STATENAME"))


#p7.y1
cps.p7.y1 <- cps %>% select(YEAR, STATENAME, employed.share, pseven, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, pseven) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, pseven, employed.share) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Not Employed') %>% 
  rename(p7.y1.raw = 'Yes_Not Employed')

cps.emp <- left_join(cps.emp, cps.p7.y1, by = c("YEAR","STATENAME")) %>%
  mutate(p7.y1.notemployed.rate = 100*(p7.y1.raw/p7.pop))

#p7.y2 
cps.p7.y2 <- cps %>% select(YEAR, STATENAME, employed, pseven, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, pseven) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, pseven, employed) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Employed', 'Yes_Unemployed') %>%
  rename(p7.y2.raw = 'Yes_Unemployed', p7.y2.raw.e = 'Yes_Employed') %>%
  mutate(p7.y2.unemployed.rate = 100*(p7.y2.raw/(p7.y2.raw+p7.y2.raw.e))) %>%
  select(YEAR, STATENAME, p7.y2.raw, p7.y2.unemployed.rate) 

cps.emp <- left_join(cps.emp, cps.p7.y2, by = c("YEAR","STATENAME"))



#p7.y3
cps.p7.y3 <- cps %>% select(YEAR, STATENAME, idle, pseven, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, pseven) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, pseven, idle) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Idle') %>% 
  rename(p7.y3.raw = 'Yes_Idle')

cps.emp <- left_join(cps.emp, cps.p7.y3, by = c("YEAR","STATENAME")) %>%
  mutate(p7.y3.idle.rate = 100*(p7.y3.raw/p7.pop))


#p7.y4
cps.p7.y4 <- cps %>% select(YEAR, STATENAME, cfw, pseven, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, pseven) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, pseven, cfw) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_cfw') %>% 
  rename(p7.y4.raw = 'Yes_cfw')

cps.emp <- left_join(cps.emp, cps.p7.y4, by = c("YEAR","STATENAME")) %>%
  mutate(p7.y4.cfw.rate = 100*(p7.y4.raw/p7.pop))




#marriage and cohabitation rates
cps.p7.marry <- cps %>% select(YEAR, STATENAME, marry, pseven, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, pseven) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_marry, pseven, marry) %>%
  spread(age_marry, overall) %>% select(YEAR, STATENAME, 'Yes_Yes') %>% rename(p7.marry.raw = 'Yes_Yes') 

cps.emp <- left_join(cps.emp, cps.p7.marry, by = c("YEAR","STATENAME")) %>%
  mutate(p7.marriage.rate = 100*(p7.marry.raw/p7.pop))

#overall p7 education
cps.p7.degree <- cps %>% select(YEAR, STATENAME, degree, pseven, WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, pseven) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_degree, pseven, degree) %>%
  spread(age_degree, overall) %>% select(YEAR, STATENAME, 'Yes_Yes') %>% rename(p7.degree.raw = 'Yes_Yes') 

cps.emp <- left_join(cps.emp, cps.p7.degree, by = c("YEAR","STATENAME")) %>%
  mutate(p7.degree.rate = 100*(p7.degree.raw/p7.pop))


#######
# p7 gender breakdowns


#p7.male
cps.p7.m <- cps %>% select(YEAR, STATENAME, pseven, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>% spread(age_gen, overall) %>%
  rename(p7.pop.m=Yes_Male) %>% select(YEAR, STATENAME,p7.pop.m)

cps.emp <- left_join(cps.emp, cps.p7.m, by = c("YEAR","STATENAME"))

#p7.female
cps.p7.f <- cps %>% select(YEAR, STATENAME, pseven, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>% spread(age_gen, overall) %>%
  rename(p7.pop.female=Yes_Female) %>% select(YEAR, STATENAME,p7.pop.female)

cps.emp <- left_join(cps.emp, cps.p7.f, by = c("YEAR","STATENAME"))


#p7.y1.male
cps.p7.y1.m <- cps %>% select(YEAR, STATENAME, employed.share, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>%
  unite(age_gen_emp, age_gen, employed.share) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Male_Not Employed') %>% 
  rename(p7.y1.raw.m = 'Yes_Male_Not Employed')

cps.emp <- left_join(cps.emp, cps.p7.y1.m, by = c("YEAR","STATENAME")) %>%
  mutate(p7.y1.notemployed.rate.m = 100*(p7.y1.raw.m/p7.pop.m))

#p7.y1.female
cps.p7.y1.f <- cps %>% select(YEAR, STATENAME, employed.share, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>%
  unite(age_gen_emp, age_gen, employed.share) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Female_Not Employed') %>% 
  rename(p7.y1.raw.female = 'Yes_Female_Not Employed')

cps.emp <- left_join(cps.emp, cps.p7.y1.f, by = c("YEAR","STATENAME")) %>%
  mutate(p7.y1.notemployed.rate.female = 100*(p7.y1.raw.female/p7.pop.female))

#p7.y2 .male
cps.p7.y2.m <- cps %>% select(YEAR, STATENAME, employed, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>%
  unite(age_gen_emp, age_gen, employed) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Male_Employed', 'Yes_Male_Unemployed') %>%
  rename(p7.y2.raw.m = 'Yes_Male_Unemployed', p7.y2.raw.e = 'Yes_Male_Employed') %>%
  mutate(p7.y2.unemployed.rate.m = 100*(p7.y2.raw.m/(p7.y2.raw.m+p7.y2.raw.e))) %>%
  select(YEAR, STATENAME, p7.y2.raw.m, p7.y2.unemployed.rate.m) 


cps.emp <- left_join(cps.emp, cps.p7.y2.m, by = c("YEAR","STATENAME"))

#p7.y2.female
cps.p7.y2.f <- cps %>% select(YEAR, STATENAME, employed, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>%
  unite(age_gen_emp, age_gen, employed) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Female_Employed', 'Yes_Female_Unemployed') %>%
  rename(p7.y2.raw.female = 'Yes_Female_Unemployed', p7.y2.raw.e.female = 'Yes_Female_Employed') %>%
  mutate(p7.y2.unemployed.rate.female = 100*(p7.y2.raw.female/(p7.y2.raw.female+p7.y2.raw.e.female))) %>%
  select(YEAR, STATENAME, p7.y2.raw.female, p7.y2.unemployed.rate.female) 

cps.emp <- left_join(cps.emp, cps.p7.y2.f, by = c("YEAR","STATENAME"))


#p7.y3.male
cps.p7.y3.m <- cps %>% select(YEAR, STATENAME, idle, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>%
  unite(age_gen_emp, age_gen, idle) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Male_Idle') %>% 
  rename(p7.y3.raw.m = 'Yes_Male_Idle')

cps.emp <- left_join(cps.emp, cps.p7.y3.m, by = c("YEAR","STATENAME")) %>%
  mutate(p7.y3.idle.rate.m = 100*(p7.y3.raw.m/p7.pop.m))

#p7.y3.female
cps.p7.y3.f <- cps %>% select(YEAR, STATENAME, idle, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>%
  unite(age_gen_emp, age_gen, idle) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Female_Idle') %>% 
  rename(p7.y3.raw.female = 'Yes_Female_Idle')

cps.emp <- left_join(cps.emp, cps.p7.y3.f, by = c("YEAR","STATENAME")) %>%
  mutate(p7.y3.idle.rate.female = 100*(p7.y3.raw.female/p7.pop.female))


#p7.y4.male
cps.p7.y4.m <- cps %>% select(YEAR, STATENAME, cfw, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>%
  unite(age_gen_emp, age_gen, cfw) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Male_cfw') %>% 
  rename(p7.y4.raw.m = 'Yes_Male_cfw')

cps.emp <- left_join(cps.emp, cps.p7.y4.m, by = c("YEAR","STATENAME")) %>%
  mutate(p7.y4.cfw.rate.m = 100*(p7.y4.raw.m/p7.pop.m))

#p7.y4.female
cps.p7.y4.f <- cps %>% select(YEAR, STATENAME, cfw, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>%
  unite(age_gen_emp, age_gen, cfw) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Female_cfw') %>% 
  rename(p7.y4.raw.female = 'Yes_Female_cfw')

cps.emp <- left_join(cps.emp, cps.p7.y4.f, by = c("YEAR","STATENAME")) %>%
  mutate(p7.y4.cfw.rate.female = 100*(p7.y4.raw.female/p7.pop.female))

#marriage and cohabitation rates.male
cps.p7.marry.m <- cps %>% select(YEAR, STATENAME, marry, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>% unite(age_gen_marry, age_gen, marry) %>%
  spread(age_gen_marry, overall) %>% select(YEAR, STATENAME, Yes_Male_Yes) %>% rename(p7.marry.raw.m = Yes_Male_Yes) 

cps.emp <- left_join(cps.emp, cps.p7.marry.m, by = c("YEAR","STATENAME")) %>%
  mutate(p7.marriage.rate.m = 100*(p7.marry.raw.m/p7.pop.m))

#marriage.female
cps.p7.marry.f <- cps %>% select(YEAR, STATENAME, marry, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>% unite(age_gen_marry, age_gen, marry) %>%
  spread(age_gen_marry, overall) %>% select(YEAR, STATENAME, Yes_Female_Yes) %>% rename(p7.marry.raw.female = Yes_Female_Yes) 

cps.emp <- left_join(cps.emp, cps.p7.marry.f, by = c("YEAR","STATENAME")) %>%
  mutate(p7.marriage.rate.female = 100*(p7.marry.raw.female/p7.pop.female))

#p7 education male
cps.p7.degree.m <- cps %>% select(YEAR, STATENAME, degree, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>% unite(age_gen_degree, age_gen, degree) %>%
  spread(age_gen_degree, overall) %>% select(YEAR, STATENAME, Yes_Male_Yes) %>% rename(p7.degree.raw.m = Yes_Male_Yes) 

cps.emp <- left_join(cps.emp, cps.p7.degree.m, by = c("YEAR","STATENAME")) %>%
  mutate(p7.degree.rate.m = 100*(p7.degree.raw.m/p7.pop.m))

#p7 education female
cps.p7.degree.f <- cps %>% select(YEAR, STATENAME, degree, pseven, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, pseven, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>% unite(age_gen_degree, age_gen, degree) %>%
  spread(age_gen_degree, overall) %>% select(YEAR, STATENAME, Yes_Female_Yes) %>% rename(p7.degree.raw.female = Yes_Female_Yes) 

cps.emp <- left_join(cps.emp, cps.p7.degree.f, by = c("YEAR","STATENAME")) %>%
  mutate(p7.degree.rate.female = 100*(p7.degree.raw.female/p7.pop.female))

#####################################################################
#P8: Working age no education subset

#################################################################
#p8 overall
cps.p8 <- cps %>% select(YEAR, STATENAME, work.age, WTFINL) %>%
  group_by(YEAR, STATENAME, work.age) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>%
  spread(work.age, overall) %>%
  rename(p8.pop='Working Age') %>% select(YEAR, STATENAME,p8.pop)

cps.emp <- left_join(cps.emp, cps.p8, by = c("YEAR","STATENAME"))

#p8.m
cps.p8.m <- cps %>% select(YEAR, STATENAME, work.age, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gender, work.age, gender) %>%  spread(age_gender, overall) %>%
  rename(p8.pop.m='Working Age_Male') %>% select(YEAR, STATENAME,p8.pop.m)

cps.emp <- left_join(cps.emp, cps.p8.m, by = c("YEAR","STATENAME")) 

#p8.female
cps.p8.f <- cps %>% select(YEAR, STATENAME,  work.age, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gender, work.age, gender) %>%   spread(age_gender, overall) %>%
  rename(p8.pop.female='Working Age_Female') %>% select(YEAR, STATENAME, p8.pop.female)

cps.emp <- left_join(cps.emp, cps.p8.f, by = c("YEAR","STATENAME"))

#p8.y1.m
cps.p8.y1.m <- cps %>% select(YEAR, STATENAME, employed.share, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(gender_age, gender, work.age) %>%  
  unite(gender_age_emp, gender_age, employed.share) %>%
  spread(gender_age_emp, overall) %>% select(YEAR, STATENAME, 'Male_Working Age_Not Employed') %>% 
  rename(p8.y1.raw.m = 'Male_Working Age_Not Employed')

cps.emp <- left_join(cps.emp, cps.p8.y1.m, by = c("YEAR","STATENAME")) %>%
  mutate(p8.y1.notemployed.rate.m = 100*(p8.y1.raw.m/p8.pop.m))

#p8.y1.female
cps.p8.y1.f <- cps %>% select(YEAR, STATENAME, employed.share, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(gender_age, gender, work.age) %>%  
  unite(gender_age_emp, gender_age, employed.share) %>%
  spread(gender_age_emp, overall) %>% select(YEAR, STATENAME, 'Female_Working Age_Not Employed') %>% 
  rename(p8.y1.raw.f = 'Female_Working Age_Not Employed')

cps.emp <- left_join(cps.emp, cps.p8.y1.f, by = c("YEAR","STATENAME")) %>%
  mutate(p8.y1.notemployed.rate.f = 100*(p8.y1.raw.f/p8.pop.female))

#for now not doing y2-y4 unless needed



#marriage and cohabitation rates
cps.p8.marry.m <- cps %>% select(YEAR, STATENAME, marry, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(gender_age, gender, work.age) %>%  unite(gender_age_marry, gender_age, marry) %>%
  spread(gender_age_marry, overall) %>% select(YEAR, STATENAME, 'Male_Working Age_Yes') %>% 
  rename(p8.marry.raw.m = 'Male_Working Age_Yes') 

cps.emp <- left_join(cps.emp, cps.p8.marry.m, by = c("YEAR","STATENAME")) %>%
  mutate(p8.marriage.rate.m = 100*(p8.marry.raw.m/p8.pop.m))

cps.p8.marry.f <- cps %>% select(YEAR, STATENAME, marry, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(gender_age, gender, work.age) %>%  unite(gender_age_marry, gender_age, marry) %>%
  spread(gender_age_marry, overall) %>% select(YEAR, STATENAME, 'Female_Working Age_Yes') %>% 
  rename(p8.marry.raw.female = 'Female_Working Age_Yes') 

cps.emp <- left_join(cps.emp, cps.p8.marry.f, by = c("YEAR","STATENAME")) %>%
  mutate(p8.marriage.rate.female = 100*(p8.marry.raw.female/p8.pop.female))

#p8 education overall
#overall p8 education
cps.p8.degree <- cps %>% select(YEAR, STATENAME, degree, work.age, WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, work.age) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_degree, work.age, degree) %>%
  spread(age_degree, overall) %>% select(YEAR, STATENAME, 'Working Age_Yes') %>% rename(p8.degree.raw = 'Working Age_Yes') 

cps.emp <- left_join(cps.emp, cps.p8.degree, by = c("YEAR","STATENAME")) %>%
  mutate(p8.degree.rate = 100*(p8.degree.raw/p8.pop))

#p8 gender-specific education rates
#p8 education male
cps.p8.degree.m <- cps %>% select(YEAR, STATENAME, degree, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, work.age, gender) %>% unite(age_gen_degree, age_gen, degree) %>%
  spread(age_gen_degree, overall) %>% select(YEAR, STATENAME, 'Working Age_Male_Yes') %>% rename(p8.degree.raw.m = 'Working Age_Male_Yes') 

cps.emp <- left_join(cps.emp, cps.p8.degree.m, by = c("YEAR","STATENAME")) %>%
  mutate(p8.degree.rate.m = 100*(p8.degree.raw.m/p8.pop.m))

#p8 education female
cps.p8.degree.f <- cps %>% select(YEAR, STATENAME, degree, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, work.age, gender) %>% unite(age_gen_degree, age_gen, degree) %>%
  spread(age_gen_degree, overall) %>% select(YEAR, STATENAME, 'Working Age_Female_Yes') %>% rename(p8.degree.raw.female = 'Working Age_Female_Yes') 

cps.emp <- left_join(cps.emp, cps.p8.degree.f, by = c("YEAR","STATENAME")) %>%
  mutate(p8.degree.rate.female = 100*(p8.degree.raw.female/p8.pop.female))

#############################
### Reconnect State FIPS
############################

#reconnect numerical FIPS code
states <- read.csv(file="fips.csv", header=T)
states <- states[1:75,] #removing error NA case at end of data
states$STATEFIP <- states$ï..STATEFIP
states$ï..STATEFIP <- NULL
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
  select(ï..State, Year, pctexfel, pctexpris, blkpctexfel, blkpctexpris) %>% rename(STATENAME=ï..State, YEAR=Year) %>% 
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

cps.emp <- left_join(cps.emp, ssi, by=c("YEAR","STATENAME")) %>% mutate(ssi.rate = 100*((ssi.rec)/population.16)) #check here (Sarah)

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


write.csv(cps.emp, file="cps_overall_pre_asec.csv")
rm(csp.subset)

################################################################################
# ASEC Disability Variables

#making ASEC supplement data
asec <- read.csv(file = "cps_asec.csv", header=T) %>% select(YEAR, CPSID, STATEFIP, ASECWT, INCSSI, INCDISAB, DISABWRK,
                                                             AGE, SEX, RACE, EMPSTAT, EDUC)

###########
#recodes
############

#prime-age binary
asec$prime <- ifelse(asec$AGE>=25 & asec$AGE<=54, "Prime", "Non-Prime")

#working age binary
asec$work.age <- ifelse(asec$AGE>=18 & asec$AGE<=65, "Working Age", "Non-Working Age")

#young adult binary
asec$ya <- ifelse(asec$AGE>=18 & asec$AGE<=25, "Young-Adult", "Adult")

#p6 age designation
asec$psix <- ifelse(asec$AGE>=18 & asec$AGE<=54, "Yes", "No")

#p7 age designation
asec$pseven <- ifelse(asec$AGE>=18 & asec$AGE<=40, "Yes", "No")

#gender binary
asec$gender <- ifelse(asec$SEX==1, "Male", "Female")

#recoded race variable - black non-black
asec$race <- rep(NA, length(asec$RACE))
asec$race[asec$RACE==100] <- "White" #white
asec$race[asec$RACE==200] <- "Black" #black
asec$race[asec$RACE>200] <- "Other" #other
asec$race <- as.factor(asec$race)
table(asec$race, asec$RACE)

#education
asec$degree <- ifelse(asec$EDUC>=111, "Yes", "No")

#disable - initital recode
asec$disabin <- ifelse(asec$DISABWRK==2, "Yes", "No") #1988 onwards

#adding in state names
states <- read.csv(file="fips.csv", header=T, stringsAsFactors = F)
states <- states[1:75,] #removing error NA case at end of data
states <- states %>% rename(STATEFIP = ï..STATEFIP) 
asec <- left_join(asec, states, by="STATEFIP")
rm(states)

#p1 matching asec covariates

#diability  - prime age males without BA
asec.p1.dis <- asec %>% select(YEAR, STATENAME, degree, disabin, prime, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, degree, prime, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_dis, degree_age_gen, disabin) %>%
  spread(degree_age_gen_dis, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.disab.raw = No_Prime_Male_Yes)

cps.emp <- left_join(cps.emp, asec.p1.dis, by = c("YEAR","STATENAME")) %>%
  mutate(p1.disab.rate = 100*(p1.disab.raw/p1.pop))

#diability  - prime age females without BA
asec.p1.dis <- asec %>% select(YEAR, STATENAME, degree,disabin, prime, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, degree, prime, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_dis, degree_age_gen, disabin) %>%
  spread(degree_age_gen_dis, overall) %>% select(YEAR, STATENAME, No_Prime_Female_Yes) %>% rename(p1.disab.raw.female = No_Prime_Female_Yes)

cps.emp <- left_join(cps.emp, asec.p1.dis, by = c("YEAR","STATENAME")) %>%
  mutate(p1.disab.rate.female = 100*(p1.disab.raw.female/p1.pop.female))

#p2 matching asec covariates

#diability
asec.p2.dis <- asec %>% select(YEAR, STATENAME, degree,disabin, work.age, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, degree, work.age, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_dis, degree_age_gen, disabin) %>%
  spread(degree_age_gen_dis, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.disab.raw = 'No_Working Age_Male_Yes')

#disability rate - working age age males without BA
cps.emp <- left_join(cps.emp, asec.p2.dis, by = c("YEAR","STATENAME")) %>%
  mutate(p2.disab.rate = 100*(p2.disab.raw/p2.pop))


#p3 male and female disability


#diability
asec.p3.dis <- asec %>% select(YEAR, STATENAME, disabin, prime, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, prime, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_dis, age_gen, disabin) %>%
  spread(age_gen_dis, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.disab.raw = Prime_Male_Yes)

#disability rate - prime age males 
cps.emp <- left_join(cps.emp, asec.p3.dis, by = c("YEAR","STATENAME")) %>%
  mutate(p3.disab.rate = 100*(p3.disab.raw/p3.pop))

#p3.female.diability
asec.p3.dis.f <- asec %>% select(YEAR, STATENAME, disabin, prime, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, prime, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_dis, age_gen, disabin) %>%
  spread(age_gen_dis, overall) %>% select(YEAR, STATENAME, Prime_Female_Yes) %>% rename(p3.disab.raw.female = Prime_Female_Yes)

#disability rate - prime age females 
cps.emp <- left_join(cps.emp, asec.p3.dis.f, by = c("YEAR","STATENAME")) %>%
  mutate(p3.disab.rate.female = 100*(p3.disab.raw.female/p3.pop.female))



#p4 matching asec covariates

#diability
asec.p4.dis <- asec %>% select(YEAR, STATENAME, disabin, prime, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, prime) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_dis, prime, disabin) %>%
  spread(age_dis, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.disab.raw = Prime_Yes)

#disability rate - prime age overall
cps.emp <- left_join(cps.emp, asec.p4.dis, by = c("YEAR","STATENAME")) %>%
  mutate(p4.disab.rate = 100*(p4.disab.raw/p4.pop))

#p5 matching asec covariates

#diability
asec.p5.dis <- asec %>% select(YEAR, STATENAME, disabin, ya, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, ya) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_dis, ya, disabin) %>%
  spread(age_dis, overall) %>% select(YEAR, STATENAME, 'Young-Adult_Yes') %>% rename(p5.disab.raw = 'Young-Adult_Yes')

#disability rate - young-adult overall
cps.emp <- left_join(cps.emp, asec.p5.dis, by = c("YEAR","STATENAME")) %>%
  mutate(p5.disab.rate = 100*(p5.disab.raw/p5.pop))

#p6 matching asec covariates

#diability
asec.p6.dis <- asec %>% select(YEAR, STATENAME, disabin, psix, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, psix) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_dis, psix, disabin) %>%
  spread(age_dis, overall) %>% select(YEAR, STATENAME, 'Yes_Yes') %>% rename(p6.disab.raw = 'Yes_Yes')

#disability rate - p6 overall
cps.emp <- left_join(cps.emp, asec.p6.dis, by = c("YEAR","STATENAME")) %>%
  mutate(p6.disab.rate = 100*(p6.disab.raw/p6.pop))

#p6 male and female disability


#diability
asec.p6.dis.m <- asec %>% select(YEAR, STATENAME, disabin, psix, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, psix, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>% unite(age_gen_dis, age_gen, disabin) %>%
  spread(age_gen_dis, overall) %>% select(YEAR, STATENAME, Yes_Male_Yes) %>% rename(p6.disab.raw.m = Yes_Male_Yes)

#disability rate - p6 males 
cps.emp <- left_join(cps.emp, asec.p6.dis.m, by = c("YEAR","STATENAME")) %>%
  mutate(p6.disab.rate.m = 100*(p6.disab.raw.m/p6.pop.m))

#p6.female.diability
asec.p6.dis.f <- asec %>% select(YEAR, STATENAME, disabin, psix, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, psix, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_gen, psix, gender) %>% unite(age_gen_dis, age_gen, disabin) %>%
  spread(age_gen_dis, overall) %>% select(YEAR, STATENAME, Yes_Female_Yes) %>% rename(p6.disab.raw.female = Yes_Female_Yes)

#disability rate - p6 females 
cps.emp <- left_join(cps.emp, asec.p6.dis.f, by = c("YEAR","STATENAME")) %>%
  mutate(p6.disab.rate.female = 100*(p6.disab.raw.female/p6.pop.female))


#p7 matching asec covariates

#diability
asec.p7.dis <- asec %>% select(YEAR, STATENAME, disabin, pseven, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, pseven) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_dis, pseven, disabin) %>%
  spread(age_dis, overall) %>% select(YEAR, STATENAME, 'Yes_Yes') %>% rename(p7.disab.raw = 'Yes_Yes')

#disability rate - p7 overall
cps.emp <- left_join(cps.emp, asec.p7.dis, by = c("YEAR","STATENAME")) %>%
  mutate(p7.disab.rate = 100*(p7.disab.raw/p7.pop))

#p7 male and female disability


#diability
asec.p7.dis.m <- asec %>% select(YEAR, STATENAME, disabin, pseven, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, pseven, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>% unite(age_gen_dis, age_gen, disabin) %>%
  spread(age_gen_dis, overall) %>% select(YEAR, STATENAME, Yes_Male_Yes) %>% rename(p7.disab.raw.m = Yes_Male_Yes)

#disability rate - p7 males 
cps.emp <- left_join(cps.emp, asec.p7.dis.m, by = c("YEAR","STATENAME")) %>%
  mutate(p7.disab.rate.m = 100*(p7.disab.raw.m/p7.pop.m))

#p7.female.diability
asec.p7.dis.f <- asec %>% select(YEAR, STATENAME, disabin, pseven, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, pseven, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_gen, pseven, gender) %>% unite(age_gen_dis, age_gen, disabin) %>%
  spread(age_gen_dis, overall) %>% select(YEAR, STATENAME, Yes_Female_Yes) %>% rename(p7.disab.raw.female = Yes_Female_Yes)

#disability rate - p7 females 
cps.emp <- left_join(cps.emp, asec.p7.dis.f, by = c("YEAR","STATENAME")) %>%
  mutate(p7.disab.rate.female = 100*(p7.disab.raw.female/p7.pop.female))


#p8 matching asec covariates

#diability
asec.p8.dis <- asec %>% select(YEAR, STATENAME, disabin, work.age, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, work.age) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_dis, work.age, disabin) %>%
  spread(age_dis, overall) %>% select(YEAR, STATENAME, 'Working Age_Yes') %>% rename(p8.disab.raw = 'Working Age_Yes')

#disability rate - p8 overall
cps.emp <- left_join(cps.emp, asec.p8.dis, by = c("YEAR","STATENAME")) %>%
  mutate(p8.disab.rate = 100*(p8.disab.raw/p8.pop))

#p8 male and female disability


#diability
asec.p8.dis.m <- asec %>% select(YEAR, STATENAME, disabin, work.age, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, work.age, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_gen, work.age, gender) %>% unite(age_gen_dis, age_gen, disabin) %>%
  spread(age_gen_dis, overall) %>% select(YEAR, STATENAME, 'Working Age_Male_Yes') %>% rename(p8.disab.raw.m = 'Working Age_Male_Yes')

#disability rate - p8 males 
cps.emp <- left_join(cps.emp, asec.p8.dis.m, by = c("YEAR","STATENAME")) %>%
  mutate(p8.disab.rate.m = 100*(p8.disab.raw.m/p8.pop.m))

#p8.female.diability
asec.p8.dis.f <- asec %>% select(YEAR, STATENAME, disabin, work.age, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, work.age, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_gen, work.age, gender) %>% unite(age_gen_dis, age_gen, disabin) %>%
  spread(age_gen_dis, overall) %>% select(YEAR, STATENAME, 'Working Age_Female_Yes') %>% rename(p8.disab.raw.female = 'Working Age_Female_Yes')

#disability rate - p8 females 
cps.emp <- left_join(cps.emp, asec.p8.dis.f, by = c("YEAR","STATENAME")) %>%
  mutate(p8.disab.rate.female = 100*(p8.disab.raw.female/p8.pop.female))



########################################
#### Writing final data
#################################

#removing district of columbia
cps.emp <- cps.emp %>% filter(STATENAME != "District of Columbia")

#number of missing cases per variable
colSums(is.na(cps.emp))

#write final csv
write.csv(cps.emp, file="FE_overall.csv") #final.data when CSP appended


