###############################################
# CPS Aggregation - black Populations
###############################################

library(ff)
library(dplyr)
library(tidyr)

####################################
#black
#########################################
### P1: Prime-age men no-BA - not employed share
#########################################


#read in cps_basic data
cps <- as.data.frame(read.csv.ffdf(file="cps_basic.csv", header=T, VERBOSE = T, first.rows=10000, next.rows=50000,
                                   sep=",", colClasses = NA)) 
class(cps)
str(cps)

cps.b <- cps %>% select(YEAR, MONTH, CPSID, STATEFIP, WTFINL, AGE, SEX, RACE, MARST, 
                        EMPSTAT, WNLOOK, EDUC) %>% filter(AGE>=16 & RACE==200) 
#write.csv(cps.w, file="cps_white.csv")
rm(cps)

#adding in state names
states <- read.csv(file="fips.csv", header=T, stringsAsFactors = F)
states <- states[1:75,] #removing error NA case at end of data
states <- states %>% rename(STATEFIP = ï..STATEFIP) 
cps <- left_join(cps.b, states, by="STATEFIP")
rm(states)
rm(cps.b)

#############################
### Demographic Recodes
############################

#prime-age binary
cps$prime <- ifelse(cps$AGE>=25 & cps$AGE<=54, "Prime", "Non-Prime")

#working age binary
cps$work.age <- ifelse(cps$AGE>=18 & cps$AGE<=65, "Working Age", "Non-Working Age")

#p6 age designation
cps$psix <- ifelse(cps$AGE>=18 & cps$AGE<=54, "Yes", "No")

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

#renaming for code fit purposes
cps.b <- cps
rm(cps)

#base white dataset
cps.emp <- cps.b %>% select(YEAR, STATENAME, WTFINL) %>%
  group_by(YEAR, STATENAME) %>% summarize(population.16.b=sum(WTFINL, na.rm=T))

#p1
cps.p1.b <- cps.b %>% select(YEAR, STATENAME, degree, prime, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% spread(degree_age_gen, overall) %>%
  rename(p1.pop.b=No_Prime_Male) %>% select(YEAR, STATENAME,p1.pop.b)

cps.emp <- left_join(cps.emp, cps.p1.b, by = c("YEAR","STATENAME"))


#p1.y1
cps.p1.y1.b <- cps.b %>% select(YEAR, STATENAME, degree,employed.share, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed.share) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Not Employed') %>% 
  rename(p1.y1.raw.b = 'No_Prime_Male_Not Employed')

cps.emp <- left_join(cps.emp, cps.p1.y1.b, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y1.notemployed.rate.b = 100*(p1.y1.raw.b/p1.pop.b))

#p1.y2 
cps.p1.y2.b <- cps.b %>% select(YEAR, STATENAME, degree, employed, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Employed', 'No_Prime_Male_Unemployed' ) %>% 
  mutate(p1.y2.unemployed.rate.b = 100*(No_Prime_Male_Unemployed/(No_Prime_Male_Employed+No_Prime_Male_Unemployed))) %>%
  rename(p1.y2.raw.b = No_Prime_Male_Unemployed) %>%
  select(YEAR, STATENAME, p1.y2.raw.b, p1.y2.unemployed.rate.b) 


cps.emp <- left_join(cps.emp, cps.p1.y2.b, by = c("YEAR","STATENAME"))

#p1.y3
cps.p1.y3.b <- cps.b %>% select(YEAR, STATENAME, degree,idle, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, idle) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_Idle') %>% 
  rename(p1.y3.raw.b = 'No_Prime_Male_Idle') 

cps.emp <- left_join(cps.emp, cps.p1.y3.b, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y3.idle.rate.b = 100*(p1.y3.raw.b/p1.pop.b))


#p1.y4
cps.p1.y4.b <- cps.b %>% select(YEAR, STATENAME, degree,cfw, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, cfw) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Prime_Male_cfw') %>% 
  rename(p1.y4.raw.b = 'No_Prime_Male_cfw')


cps.emp <- left_join(cps.emp, cps.p1.y4.b, by = c("YEAR","STATENAME")) %>%
  mutate(p1.y4.cfw.rate.b = 100*(p1.y4.raw.b/p1.pop.b))



#marriage and cohabitation rates
cps.p1.marry.b <- cps.b %>% select(YEAR, STATENAME, degree,marry, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_marry, degree_age_gen, marry) %>%
  spread(degree_age_gen_marry, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.marry.raw.b = No_Prime_Male_Yes) 

cps.emp <- left_join(cps.emp, cps.p1.marry.b, by = c("YEAR","STATENAME")) %>%
  mutate(p1.marriage.rate.b = 100*(p1.marry.raw.b/p1.pop.b))

#cps.p1.cohab.b <- cps.b %>% select(YEAR, STATENAME, degree,cohab, prime, gender, WTFINL) %>% 
# group_by(YEAR, STATENAME, cohab, degree, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
#unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_cohab, degree_age_gen, cohab) %>%
#spread(degree_age_gen_cohab, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.cohab.raw.b = No_Prime_Male_Yes)

#cohab rate - prime age males without BA
#cps.emp <- left_join(cps.emp, cps.p1.cohab.b, by = c("YEAR","STATENAME")) %>%
# mutate(p1.cohab.rate.b = 100*(p1.cohab.raw.b/p1.pop.b))


#########################
###  P2: Working-age male no-BA 
#########################

#p2
cps.p2.b <- cps.b %>% select(YEAR, STATENAME, degree, work.age, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% spread(degree_age_gen, overall) %>%
  rename(p2.pop.b='No_Working Age_Male') %>% select(YEAR, STATENAME,p2.pop.b)

cps.emp <- left_join(cps.emp, cps.p2.b, by = c("YEAR","STATENAME"))


#p2.y1
cps.p2.y1.b <- cps.b %>% select(YEAR, STATENAME, degree,employed.share, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed.share) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Not Employed') %>% 
  rename(p2.y1.raw.b = 'No_Working Age_Male_Not Employed')

cps.emp <- left_join(cps.emp, cps.p2.y1.b, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y1.notemployed.rate.b = 100*(p2.y1.raw.b/p2.pop.b))

#p2.y2 
cps.p2.y2.b <- cps.b %>% select(YEAR, STATENAME, degree, employed, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, employed) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Employed', 'No_Working Age_Male_Unemployed') %>%
  rename(p2.y2.raw.b = 'No_Working Age_Male_Unemployed', p2.y2.raw.e.b = 'No_Working Age_Male_Employed') %>%
  mutate(p2.y2.unemployed.rate.b = 100*(p2.y2.raw.b/(p2.y2.raw.b+p2.y2.raw.e.b))) %>%
  select(YEAR, STATENAME, p2.y2.raw.b, p2.y2.unemployed.rate.b)

cps.emp <- left_join(cps.emp, cps.p2.y2.b, by = c("YEAR","STATENAME"))

#p2.y3
cps.p2.y3.b <- cps.b %>% select(YEAR, STATENAME, degree,idle, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, idle) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Idle') %>% 
  rename(p2.y3.raw.b = 'No_Working Age_Male_Idle')

cps.emp <- left_join(cps.emp, cps.p2.y3.b, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y3.idle.rate.b = 100*(p2.y3.raw.b/p2.pop.b))



#p2.y4
cps.p2.y4.b <- cps.b %>% select(YEAR, STATENAME, degree,cfw, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>%
  unite(degree_age_gen_emp, degree_age_gen, cfw) %>%
  spread(degree_age_gen_emp, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_cfw') %>% 
  rename(p2.y4.raw.b = 'No_Working Age_Male_cfw')

cps.emp <- left_join(cps.emp, cps.p2.y4.b, by = c("YEAR","STATENAME")) %>%
  mutate(p2.y4.cfw.rate.b = 100*(p2.y4.raw.b/p2.pop.b))



#marriage and cohabitation rates
cps.p2.marry.b <- cps.b %>% select(YEAR, STATENAME, degree,marry, work.age, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, degree, work.age, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_marry, degree_age_gen, marry) %>%
  spread(degree_age_gen_marry, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.marry.raw.b = 'No_Working Age_Male_Yes') 

cps.emp <- left_join(cps.emp, cps.p2.marry.b, by = c("YEAR","STATENAME")) %>%
  mutate(p2.marriage.rate.b = 100*(p2.marry.raw.b/p2.pop.b))

#cps.p2.cohab.b <- cps.b %>% select(YEAR, STATENAME, degree,cohab, work.age, gender, WTSUPP) %>% 
# group_by(YEAR, STATENAME, cohab, degree, work.age, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
#unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_cohab, degree_age_gen, cohab) %>%
#spread(degree_age_gen_cohab, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.cohab.raw.b = 'No_Working Age_Male_Yes')

#cps.emp <- left_join(cps.emp, cps.p2.cohab.b, by = c("YEAR","STATENAME")) %>%
# mutate(p2.cohab.rate.b = 100*(p2.cohab.raw.b/p2.pop.b))


#########################
###  P3: prime-age all men
#########################

#p3
cps.p3.b <- cps.b %>% select(YEAR, STATENAME, prime, gender,WTFINL) %>% 
  group_by(YEAR, STATENAME, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% spread(age_gen, overall) %>%
  rename(p3.pop.b=Prime_Male) %>% select(YEAR, STATENAME,p3.pop.b)

cps.emp <- left_join(cps.emp, cps.p3.b, by = c("YEAR","STATENAME"))


#p3.y1
cps.p3.y1.b <- cps.b %>% select(YEAR, STATENAME, employed.share, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, employed.share) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Not Employed') %>% 
  rename(p3.y1.raw.b = 'Prime_Male_Not Employed')

cps.emp <- left_join(cps.emp, cps.p3.y1.b, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y1.notemployed.rate.b = 100*(p3.y1.raw.b/p3.pop.b))

#p3.y2 
cps.p3.y2.b <- cps.b %>% select(YEAR, STATENAME, employed, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, employed) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Employed', 'Prime_Male_Unemployed') %>%
  rename(p3.y2.raw.b = 'Prime_Male_Unemployed', p3.y2.raw.e.b = 'Prime_Male_Employed') %>%
  mutate(p3.y2.unemployed.rate.b = 100*(p3.y2.raw.b/(p3.y2.raw.b+p3.y2.raw.e.b))) %>%
  select(YEAR, STATENAME, p3.y2.raw.b, p3.y2.unemployed.rate.b) 

cps.emp <- left_join(cps.emp, cps.p3.y2.b, by = c("YEAR","STATENAME"))

#p3.y3
cps.p3.y3.b <- cps.b %>% select(YEAR, STATENAME, idle, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, idle) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_Idle') %>% 
  rename(p3.y3.raw.b = 'Prime_Male_Idle')

cps.emp <- left_join(cps.emp, cps.p3.y3.b, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y3.idle.rate.b = 100*(p3.y3.raw.b/p3.pop.b))

#p3.y4
cps.p3.y4.b <- cps.b %>% select(YEAR, STATENAME, cfw, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>%
  unite(age_gen_emp, age_gen, cfw) %>%
  spread(age_gen_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Male_cfw') %>% 
  rename(p3.y4.raw.b = 'Prime_Male_cfw')

cps.emp <- left_join(cps.emp, cps.p3.y4.b, by = c("YEAR","STATENAME")) %>%
  mutate(p3.y4.cfw.rate.b = 100*(p3.y4.raw.b/p3.pop.b))




#marriage and cohabitation rates
cps.p3.marry.b <- cps.b %>% select(YEAR, STATENAME, marry, prime, gender, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, prime, gender) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_marry, age_gen, marry) %>%
  spread(age_gen_marry, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.marry.raw.b = Prime_Male_Yes) 

cps.emp <- left_join(cps.emp, cps.p3.marry.b, by = c("YEAR","STATENAME")) %>%
  mutate(p3.marriage.rate.b = 100*(p3.marry.raw.b/p3.pop.b))

#cps.p3.cohab.b <- cps.b %>% select(YEAR, STATENAME, cohab, prime, gender, WTSUPP) %>% 
# group_by(YEAR, STATENAME, cohab, prime, gender) %>% summarize(overall=sum(WTSUPP, na.rm=T)) %>% 
#unite(age_gen, prime, gender) %>% unite(age_gen_cohab, age_gen, cohab) %>%
#spread(age_gen_cohab, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.cohab.raw.b = Prime_Male_Yes)

#cohab rate - prime age males without BA
#cps.emp <- left_join(cps.emp, cps.p3.cohab.b, by = c("YEAR","STATENAME")) %>%
# mutate(p3.cohab.rate.b = 100*(p3.cohab.raw.b/p3.pop.b))



#########################
### p4: Prime-age all  
#########################

#p4
cps.p4.b <- cps.b %>% select(YEAR, STATENAME, prime,WTFINL) %>% 
  group_by(YEAR, STATENAME, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  spread(prime, overall) %>%
  rename(p4.pop.b=Prime) %>% select(YEAR, STATENAME,p4.pop.b)

cps.emp <- left_join(cps.emp, cps.p4.b, by = c("YEAR","STATENAME"))


#p4.y1
cps.p4.y1.b <- cps.b %>% select(YEAR, STATENAME, employed.share, prime, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, prime, employed.share) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Not Employed') %>% 
  rename(p4.y1.raw.b = 'Prime_Not Employed')

cps.emp <- left_join(cps.emp, cps.p4.y1.b, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y1.notemployed.rate.b = 100*(p4.y1.raw.b/p4.pop.b))

#p4.y2 
cps.p4.y2.b <- cps.b %>% select(YEAR, STATENAME, employed, prime, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, prime, employed) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Employed', 'Prime_Unemployed') %>%
  rename(p4.y2.raw.b = 'Prime_Unemployed', p4.y2.raw.e.b = 'Prime_Employed') %>%
  mutate(p4.y2.unemployed.rate.b = 100*(p4.y2.raw.b/(p4.y2.raw.b+p4.y2.raw.e.b))) %>%
  select(YEAR, STATENAME, p4.y2.raw.b, p4.y2.unemployed.rate.b) 

cps.emp <- left_join(cps.emp, cps.p4.y2.b, by = c("YEAR","STATENAME"))

#p4.y3
cps.p4.y3.b <- cps.b %>% select(YEAR, STATENAME, idle, prime, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, prime, idle) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_Idle') %>% 
  rename(p4.y3.raw.b = 'Prime_Idle')

cps.emp <- left_join(cps.emp, cps.p4.y3.b, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y3.idle.rate.b = 100*(p4.y3.raw.b/p4.pop.b))


#p4.y4
cps.p4.y4.b <- cps.b %>% select(YEAR, STATENAME, cfw, prime, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, prime, cfw) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Prime_cfw') %>% 
  rename(p4.y4.raw.b = 'Prime_cfw')

cps.emp <- left_join(cps.emp, cps.p4.y4.b, by = c("YEAR","STATENAME")) %>%
  mutate(p4.y4.cfw.rate.b = 100*(p4.y4.raw.b/p4.pop.b))



#marriage and cohabitation rates
cps.p4.marry.b <- cps.b %>% select(YEAR, STATENAME, marry, prime, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_marry, prime, marry) %>%
  spread(age_marry, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.marry.raw.b = Prime_Yes) 

cps.emp <- left_join(cps.emp, cps.p4.marry.b, by = c("YEAR","STATENAME")) %>%
  mutate(p4.marriage.rate.b = 100*(p4.marry.raw.b/p4.pop.b))

#cps.p4.cohab.w <- cps.w %>% select(YEAR, STATENAME, cohab, prime, WTFINL) %>% 
# group_by(YEAR, STATENAME, cohab, prime) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
#unite(age_cohab, prime, cohab) %>%
#spread(age_cohab, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.cohab.raw.w = Prime_Yes)

#cohab rate - prime age males without BA
#cps.emp <- left_join(cps.emp, cps.p4.cohab.w, by = c("YEAR","STATENAME")) %>%
# mutate(p4.cohab.rate.w = 100*(p4.cohab.raw.w/p4.pop.w))

#########################
### p6: 18-54 all  
#########################

#p6
cps.p6.b <- cps.b %>% select(YEAR, STATENAME, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  spread(psix, overall) %>%
  rename(p6.pop.b='Yes') %>% select(YEAR, STATENAME,p6.pop.b)

cps.emp <- left_join(cps.emp, cps.p6.b, by = c("YEAR","STATENAME"))


#p6.y1
cps.p6.y1.b <- cps.b %>% select(YEAR, STATENAME, employed.share, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed.share, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, psix, employed.share) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Not Employed') %>% 
  rename(p6.y1.raw.b = 'Yes_Not Employed')

cps.emp <- left_join(cps.emp, cps.p6.y1.b, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y1.notemployed.rate.b = 100*(p6.y1.raw.b/p6.pop.b))

#p6.y2 
cps.p6.y2.b <- cps.b %>% select(YEAR, STATENAME, employed, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, employed, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, psix, employed) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Employed', 'Yes_Unemployed') %>%
  rename(p6.y2.raw.b = 'Yes_Unemployed', p6.y2.raw.e = 'Yes_Employed') %>%
  mutate(p6.y2.unemployed.rate.b = 100*(p6.y2.raw.b/(p6.y2.raw.b+p6.y2.raw.e))) %>%
  select(YEAR, STATENAME, p6.y2.raw.b, p6.y2.unemployed.rate.b) 

cps.emp <- left_join(cps.emp, cps.p6.y2.b, by = c("YEAR","STATENAME"))



#p6.y3
cps.p6.y3.b <- cps.b %>% select(YEAR, STATENAME, idle, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, idle, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, psix, idle) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_Idle') %>% 
  rename(p6.y3.raw.b = 'Yes_Idle')

cps.emp <- left_join(cps.emp, cps.p6.y3.b, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y3.idle.rate.b = 100*(p6.y3.raw.b/p6.pop.b))


#p6.y4
cps.p6.y4.b <- cps.b %>% select(YEAR, STATENAME, cfw, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, cfw, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_emp, psix, cfw) %>%
  spread(age_emp, overall) %>% select(YEAR, STATENAME, 'Yes_cfw') %>% 
  rename(p6.y4.raw.b = 'Yes_cfw')

cps.emp <- left_join(cps.emp, cps.p6.y4.b, by = c("YEAR","STATENAME")) %>%
  mutate(p6.y4.cfw.rate.b = 100*(p6.y4.raw.b/p6.pop.b))




#marriage and cohabitation rates
cps.p6.marry.b <- cps.b %>% select(YEAR, STATENAME, marry, psix, WTFINL) %>% 
  group_by(YEAR, STATENAME, marry, psix) %>% summarize(overall=sum(WTFINL, na.rm=T)) %>% 
  unite(age_marry, psix, marry) %>%
  spread(age_marry, overall) %>% select(YEAR, STATENAME, 'Yes_Yes') %>% rename(p6.marry.raw.b = 'Yes_Yes') 

cps.emp <- left_join(cps.emp, cps.p6.marry.b, by = c("YEAR","STATENAME")) %>%
  mutate(p6.marriage.rate.b = 100*(p6.marry.raw.b/p6.pop.b))





######################################################################################################
# ASEC Variables for black population
#####################################################################################################
#making ASEC supplement data
asec <- read.csv(file = "cps_asec.csv", header=T) %>% select(YEAR, CPSID, STATEFIP, ASECWT, INCSSI, INCDISAB, DISABWRK,
                                                             AGE, SEX, RACE, EMPSTAT, EDUC) %>% filter(AGE>=16 & RACE==200) 

###########
#recodes
############

#prime-age binary
asec$prime <- ifelse(asec$AGE>=25 & asec$AGE<=54, "Prime", "Non-Prime")

#working age binary
asec$work.age <- ifelse(asec$AGE>=18 & asec$AGE<=65, "Working Age", "Non-Working Age")

#p6 age designation
asec$psix <- ifelse(asec$AGE>=18 & asec$AGE<=54, "Yes", "No")

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
asec.p1.dis.b <- asec %>% select(YEAR, STATENAME, degree,disabin, prime, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, degree, prime, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(degree_age, degree, prime) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_dis, degree_age_gen, disabin) %>%
  spread(degree_age_gen_dis, overall) %>% select(YEAR, STATENAME, No_Prime_Male_Yes) %>% rename(p1.disab.raw.b = No_Prime_Male_Yes)

cps.emp <- left_join(cps.emp, asec.p1.dis.b, by = c("YEAR","STATENAME")) %>%
  mutate(p1.disab.rate.b = 100*(p1.disab.raw.b/p1.pop.b))



#p2 matching asec covariates

#diability
asec.p2.dis.b <- asec %>% select(YEAR, STATENAME, degree,disabin, work.age, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, degree, work.age, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(degree_age, degree, work.age) %>%  unite(degree_age_gen, degree_age, gender) %>% unite(degree_age_gen_dis, degree_age_gen, disabin) %>%
  spread(degree_age_gen_dis, overall) %>% select(YEAR, STATENAME, 'No_Working Age_Male_Yes') %>% rename(p2.disab.raw.b = 'No_Working Age_Male_Yes')

#disability rate - prime age males without BA
cps.emp <- left_join(cps.emp, asec.p2.dis.b, by = c("YEAR","STATENAME")) %>%
  mutate(p2.disab.rate.b = 100*(p2.disab.raw.b/p2.pop.b))





#diability
asec.p3.dis.b <- asec %>% select(YEAR, STATENAME, disabin, prime, gender, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, prime, gender) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_gen, prime, gender) %>% unite(age_gen_dis, age_gen, disabin) %>%
  spread(age_gen_dis, overall) %>% select(YEAR, STATENAME, Prime_Male_Yes) %>% rename(p3.disab.raw.b = Prime_Male_Yes)

#disability rate - prime age males without BA
cps.emp <- left_join(cps.emp, asec.p3.dis.b, by = c("YEAR","STATENAME")) %>%
  mutate(p3.disab.rate.b = 100*(p3.disab.raw.b/p3.pop.b))


#p4 matching asec covariates

#diability
asec.p4.dis.b <- asec %>% select(YEAR, STATENAME, disabin, prime, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, prime) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_dis, prime, disabin) %>%
  spread(age_dis, overall) %>% select(YEAR, STATENAME, Prime_Yes) %>% rename(p4.disab.raw.b = Prime_Yes)

#disability rate - prime age males without BA
cps.emp <- left_join(cps.emp, asec.p4.dis.b, by = c("YEAR","STATENAME")) %>%
  mutate(p4.disab.rate.b = 100*(p4.disab.raw.b/p4.pop.b))

#p6 matching asec covariates

#diability
asec.p6.dis.b <- asec %>% select(YEAR, STATENAME, disabin, psix, ASECWT) %>% 
  group_by(YEAR, STATENAME, disabin, psix) %>% summarize(overall=sum(ASECWT, na.rm=T)) %>% 
  unite(age_dis, psix, disabin) %>%
  spread(age_dis, overall) %>% select(YEAR, STATENAME, 'Yes_Yes') %>% rename(p6.disab.raw.b = 'Yes_Yes')

#disability rate - prime age males without BA
cps.emp <- left_join(cps.emp, asec.p6.dis.b, by = c("YEAR","STATENAME")) %>%
  mutate(p6.disab.rate.b = 100*(p6.disab.raw.b/p6.pop.b))



########################################
#### Writing final data
#################################

#removing district of columbia
cps.emp <- cps.emp %>% filter(STATENAME != "District of Columbia")

#number of missing cases per variable
colSums(is.na(cps.emp))

#write final csv
write.csv(cps.emp, file="FE_black.csv") #final.data when CSP appended





