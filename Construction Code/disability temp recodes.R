######################################
### Temporary spot for disab asec recodes

#disable - initital recode
cps$disabin <- ifelse(cps$DISABWRK==2, "Yes", "No") #1988 onwards

#######################################
### Can't Find Work (NILF & WNLOOK 1-5)

cps$cfw <- cps$employed
cps$cfw[cps$EMPSTAT >= 30 & cps$WNLOOK==1|cps$WNLOOK==2|cps$WNLOOK==3|cps$WNLOOK==4|cps$WNLOOK==5] <- "cfw"
cps$cfw <- as.factor(cps$cfw)
table(cps$EMPSTAT, cps$cfw)
