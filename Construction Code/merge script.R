#########################################
### CPS JOIN
########################################
library (dplyr)

overall <- read.csv(file="FE_overall.csv")
white <- read.csv(file="FE_white.csv")
black <- read.csv(file="FE_black.csv")

full <- overall %>% left_join(white, by=c("STATENAME", "YEAR")) %>% left_join(black, by=c("STATENAME", "YEAR"))

write.csv(full, file="FE_final.csv")
