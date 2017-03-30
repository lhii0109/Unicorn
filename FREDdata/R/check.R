pkg=c("lubridate", "tidyverse")
sapply(pkg, require, character=T)

FRED=read.csv("../Data/CPIAUCSL.csv", stringsAsFactors = F)

closest_date=function(Date){
  x=ymd(FRED$DATE)
  FRED[which(abs(x-ymd(Date))==min(abs(x-ymd(Date)))),] %>% 
    return()
  
}

# closest_date("1988-09-09")

