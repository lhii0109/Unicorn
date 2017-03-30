




FRED.CPI <- read.csv("./Data/CPIAUCSL.csv")


inflation.through.time <- function(beginning.date,ending.date){
  FRED.CPI$DATE <- ymd(FRED.CPI$DATE)
 bottom.cut <- FRED.CPI %>% filter(DATE >paste(beginning.date))
 final.cut <- bottom.cut %>% filter(DATE < paste(ending.date))
ggplot(final.cut, aes(DATE, CPIAUCSL)) + geom_line()
}

