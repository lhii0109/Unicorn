# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


FRED.CPI <- read.csv("./Data/CPIAUCSL.csv")


inflation.through.time <- function(x,y){
  FRED.CPI$DATE <- ymd(FRED.CPI$DATE)
 bottom.cut <- FRED.CPI %>% filter(DATE >paste(x))
 final.cut <- bottom.cut %>% filter(DATE < paste(y))
ggplot(final.cut, aes(DATE, CPIAUCSL)) + geom_line()
}

