#' Obtain the closest date entry in the FRED data set
#' 
#' @param x Initial date that one wishes to match with the closest date on FRED data set.
#' @export
#' @return Return the FRED data with the date that is closest to the initial date \code{x}.
#' @examples
#' closest_date("1988-09-23")

closest_date=function(x){
  y=ymd(FRED$DATE)
  FRED[which(abs(y-ymd(x))==min(abs(y-ymd(x)))),] %>% 
    return()
  
}

#' Convert values 
#' 
#' @param x Date for the initial entry
#' @param y Value for the initial entry
#' @param z Date for the target entry
#' @export
#' @return Return the inflation incorporated price of the initial price \code{y}, given the initial date \code{x} and target date \code{z}.
#' @examples
#' convert_values("2017-03-30", 100, "1988-09-23")


# convert_values=function(initial_day, initial_value, target_day){
#   adj_initial=closest_date(initial_day)
#   adj_target=closest_date(target_day)
#   target_value=as.numeric(initial_value)*adj_target$CPIAUCSL/adj_initial$CPIAUCSL
#   return(target_value)
# }


convert_values=function(x, y, z){
  adj_initial=closest_date(x)
  adj_target=closest_date(z)
  target_value=as.numeric(y)*adj_target$CPIAUCSL/adj_initial$CPIAUCSL
  return(target_value)
}

pkg=c("lubridate", "tidyverse")
sapply(pkg, require, character=T)
FRED=read.csv("../Data/CPIAUCSL.csv", stringsAsFactors = F)
convert_values("2017-03-30", 100, "1988-09-23")