#Avg spotprices from 2021 to 2020
library(readr)
library(magrittr)
library(lubridate)
library('forecast')
library(astsa)
library(tseries)
library(MASS)
library(xts)
library(ggplot2)

dat_senest <- 
  readr::read_csv(
    'elspotsenest.csv'
  )


daily_avg_ts <- function(x){
  transformed_data <- x %>% 
    dplyr::mutate(
      ID = rev(seq.int(nrow(x))),
      Day_id = 1+floor(
        interval(
          x$HourUTC[length(x$HourUTC)],
          HourUTC)/days(1))
    ) %>% 
    dplyr::select(
      ID,
      Day_id,
      SpotPriceEUR
    ) %>% 
    dplyr::arrange(ID)%>% 
    dplyr::group_by(Day_id) %>%
    dplyr::summarise_at(
      dplyr::vars(SpotPriceEUR), 
      list(DailySpotAvg = mean)
    ) %>% 
    dplyr::mutate(
      DailySpotAvg = DailySpotAvg
    ) %>% 
    dplyr::arrange(Day_id)
  
  output <- ts(transformed_data$DailySpotAvg,
               start = 1,
               frequency = 1)
  return(output)
}

spot_senest <- daily_avg_ts(dat_senest)

timestamps <- seq(as.Date("2021/01/01"), as.Date("2022/05/03"), by = "days")
temp_prices.ts <- xts(spot_senest, order.by = timestamps)
prices.ts <- temp_prices.ts["2021-10-01/2022-03-31"]
plot(prices.ts, ylab="EUR/MWh", main = "Electricity Spot Prices (DK1)")
tempdf <- as.data.frame(prices.ts); prices <- ts(tempdf$V1)
mod <- arfima::arfima(prices, order = c(0,0,0), xreg = vind)
