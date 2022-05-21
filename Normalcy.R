#Datatransformation and prewhitening of Normalcy index
#Run "Senest_spot.R" before this
library(readr)
library(magrittr)
library(lubridate)
library('forecast')
library(astsa)
library(tseries)
library(MASS)
library(xts)
library(TSA)

normalcy_raw <- 
  readr::read_csv(
    'normalcy-index.csv'
  )

normalcy_df <- normalcy_raw %>% 
  dplyr::filter(
    iso3c == "DNK"
  )

timestamps <- seq.Date(
  from = as.Date("2020/02/28"), 
  to = as.Date("2022/04/25"), 
  by = "days")

normalcy.ts <- xts(normalcy_df$overall,order.by = timestamps)
#hund <- xts(rep(100,788), order.by = timestamps)
plot(normalcy.ts, main = "Global Normalcy Index (DK)")
#lines(hund, lty = "dashed", lwd = 2, col = "gray25")

w_normalcy <- normalcy.ts["2021-10-01/2022-03-31"]

plot(w_normalcy)
n_df <- as.data.frame(w_normalcy)
normalcy <- ts(n_df$V1)
t_df <- as.data.frame(prices.ts)
prices <- ts(t_df$V1)
tempp <- as.data.frame(normalcy.ts["2021-09-30/2022-03-30"])
lagged_normalcy <- ts(tempp$V1)
ccf(prices,normalcy)


mod <- arima(normalcy, order = c(2,2,2))
prewhiten(normalcy, log(prices), x.model = mod)
fitwhite <- fitted(Arima(normalcy, model = mod))
fitwhite1 <- fitted(Arima(log(prices), model = mod))
ccf(fitwhite, fitwhite1)
