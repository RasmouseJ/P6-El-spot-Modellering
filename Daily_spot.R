library(readr)
library(magrittr)
library(lubridate)
library('forecast')
library(astsa)
library(tseries)
library(MASS)
library(xts)
library(ggplot2)

dat <- 
  readr::read_csv(
    'spotpriser18-19.csv'
  )
time_index <- seq(from = as.POSIXct("2018-01-01 00:00"), 
                  to = as.POSIXct("2019-12-17 23:00"), by = "hour")
dat.xts <- xts(dat$SpotPriceEUR, order.by = time_index)
plot(dat.xts, main = "Hourly Electricity Spot Prices", ylab = "EUR/MWh")

daily_avg_ts <- function(x){
  transformed_data <- x %>% 
    dplyr::mutate(
      ID = rev(seq.int(nrow(dat))),
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

timestamps <- seq(as.Date("2018/1/1"), as.Date("2019/12/17"), by = "days")
daily_spot <- daily_avg_ts(dat)
daily_spot.xts <- xts(daily_spot, order.by = timestamps)
plot(daily_spot.xts, main = "Daily Average Electricity Spot Prices (DK1)", ylab = "EUR/MWh")

train_spot <- ts(head(daily_spot, -14),
                 start = 1,
                 end = 716,
                 frequency = 1)
train_spot.xts <- xts(train_spot, order.by = timestamps)
plot(train_spot.xts, main = "Daily Average Electricity Spot Prices (DK1)", ylab = "EUR/MWh")
test_spot <- ts(tail(daily_spot, 14), 
                start = 717,
                end = 730,
                frequency = 1)

ltrain <- log(train_spot + 100)
ltrain.xts <- xts(ltrain, order.by = timestamps)
plot(ltrain.xts, main = "Log-transformed Daily Average Electricity Spot Prices")
ltest <- log(test_spot + 100)

##### Funktioner -------------------------------------------------------------
mape <- function(pred,act){
  n = length(pred)
  result <- 1/n*sum(abs((as.numeric(act) - as.numeric(pred))/as.numeric(act)))
  return(result*100)
}

rmse <- function(pred, act){
  n = length(pred)
  result <- 1/n*sqrt(sum((as.numeric(pred)-as.numeric(act))^2))
  return(result)
}

forecasting <- function(mod, act){
  f <- forecast::forecast(mod, h=14)
  autoplot(f,xlim = c(690,730)) + 
    forecast::autolayer(ltest, series = "Data") +
    autolayer(f$mean, series = "Forecast")
  diff <- ts(
    c(tail(ltrain,1),
      head(f$mean,1)),
    start = 716
  )
  lines(diff)
  return(f$mean)
}

vartime <- function(x, nmax = round(length(x) / 10)) {
  v <- rep(NA, nmax);
  for (n in 1:nmax) {
    y <- filter(x, rep(1/n, n), sides = 1);
    v[n] <- var(y, na.rm = TRUE);
  }
  return(v)
}

fracdiff <- function (x , d ){
  iT <- length ( x )
  np2 <- nextn (2* iT - 1 , 2)
  k <- 1:( iT -1)
  b <- c (1 , cumprod (( k - d - 1)/ k ))
  dx <- fft ( fft ( c (b , rep (0 , np2 - iT )))*
                fft ( c (x , rep (0 , np2 - iT ))) , inverse = T )/ np2 ;
  return ( Re ( dx [1: iT ]))
}

##### Modeller ---------------------------------------------------------------


#AR
ar <- sarima(ltrain,1,0,0)
ar <- forecast::Arima(ltrain, order = c(1,0,0))
#ARIMA
ari <- sarima(ltrain,6,1,2)
ari <- forecast::Arima(ltrain, order = c(6,1,2))

#SARIMA auto
sari_auto <- forecast::Arima(ltrain, order = c(1,1,2), seasonal = list(order=c(0,0,2),period = 7))
#SARIMA manuel
sari_man <- forecast::Arima(ltrain, order = c(1,1,1), seasonal = list(order=c(1,0,1),period = 7))
sarima(ltrain,1,1,1,1,0,1,7)
#ARFIMA
df <- fracdiff::diffseries(ltrain, 0.45)
arf <- forecast::Arima(df, order = c(2,0,3))
sarima(df,2,0,3)
#SARFIMA
sarf <- sarima(df,2,0,1,1,0,1,7)
sarf <- forecast::Arima(df, 
                        order = c(2,0,3), 
                        seasonal = list(
                          order = c(1,0,1), 
                          period = 7))


##### Forecasting ------------------------------------------------------------

#AR
ar_f <- forecast::forecast(ltrain, h = 14, model = ar)
ar_ff <- ar_f$mean
nt_ar <- exp(ar_ff) - 100
mape(ar_f$mean, ltest)
rmse(ar_f$mean, ltest)
mape(nt_ar, test_spot)
rmse(nt_ar, test_spot)

ar_upper <- ar_f$upper
upper_df1 <- as.data.frame(ar_upper)
upper_ar <- upper_df1$`95%`
ar_lower <- ar_f$lower
lower_df1 <- as.data.frame(ar_lower1)
lower_ar <- lower_df$`95%`

#ARIMA
ari_f <- forecast::forecast(ltrain, h=14, model = ari)
ari_ff <- ari_f$mean
nt_ari <- exp(ari_ff) - 100 
mape(ari_f$mean, ltest)
rmse(ari_f$mean, ltest)
mape(nt_ari, test_spot)
rmse(nt_ari, test_spot)

ari_upper <- ari_f$upper
upper_df2 <- as.data.frame(ari_upper)
upper_ari <- upper_df2$`95%`
ari_lower <- ari_f$lower
lower_df2 <- as.data.frame(ari_lower)
lower_ari <- lower_df2$`95%`


#SARIMA
sari_f <- forecast::forecast(ltrain, h = 14, model = sari_man)
sari_ff <- sari_f$mean
nt_sari <- exp(sari_ff)-100
mape(sari_f$mean, ltest)
rmse(sari_f$mean, ltest)
mape(nt_sari, test_spot)
rmse(nt_sari, test_spot)

sari_upper <- sari_f$upper
upper_df3 <- as.data.frame(sari_upper)
upper_sari <- upper_df3$`95%`
sari_lower <- sari_f$lower
lower_df3 <- as.data.frame(sari_lower)
lower_sari <- lower_df3$`95%`

#ARFIMA
df_forecast <- forecast::forecast(df, model = arf, h=14)
forecast <- ts(fracdiff::diffseries(df_forecast$mean, d = -0.45), start = 717) + mean(ltrain)
nt_arf <- exp(forecast)-100
mape(forecast, ltest)
rmse(forecast, ltest)
mape(nt_arf, test_spot)
rmse(nt_arf, test_spot)

dfu1 <- as.data.frame(df_forecast$upper)
upper_arf <- ts(fracdiff::diffseries(dfu1$`95%`, d = -0.45), start = 717) + mean(ltrain)
dfl1 <- as.data.frame(df_forecast$lower)
lower_arf <- ts(fracdiff::diffseries(dfl1$`95%`, d = -0.45), start = 717) + mean(ltrain)

#SARFIMA
dfs_forecast <- forecast::forecast(df, model = sarf, h=14)
forecast_s <- ts(fracdiff::diffseries(dfs_forecast$mean, d = -0.45),start = 717) + mean(ltrain)
nt_sarf <- exp(forecast_s)-100
mape(forecast_s, ltest)
rmse(forecast_s, ltest)
mape(nt_sarf, test_spot)
rmse(nt_sarf, test_spot)

dfu2 <- as.data.frame(dfs_forecast$upper)
upper_sarf <- ts(fracdiff::diffseries(dfu2$`95%`, d = -0.45), start = 717) + mean(ltrain)
dfl2 <- as.data.frame(dfs_forecast$lower)
lower_sarf <- ts(fracdiff::diffseries(dfl2$`95%`, d = -0.45), start = 717) + mean(ltrain)
