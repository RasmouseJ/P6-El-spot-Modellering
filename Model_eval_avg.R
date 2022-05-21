#Average model evaluation over n weeks
n <- 52
data_trans <- function(initial_data, loop_count, training){
  train <- ts(head(initial_data, -(14+7*(loop_count-1))),
              start = 1,
              frequency = 1)
  test <- ts(head(tail(initial_data, 14+7*(loop_count-1))), 14, 
             start = 730 - (14+7*(loop_count-1)) + 1,
             end = 730 - 7*(loop_count-1),
             frequency = 1)
  if (training == T) {
    return(log(train+100)) 
  } else {
    return(log(test+100))
  }
}

model_eval <- function(data, test_data){
  test_spot <- exp(test_data) - 100
  #AR
  ar <- forecast::Arima(data, order = c(1,0,0))
  #ARIMA
  ari <- forecast::Arima(data, order = c(6,1,2))
  #SARIMA
  sari_man <- forecast::Arima(data, order = c(1,1,1), seasonal = list(order=c(1,0,1),period = 7))
  #ARFIMA
  temp_diff <- fracdiff::fracdiff(data)
  frac_diff <- temp_diff$d 
  df <- fracdiff::diffseries(data, frac_diff)
  arf <- forecast::Arima(df, order = c(2,0,3))
  #SARFIMA
  sarf <- forecast::Arima(df, 
                          order = c(2,0,3), 
                          seasonal = list(
                            order = c(1,0,1), 
                            period = 7))
  #AR
  ar_f <- forecast::forecast(data, h = 14, model = ar)
  ar_ff <- ar_f$mean
  nt_ar <- exp(ar_ff) - 100
  ar <- sarima(data,1,0,0, details = F)
  ar_eval <- c(
    ar$AIC,
    ar$AICc,
    ar$BIC,
    mape(ar_f$mean, test_data),
    rmse(ar_f$mean, test_data),
    mape(nt_ar, test_spot),
    rmse(nt_ar, test_spot)
  )
  
  #ARIMA
  ari_f <- forecast::forecast(data, h=14, model = ari)
  ari_ff <- ari_f$mean
  nt_ari <- exp(ari_ff) - 100
  ari <- sarima(data,6,1,2, details = F)
  ari_eval <- c(
    ari$AIC,
    ari$AICc,
    ari$BIC,
    mape(ari_f$mean, test_data),
    rmse(ari_f$mean, test_data),
    mape(nt_ari, test_spot),
    rmse(nt_ari, test_spot)
  )
  
  #SARIMA
  sari_f <- forecast::forecast(data, h = 14, model = sari_man)
  sari_ff <- sari_f$mean
  nt_sari <- exp(sari_ff)-100
  sari <- sarima(data,1,1,1,1,0,1,7, details = F)
  sari_eval <- c(
    sari$AIC,
    sari$AICc,
    sari$BIC,
    mape(sari_f$mean, test_data),
    rmse(sari_f$mean, test_data),
    mape(nt_sari, test_spot),
    rmse(nt_sari, test_spot)
  )
  
  #ARFIMA
  df_forecast <- forecast::forecast(df, model = arf, h=14)
  forecast <- ts(fracdiff::diffseries(df_forecast$mean, d = -0.45), start = 717) + mean(data)
  nt_arf <- exp(forecast)-100
  arf <- sarima(df,2,0,3, details = F)
  arf_eval <- c(
    arf$AIC,
    arf$AICc,
    arf$BIC,
    mape(forecast, test_data),
    rmse(forecast, test_data),
    mape(nt_arf, test_spot),
    rmse(nt_arf, test_spot)
  )
  
  #SARFIMA
  dfs_forecast <- forecast::forecast(df, model = sarf, h=14)
  forecast_s <- ts(fracdiff::diffseries(dfs_forecast$mean, d = -0.45),start = 717) + mean(data)
  nt_sarf <- exp(forecast_s)-100
  sarf <- sarima(df,2,0,1,1,0,1,7, details = F)
  sarf_eval <- c(
    sarf$AIC,
    sarf$AICc,
    sarf$BIC,
    mape(forecast_s, test_data),
    rmse(forecast_s, test_data),
    mape(nt_sarf, test_spot),
    rmse(nt_sarf, test_spot)
    )
  Metode <- c("AIC", "AICc", "BIC", "MAPE", "RMSE", "T MAPE", "T RMSE")

  df <- data.frame("AR" = ar_eval, 
                   "ARIMA" = ari_eval, 
                   "SARIMA" = sari_eval, 
                   "ARFIMA" = arf_eval,
                   "SARFIMA" = sarf_eval
                   )
  return(df)
}

avg_model_eval <- function(){
  for (i in 1:n){
    data <- data_trans(daily_spot, i, T)
    test_data <- data_trans(daily_spot, i, F)
    
    try(result <- model_eval(data = data, test_data = test_data))
    if (i == 1){
      summed_matrix <- result
    } else {
      summed_matrix <- summed_matrix + result
    }
  }
  result <- 1/n*summed_matrix
  return(result)
}
avg_model_eval()