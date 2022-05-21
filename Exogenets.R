#Wind, temperatur and gas prices
library("readr")
library("dplyr")
library("tidyr")


##### Gas --------------------------------------------------------------------
gas <- 
  readr::read_csv(
    "ICE Dutch TTF Natural Gas Futures Historical Data (2).csv"
  ) %>% 
  dplyr::select(
    Date,
    Price
  ) %>% 
  apply(2, rev
  ) %>% 
  data.frame()
gas_ts <- ts(gas$Price)


##### Temperatur -------------------------------------------------------------
temp_df <- 
  readr::read_csv("Temp 1.oktober-31-marts.csv"
  ) %>% as.data.frame()
temp <- ts(temp_df$temp)

##### Vind -------------------------------------------------------------------
vind_df <- 
  readr::read_csv("vind 1.okt-31.marts.csv"
  ) %>% 
  dplyr::mutate(
    "vind" = gsub(";", "", `vind;`)
  )
vind <- ts(as.numeric(vind_df$vind))

vind
ccf(prices,vind)

data <- data.frame(
  Spot = as.numeric(prices),
  Normalcy = as.numeric(normalcy),
  Vind = as.numeric(vind),
  Temperatur = as.numeric(temp)
)

aic <- Inf
i <- 0
for (d in c(0:1)){
  for (p in c(0:10)){
    for (q in c(0:10)){
      try(mod <- astsa::sarima(prices, p, d, q, xreg = data.frame(vind,temp), details = F))
      i <- i+1
      print(paste0(i,"/242"))
      if (mod$AIC < aic){
        print("check")
        aic <- mod$AIC
        fin_mod <- mod
        d_fin <- d
      }
    }
  }  
}
fin_mod;d_fin

#med vind: (5,1,2)

# 
# mod_w  <- stats::arima(prices, ,order = c(7,0,4), xreg = data.frame(vind, temp))
# mod <- stats::arima(prices, ,order = c(7,0,4))
# 
# rss1 <- norm(as.numeric(mod$residuals),"2")^2
# rss2 <- norm(as.numeric(mod_w$residuals),"2")^2
# k1 <- 12
# k2 <- 14
# n <- length(prices)

