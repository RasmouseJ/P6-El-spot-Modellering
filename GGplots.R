#GGplots - run "Daily_spots.R" before this

ltrain_w <- as.numeric(window(ltrain, start = 708, end = 716))

data <- data.frame(
  date = seq(as.Date("2019/12/09"), as.Date("2019/12/31"), by = "days"),
  ar = c(ltrain_w, as.numeric(ar_ff)),
  l_ar = c(ltrain_w, as.numeric(lower_ar)),
  u_ar = c(ltrain_w, as.numeric(upper_ar)),
  ari = c(ltrain_w, as.numeric(ari_ff)),
  l_ari = c(ltrain_w, as.numeric(lower_ari)),
  u_ari = c(ltrain_w, as.numeric(upper_ari)),
  sari = c(ltrain_w, as.numeric(sari_ff)),
  l_sari = c(ltrain_w, as.numeric(lower_sari)),
  u_sari = c(ltrain_w, as.numeric(upper_sari)),
  arf = c(ltrain_w, as.numeric(forecast)),
  l_arf = c(ltrain_w, as.numeric(lower_arf)),
  u_arf = c(ltrain_w, as.numeric(upper_arf)),
  sarf = c(ltrain_w, as.numeric(forecast_s)),
  l_sarf = c(ltrain_w, as.numeric(lower_sarf)),
  u_sarf = c(ltrain_w, as.numeric(upper_sarf)),
  ltest = c(ltrain_w, ltest)
)

ggplot2::ggplot(data = data) +
  geom_line(aes(x = date, y = ar, color = "AR"), size = 1) + 
  geom_point(aes(x = date, y = ar, color = "AR")) +
  geom_line(aes(x = date, y = ari, color = "ARIMA"), size = 1) +
  geom_point(aes(x = date, y = ari, color = "ARIMA")) +
  geom_line(aes(x = date, y = sari, color = "SARIMA"), size = 1) +
  geom_point(aes(x = date, y = sari, color = "SARIMA")) +
  geom_line(aes(x = date, y = arf, color = "ARFIMA"), size = 1) +
  geom_point(aes(x = date, y = arf, color = "ARFIMA")) +
  geom_line(aes(x = date, y = sarf, color = "SARFIMA"), size = 1) +
  geom_point(aes(x = date, y = sarf, color = "SARFIMA")) +
  geom_line(data = data, aes(x = date, y = ltest, linetype = "Spot Price"), color = "gray20", size = 1) +
  geom_point(aes(x = date, y = ltest, linetype = "Spot Price"), shape = 15, size = 2) +
  labs(color = "Forecast",
       linetype = "Actual") +
  ylim(4.8, 5) + 
  xlab("Date") + 
  ylab(" ") +
  geom_vline(xintercept = as.numeric(as.Date("2019-12-17")), linetype=2, color = "grey25") + 
  annotate(geom="text",x=as.Date("2019-12-25"),
           y=4.99,size = 4.5,fontface = "bold",color = "gray25", label="Prediction",
           family = "sans") + 
  annotate(geom="text",x=as.Date("2019-12-12"),
           y=4.99,size = 4.5,fontface = "bold",color = "gray25", label="Sample")


ggplot2::ggplot(data = data) +
  geom_line(aes(x = date, y = ar, color = "AR"), size = 1) + 
  geom_point(aes(x = date, y = ar, color = "AR")) +
  geom_ribbon(aes(x = date, y=ar,ymin = l_ar, ymax = u_ar), alpha = 0.1) + 
  geom_line(data = data, aes(x = date, y = ltest, linetype = "Spot Price"), color = "gray20", size = 1) +
  geom_point(aes(x = date, y = ltest, linetype = "Spot Price"), shape = 15, size = 2) +
  labs(color = "Forecast",
       linetype = "Actual") +
  xlab("Date") + 
  ylab(" ") +
  geom_vline(xintercept = as.numeric(as.Date("2019-12-17")), linetype=2, color = "grey25") + 
  annotate(geom="text",x=as.Date("2019-12-25"),
           y=5.08,size = 4.5,fontface = "bold",color = "gray25", label="Prediction",
           family = "sans") + 
  annotate(geom="text",x=as.Date("2019-12-12"),
           y=5.08,size = 4.5,fontface = "bold",color = "gray25", label="Sample")

ggplot2::ggplot(data = data) +
  geom_point(aes(x = date, y = ari, size = "ARIMA"), color = "seagreen3") +
  geom_line(aes(x = date, y = ari, size = "ARIMA"), size = 1, color = "seagreen3") +
  geom_ribbon(aes(x = date, y=ar,ymin = l_ari, ymax = u_ari), alpha = 0.1) + 
  geom_line(data = data, aes(x = date, y = ltest, linetype = "Spot Price"), color = "gray20", size = 1) +
  geom_point(aes(x = date, y = ltest, linetype = "Spot Price"), shape = 15, size = 2) +
  labs(size = "Forecast",
       linetype = "Actual") +
  xlab("Date") + 
  ylab(" ") +
  geom_vline(xintercept = as.numeric(as.Date("2019-12-17")), linetype=2, color = "grey25") + 
  annotate(geom="text",x=as.Date("2019-12-25"),
           y=5.08,size = 4.5,fontface = "bold",color = "gray25", label="Prediction",
           family = "sans") + 
  annotate(geom="text",x=as.Date("2019-12-12"),
           y=5.08,size = 4.5,fontface = "bold",color = "gray25", label="Sample")

ggplot2::ggplot(data = data) +
  geom_point(aes(x = date, y = sari, size = "SARIMA"), color = "orchid2") +
  geom_line(aes(x = date, y = sari, size = "SARIMA"), size = 1, color = "orchid2") +
  geom_ribbon(aes(x = date, y=ar,ymin = l_sari, ymax = u_sari), alpha = 0.1) + 
  geom_line(data = data, aes(x = date, y = ltest, linetype = "Spot Price"), color = "gray20", size = 1) +
  geom_point(aes(x = date, y = ltest, linetype = "Spot Price"), shape = 15, size = 2) +
  labs(size = "Forecast",
       linetype = "Actual") +
  xlab("Date") + 
  ylab(" ") +
  geom_vline(xintercept = as.numeric(as.Date("2019-12-17")), linetype=2, color = "grey25") + 
  annotate(geom="text",x=as.Date("2019-12-25"),
           y=5.08,size = 4.5,fontface = "bold",color = "gray25", label="Prediction",
           family = "sans") + 
  annotate(geom="text",x=as.Date("2019-12-12"),
           y=5.08,size = 4.5,fontface = "bold",color = "gray25", label="Sample")

ggplot2::ggplot(data = data) +
  geom_point(aes(x = date, y = arf, size = "ARFIMA"), color = "goldenrod1") +
  geom_line(aes(x = date, y = arf, size = "ARFIMA"), size = 1, color = "goldenrod1") +
  geom_ribbon(aes(x = date, y=ar,ymin = l_ar + 0.002748689, ymax = u_ar + 0.002748689), alpha = 0.1) + 
  geom_line(data = data, aes(x = date, y = ltest, linetype = "Spot Price"), color = "gray20", size = 1) +
  geom_point(aes(x = date, y = ltest, linetype = "Spot Price"), shape = 15, size = 2) +
  labs(size = "Forecast",
       linetype = "Actual") +
  xlab("Date") + 
  ylab(" ") +
  geom_vline(xintercept = as.numeric(as.Date("2019-12-17")), linetype=2, color = "grey25") + 
  annotate(geom="text",x=as.Date("2019-12-25"),
           y=5.08,size = 4.5,fontface = "bold",color = "gray25", label="Prediction",
           family = "sans") + 
  annotate(geom="text",x=as.Date("2019-12-12"),
           y=5.08,size = 4.5,fontface = "bold",color = "gray25", label="Sample")

ggplot2::ggplot(data = data) +
  geom_point(aes(x = date, y = sarf, size = "SARIMA"), color = "deepskyblue2") +
  geom_line(aes(x = date, y = sarf, size = "SARIMA"), size = 1, color = "deepskyblue2") +
  geom_ribbon(aes(x = date, y=ar,ymin = l_sari + 0.03135052, ymax = u_sari + 0.03135052), alpha = 0.1) + 
  geom_line(data = data, aes(x = date, y = ltest, linetype = "Spot Price"), color = "gray20", size = 1) +
  geom_point(aes(x = date, y = ltest, linetype = "Spot Price"), shape = 15, size = 2) +
  labs(size = "Forecast",
       linetype = "Actual") +
  xlab("Date") + 
  ylab(" ") +
  geom_vline(xintercept = as.numeric(as.Date("2019-12-17")), linetype=2, color = "grey25") + 
  annotate(geom="text",x=as.Date("2019-12-25"),
           y=5.08,size = 4.5,fontface = "bold",color = "gray25", label="Prediction",
           family = "sans") + 
  annotate(geom="text",x=as.Date("2019-12-12"),
           y=5.08,size = 4.5,fontface = "bold",color = "gray25", label="Sample")