library(tseries)
library(magrittr)
t_statistics <- function(n, true_phi){
  t <- c()
  for (j in 1:n){
    RWalk <- function(true_phi){
      x0 = 0
      x <- c(x0)
      for (i in 1:50) {
        w <- rnorm(1,0,1)
        new_x <- w + true_phi*tail(x,n=1)
        x <- c(x,new_x)
      }
      return(ts(x))
    }
    rw <- RWalk(true_phi)
    drw <- diff(rw)
    
    lagpad <- function(x, k) {
      if (k>0) {
        return (c(rep(NA, k), x)[1 : length(x)] );
      }
      else {
        return (c(x[(-k+1) : length(x)], rep(NA, -k)));
      }
    }
    lagged_rw <- lagpad(rw, k=1)
    lrw_nan <- na.remove(lagged_rw)
    
    model <- lm(drw ~ lrw_nan)
    t_val <- coef(summary(model))[2, "t value"]
    t <- c(t, t_val)
  }
  return(t)
}

plot_t <- function(t){
  hist(t, breaks = 20, prob=TRUE)
  lines(density(t, adjust=2), lwd = 1.4)
}

get_crit_val <- function(t){
  val <- sort(t) %>% head(500) %>% tail(1)
  return(val)
}

t <- t_statistics(n=10000, true_phi = 1);plot_t(t)
crit_val <-get_crit_val(t)
abline(v=crit_val, lty = "dotted", lwd = 1.4, col = "red");crit_val
