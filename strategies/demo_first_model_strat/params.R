param_set_generator <- function() {

  buy_threshold <- round(runif(n = 1, min = 1.005, max = 1.125), digits = 3)
  sell_threshold <- round(runif(n = 1, min = 0.950, max = buy_threshold), digits = 3)
  
  return(list(
    buy_threshold = buy_threshold,
    sell_threshold = sell_threshold
  ))

}