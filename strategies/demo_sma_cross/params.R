param_set_generator <- function() {
  
  long_sma <- sample(15:25, 1)
  short_sma <- max(long_sma - sample(5:20, 1), 5)
  
  return(list(
    long_sma = long_sma,
    short_sma = short_sma
  ))

}