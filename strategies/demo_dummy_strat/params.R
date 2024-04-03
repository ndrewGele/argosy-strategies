param_set_generator <- function() {

  lucky_number <- sample(0:9, 1)
  
  return(list(
    lucky_number = lucky_number
  ))

}