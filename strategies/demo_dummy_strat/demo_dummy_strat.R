demo_dummy_strat <- function(
    all.data,
    symbol,
    model.set, 
    param.set
) {
  
  # Load dependenceies and run model predictions if needed
  require(dplyr)

  # Throw error if needed params are missing
  if(!hasName(param.set, 'lucky_number')) stop('params arg is missing lucky_number')
  if(missing(symbol) & length(unique(all.data$symbol)) != 1) {
    stop('symbol arg must be provided or all.data must be limited to one symbol')
  }
  
  # Pre-process data if needed
  df <- all.data
  if(!missing(symbol)) {
    df <- df[df$symbol == symbol,]
  }
  if(nrow(df) == 0) stop('no data after filtering all.data by symbol argument')
  
  # Iterate over rows, adding decisions
  decisions_df <- data.frame()
  for(i in 1:nrow(df)) {
    
    row <- df[i,]
    decision_df <- data.frame(
      date = row$date,
      symbol = row$symbol,
      decision = NA,
      order_type = NA,
      limit_price = NA,
      stop_price = NA, 
      trail_price = NA,
      trail_percent = NA,
      order_class = NA,
      take_profit = NA,
      stop_loss = NA,
      stop_limit = NA
    )
    
    # Multiply and round to avoid losing trailing zeroes
    close_val <- round(row$close*100, digits = 0) 
    close_digit <- substr(close_val, nchar(close_val), nchar(close_val))
    
    if(close_digit == param.set$lucky_number) {
      decision_df <- mutate(
        decision_df,
        decision = 'buy',
        order_type = 'market', # market, limit, stop, stop_limit, trailing_stop
        order_class = 'simple' # simple, bracket, oco, oto
      )
    } else if(close_digit == 9-param.set$lucky_number) {
      decision_df <- mutate(
        decision_df, 
        decision = 'sell',
        order_type = 'market', # market, limit, stop, stop_limit, trailing_stop
        order_class = 'simple' # simple, bracket, oco, oto
      )
    }
    
    decisions_df <- bind_rows(decisions_df, decision_df)
    last_row <- row
  
  }
  
  full_df <- df %>%
    transmute(
      strategy = 'dummy_strat', date, symbol
    ) %>% 
    inner_join(
      decisions_df,
      by = c('date', 'symbol')
    )
  
  return(full_df)
  
}
