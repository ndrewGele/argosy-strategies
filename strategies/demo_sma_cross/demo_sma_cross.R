demo_sma_cross <- function(
    all.data,
    symbol,
    model.set, 
    param.set
) {
  
  # Load dependenceies and run model predictions if needed
  require(dplyr)
  require(TTR)
  
  # Throw error if needed params are missing
  if(!hasName(param.set, 'long_sma')) stop('params arg is missing long_sma')
  if(!hasName(param.set, 'short_sma')) stop('params arg is missing short_sma')
  if(missing(symbol) & length(unique(all.data$symbol)) != 1) {
    stop('symbol arg must be provided or all.data must be limited to one symbol')
  }
  
  # Pre-process data if needed
  df <- all.data
  if(!missing(symbol)) {
    df <- df[df$symbol == symbol,]
  }
  if(nrow(df) == 0) stop('no data after filtering all.data by symbol argument')
  df <- df %>% 
    mutate(
      long_sma = TTR::SMA(close, n = param.set$long_sma),
      short_sma = TTR::SMA(close, n = param.set$short_sma),
      sma_diff = coalesce(long_sma - short_sma, 0)
    )
  
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
    
    if(is.na(row$long_sma)) {}
    else if(is.null(last_row)) {}
    else if(is.na(last_row$long_sma)) {}
    else if(row$sma_diff < 0 & last_row$sma_diff >= 0) {
      decision_df <- decision_df %>% 
        mutate(
          decision = 'buy',
          order_type = 'market', # market, limit, stop, stop_limit, trailing_stop
          order_class = 'simple' # simple, bracket, oco, oto
        )
    }
    else if(row$sma_diff > 0 & last_row$sma_diff <= 0) {
      decision_df <- decision_df %>% 
        mutate(
          decision = 'sell',
          order_type = 'market', # market, limit, stop, stop_limit, trailing_stop
          order_class = 'simple' # simple, bracket, oco, oto
        )
    }
    else {}
    
    decisions_df <- bind_rows(decisions_df, decision_df)
    last_row <- row
    
  }  
  
  full_df <- df %>%
    transmute(
      strategy = 'sma_cross', date, symbol
    ) %>% 
    inner_join(
      decisions_df,
      by = c('date', 'symbol')
    )
  
  return(full_df)
  
}
