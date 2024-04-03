demo_first_model_strat <- function(
    all.data,
    symbol,
    model.set,
    param.set
) {
  
  # Load dependenceies
  require(dplyr)
  require(dbplyr)
  
  require(recipes)
  require(workflows)
  
  require(glmnet)
  require(ranger)
  require(xgboost)
  
  # Throw error if needed params are missing
  if(!hasName(param.set, 'buy_threshold')) stop('params arg is missing buy_threshold')
  if(!hasName(model.set, 'next_close')) stop('models arg is missing next_close')
  if(!hasName(param.set, 'sell_threshold')) stop('params arg is missing sell_threshold')
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
      next_close = predict(model.set$next_close$model, .)$.pred %>% 
        round(digits = 3)
    )
  
  decisions_df <- df %>% 
    transmute(
      strategy = 'first_model_strat',
      date, symbol, 
      next_close, # if this isn't in transmute, case_when fails
      decision = case_when(
        next_close > param.set$buy_threshold ~ 'buy',
        next_close < param.set$sell_threshold ~ 'sell',
        TRUE ~ NA
      ),
      order_type = case_when(!is.na(decision) ~ 'market'),
      limit_price = NA,
      stop_price = NA, 
      trail_price = NA,
      trail_percent = NA,
      order_class = case_when(!is.na(decision) ~ 'simple'),
      take_profit = NA,
      stop_loss = NA,
      stop_limit = NA
    ) %>% 
    select(-next_close)
  
  return(decisions_df)
  
}
