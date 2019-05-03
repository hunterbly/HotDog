get_data <- function(){

  ## Get sample data when no db conection

  df = data.table::fread("./data/stock.csv")

  return(df)
}

cal_signal <- function(df){

  data <- df %>% filter(volume != 0) %>% arrange(code, date)

  data <- data %>% mutate(open.lag1 = lag(open, n = 1),
                          open.lag2 = lag(open, n = 2),
                          close.lag1 = lag(close, n = 1),
                          close.lag2 = lag(close, n = 2))

  data <- data %>% mutate(s_bull_stick = f_bull_stick(open, close),
                          s_bear_stick = f_bear_stick(open, close),
                          s_bull_engulf = f_bull_engulf(open, close, open.lag1, close.lag1),
                          s_bear_engulf = f_bear_engulf(open, close, open.lag1, close.lag1),
                          s_bull_harami = f_bull_harami(open, close, open.lag1, close.lag1),
                          s_bear_harami = f_bear_harami(open, close, open.lag1, close.lag1),
                          s_2day_reverse_good = f_2day_reverse_good(open, close, open.lag1, close.lag1, open.lag2, close.lag2),
                          s_2day_reverse_bad = f_2day_reverse_bad(open, close, open.lag1, close.lag1, open.lag2, close.lag2),
                          s_bull_pierce = f_bull_pierce(open, close, open.lag1, close.lag1),
                          s_bear_pierce = f_bear_pierce(open, close, open.lag1, close.lag1),
                          s_hammer = f_hammer(open, close, open.lag1, close.lag1, low, high, tail_multiplier = 2, least_body_length = 0.005),
                          s_shooting_star = f_shooting_star(open, close, open.lag1, close.lag1, low, high, tail_multiplier = 2, least_body_length = 0.005))


  for(i in 1:5) {
    data <- high_return(df = data, n=i)
    data <-  low_return(df = data, n=i)
  }

  data %>% arrange(desc(date))

  return(data)
}
