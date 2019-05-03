
f_bull_stick <- function(open.lag0, close.lag0, upper.limit = 0.05, lower.limit = 0.04)
{
  boolean <- 	ifelse(
    ((close.lag0 - open.lag0) / open.lag0) > lower.limit &
      ((close.lag0 - open.lag0) / open.lag0) < upper.limit,
    1, 0
  )

  return(boolean)
}

f_bear_stick <- function(open.lag0, close.lag0, upper.limit = -0.05, lower.limit = -0.04)
{
  boolean <- 	ifelse(
    ((close.lag0 - open.lag0) / open.lag0) < lower.limit &
      ((close.lag0 - open.lag0) / open.lag0) > upper.limit, 1, 0
  )

  return(boolean)
}

f_bull_engulf <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{

  boolean <- 	ifelse(
    ((close.lag0 > open.lag0) &
       (open.lag1 > close.lag1) &
       (close.lag0 > open.lag1) &
       (close.lag1 > open.lag0))
    , 1, 0
  )

  return(boolean)
}

f_bear_engulf <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{

  boolean <- 	ifelse(
    (close.lag0 < open.lag0) &
      (close.lag1 > open.lag1) &
      (close.lag0 < open.lag1) &
      (close.lag1 < open.lag0)
    , 1, 0
  )

  return(boolean)
}

f_bull_harami<- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
  boolean <- 	ifelse(
    (close.lag0 > open.lag0) &
      (close.lag1 < open.lag1) &
      (close.lag0 < open.lag1) &
      (close.lag1 < open.lag0)
    , 1, 0
  )

  return(boolean)
}

f_bear_harami<- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
  boolean <- 	ifelse(
    (close.lag0 < open.lag0) &
      (close.lag1 > open.lag1) &
      (close.lag0 > open.lag1) &
      (close.lag1 > open.lag0)
    , 1, 0
  )

  return(boolean)
}

f_2day_reverse_good<- function(open.lag0, close.lag0, open.lag1, close.lag1, open.lag2, close.lag2, today_up_threshold = 0.03, ytd_down_threshold = -0.03)
{
  boolean <- 	ifelse(
    (((close.lag0 - close.lag1) / close.lag1) > today_up_threshold) &
      (((close.lag1 - close.lag2) / close.lag2) < ytd_down_threshold)
    , 1, 0
  )

  return(boolean)
}

f_2day_reverse_bad<- function(open.lag0, close.lag0, open.lag1, close.lag1, open.lag2, close.lag2, today_down_threshold = -0.03, ytd_up_threshold = 0.03)
{
  boolean <- 	ifelse(
    (((close.lag0 - close.lag1) / close.lag1) < today_down_threshold) &
      (((close.lag1 - close.lag2) / close.lag2) > ytd_up_threshold)
    , 1, 0
  )

  return(boolean)
}

f_bull_pierce <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
  boolean <- 	ifelse(
    (close.lag0 > open.lag0) &
      (close.lag1 < open.lag1) &
      (open.lag0 < close.lag1) &
      (close.lag0 < open.lag1) &
      (close.lag0 >((open.lag1 + close.lag1)/2))
    , 1, 0
  )

  return(boolean)
}

f_bear_pierce <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
  boolean <- 	ifelse(
    (close.lag1 > open.lag1) &
      (open.lag0 > close.lag0) &
      (close.lag0 > open.lag1) &
      (close.lag0 < ((open.lag1 + close.lag1)/2)) &
      (open.lag0 > close.lag1)
    , 1, 0
  )

  return(boolean)
}

f_hammer <- function(open.lag0, close.lag0, open.lag1, close.lag1, low.lag0, high.lag0, tail_multiplier = 2, least_body_length = 0.005)
{
  boolean <- 	ifelse(
    (close.lag0 > open.lag0) &
      ((open.lag0 - low.lag0) >= (tail_multiplier * (close.lag0 - open.lag0))) &
      (((close.lag0 - open.lag0) / open.lag0) > least_body_length) &
      ((high.lag0 - close.lag0) <= (close.lag0 - open.lag0))
    , 1, 0
  )

  return(boolean)
}

f_shooting_star <- function(open.lag0, close.lag0, open.lag1, close.lag1, low.lag0, high.lag0, tail_multiplier = 2, least_body_length = 0.005)
{
  boolean <- 	ifelse(
    (open.lag0 > close.lag0) &
      ((high.lag0 - open.lag0) >= (tail_multiplier * (open.lag0 - close.lag0))) &
      (((close.lag0 - low.lag0) <= (open.lag0 - close.lag0))) &
      (((open.lag0 - close.lag0) / open.lag0) > least_body_length)
    , 1, 0
  )

  return(boolean)

}


high_return <- function(df, n) {
  df %>% arrange(date)
  varname <- paste("high.return.lead", n , sep=".")
  varval <- lazyeval::interp(~(lead(high, n) - close) / close, n=n)
  mutate_(df, .dots= setNames(list(varval), varname))
}

low_return <- function(df, n) {
  df %>% arrange(date)
  varname <- paste("low.return.lead", n , sep=".")
  varval <- lazyeval::interp(~(lead(low, n) - close) / close, n=n)
  mutate_(df, .dots= setNames(list(varval), varname))
}

eval_signal <- function(signal.list, df){

  df.return <- ldply(signal.list, cal_signal_strength, df)
  df.return <- t(df.return)
  colnames(df.return) <- signal.list
  rownames(df.return) <- NULL

  data.frame(df.return)


}


cal_signal_strength <- function(signalName, df, threshold = 0.03){

  out <- tryCatch({
    filter_criteria <- interp(~which_column == 1, which_column = as.name(signalName))
    df.filtered <- df %>% filter_(filter_criteria)
    signal_count <- nrow(df.filtered)

    # Calculate positive return, median & count
    df.filtered.high <- df.filtered %>% select(matches('high.return'))
    df.filtered.high.threshold <- sapply(df.filtered.high, function(x){ (y <- ifelse(x >= threshold, x, NA))})

    df.filtered.high.threshold.median <- apply(df.filtered.high.threshold,2,median,na.rm=TRUE)
    df.filtered.high.threshold.count  <- apply(df.filtered.high.threshold, 2, function(c){sum(c!=0, na.rm = TRUE)/signal_count})
    df.filtered.high.threshold.return <- rbind(df.filtered.high.threshold.median, df.filtered.high.threshold.count)
    df.filtered.high.threshold.return[] <- vapply(df.filtered.high.threshold.return, function(x){ifelse(is.na(x), 0, x)}, numeric(1))

    # Calculate negative return, median & count
    df.filtered.low <- df.filtered %>% select(matches('low.return'))
    df.filtered.low.threshold <- sapply(df.filtered.low, function(x){ (y <- ifelse(x <= -1 * threshold, x, NA))})

    df.filtered.low.threshold.median <- apply(df.filtered.low.threshold,2,median,na.rm=TRUE)
    df.filtered.low.threshold.count  <- apply(df.filtered.low.threshold, 2, function(c){sum(c!=0, na.rm = TRUE)/signal_count})
    df.filtered.low.threshold.return <- rbind(df.filtered.low.threshold.median, df.filtered.low.threshold.count)
    df.filtered.low.threshold.return[] <- vapply(df.filtered.low.threshold.return, function(x){ifelse(is.na(x), 0, x)}, numeric(1))


    #Calculate Signal Strength
    number_days	= ncol(df.filtered.high.threshold.return)
    index.strength = (abs(sum(colProds(df.filtered.high.threshold.return))) - abs(sum(colProds(df.filtered.low.threshold.return)))) * 10000 / number_days

    index.strength
  },error = function(e){
    message('Error')
    return(0)
  }
  )
  return(out)



}

get_singal_strength <- function(){



  data.signal <- data %>% nest(-code) %>% mutate(signal = map(data, ~cal_signal(.)))

  data.signal.eval <- data.signal %>% mutate(signal.eval = map(signal, ~evalSignal(signal_list, .)))

  # get evaluated signal
  signal.result <- data.signal.eval %>% select(code, signal.eval) %>% unnest(signal.eval)
}
