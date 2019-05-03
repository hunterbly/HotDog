hotdog <- function(){
  print("Hotdog")
}

get_data <- function(){

  ## Get sample data when no db conection

  df = data.table::fread("./data/stock.csv")

  return(df)
}

get_signal <- function(df){
  data <- df %>% filter(volume != 0) %>% arrange(code, date)

  data <- data %>% mutate(open.lag1 = lag(open, n = 1),
                          open.lag2 = lag(open, n = 2),
                          close.lag1 = lag(close, n = 1),
                          close.lag2 = lag(close, n = 2))
}
