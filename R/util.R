stop_quietly <- function(msg) {

  ## Stop the program with custom message without traceback
  ##
  ## Args:
  ##  msg (str): Error message to be displayed in console
  ##
  ## Example:
  ##  stop_quietly("Not Implemented")

  opt <- options(error=NULL)
  on.exit(options(opt))
  stop(msg, call. = FALSE)
}

round_dataframe <- function(df, digits = 2){

  ## Round the numeric columns of the provided dataframe
  ##
  ## Args:
  ##  df (Dataframe): Dataframe to be rounded
  ##  digits (num): No of digits to be rounded
  ##
  ## Returns:
  ##  df (Dataframe): Dataframe with rounded numbers
  ##
  ## Example:
  ##  dt = as.data.table(iris)
  ##  dt = round_dataframe(df = dt, digits = 0)

  # Find numeric cols, round the columns with no of digits provided
  numeric.cols = colnames(Filter(is.numeric, df))
  df[, (numeric.cols) := round(.SD, digits), .SDcols = numeric.cols]

  return(df)
}

check_cronjob <-function(local = FALSE){

  ####
  # SQL for checking
  ####

  sql.stock = 'SELECT DATE, COUNT(1)
                FROM STOCK
                GROUP BY DATE
                ORDER BY DATE DESC
                LIMIT 10'

  sql.ccass = 'SELECT DATE, COUNT(1)
              FROM CCASS
              GROUP BY DATE
              ORDER BY DATE DESC
              LIMIT 10'

  sql.option = 'SELECT DATE, COUNT(1)
                FROM OPTION
                GROUP BY DATE
                ORDER BY DATE DESC
                LIMIT 10'

  sql.signal = 'SELECT DATE, COUNT(1)
                FROM SIGNAL_HISTORY
                GROUP BY DATE
                ORDER BY DATE DESC
                LIMIT 10'

  ####
  # Result dataframe
  ####

  df.stock  = sql_query(sql.stock,  local)
  df.ccass  = sql_query(sql.ccass,  local)
  df.option = sql_query(sql.option, local)
  df.signal = sql_query(sql.signal, local)


  print(head(df.stock))
  print(head(df.ccass))
  print(head(df.option))
  print(head(df.signal))

  return(df.stock)
}
