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

create_lead_calendar <- function(n = 5, local = FALSE){

  # Create calendar
  df.calendar = tryCatch({

    sql_query("SELECT DISTINCT DATE
                             FROM STOCK
                             WHERE CODE = '00001'
                             ORDER BY DATE DESC", local)

  }, error = function(e){
    message(e)
    stop_quietly("No calendar in database")

  })


  # Rename column
  if (n > 0){
    # Shift by n day
    df.calendar = df.calendar[, data.table::shift(date, 0:n)]
    colname = paste0('date_' , 1:n)   # Create column name like date_1, date_2, date_n

  } else{

    # If n = 0, return dataframe with no shift
    return(df.calendar)

  }

  # Append back the first column (date 0), then rename columns
  colname = c('date', colname)
  colnames(df.calendar) = colname

  # To long format, order by date desc
  df.calendar.long = data.table::as.data.table(reshape2::melt(df.calendar, id.vars = c("date"), value.name = "leading") )
  df.calendar.long = df.calendar.long[order(-date)]

  # Remove variable column, remove NA in leading column
  df.calendar.long[, variable := NULL]
  df.calendar.long = df.calendar.long[!is.na(leading)]

  return(df.calendar.long)

}

check_cronjob <-function(local = FALSE){


  ## Return the latest date of records in the cronjob tables
  ##
  ## Args:
  ##  local (bool): Boolean flag to indicate whether the connection is using Local or Remote IP
  ##
  ## Returns:
  ##  df.res (Dataframe): Dataframe of latest date of cronjob tables
  ##
  ## Example:
  ##  df.res = check_cronjob(local = FALSE)

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

  # Add back table name
  df.stock[, table := 'stock']
  df.ccass[, table := 'ccass']
  df.option[, table := 'option']
  df.signal[, table := 'signal']

  # rbind, return first row, reorder column
  df.combine = do.call("rbind", list(df.stock,
                                     df.ccass,
                                     df.option,
                                     df.signal))
  df.res = df.combine[, head(.SD, 1), by=table]
  df.res[, date := as.character(date)]

  return(df.res)
}


filter_by_threshold <- function(x, positive = TRUE, threshold = 0.03){

  if(is.na(x)){
    return(NA)
  }

  if(positive == TRUE) {        # Positive signal, e.g. going up
    res = ifelse(x >= threshold, x, NA)
  } else {
    res = ifelse(x <= -threshold, x, NA)
  }

  return(res)
}
