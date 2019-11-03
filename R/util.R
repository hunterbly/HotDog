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

create_lead_calendar <- function(n = 5){

  # Create calendar
  df.calendar = tryCatch({

    sql_query("SELECT DISTINCT DATE
                             FROM STOCK
                             WHERE CODE = '00001'
                             ORDER BY DATE DESC", local)

  }, error = function(e){

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

  # To long format, order
  df.calendar.long = data.table::as.data.table(reshape2::melt(df.calendar, id.vars = c("date"), value.name = "leading") )
  df.calendar.long = df.calendar.long[order(-date)]

  # Remove variable column
  df.calendar[variable := NULL]

  return(df.calendar)

}
