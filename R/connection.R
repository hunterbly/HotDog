sql_connection <- function(local = FALSE){

  ## Return connection for remote database
  ##
  ## Arg
  ##  local (bool): Boolean flag to indicate whether the connection is using Local or Remote IP
  ##
  ## Return
  ##  conn (PostgreSQLConnection): PSQL connection
  ##
  ## Example
  ##  conn = sql_connection
  ##  DBI::dbDisconnect(conn)

  if(local == TRUE){

    conn <- DBI::dbConnect(drv    = RPostgreSQL::PostgreSQL(),
                           dbname = "stock",
                           host   = "localhost",
                           port   = 4004,
                           user   = "db_user",
                           password = 'P@ssw0rDB')

  } else{
    # Remote
    conn <- DBI::dbConnect(drv    = RPostgreSQL::PostgreSQL(),
                           dbname = "stock",
                           host   = "206.189.149.240",
                           port   = 4004,
                           user   = "db_user",
                           password = 'P@ssw0rDB')

  }

  return(conn)
}


sql_query <- function(sql, local = FALSE){

  ## Get data from the provided sql
  ##
  ## Arg
  ##  sql (str): SQL statement
  ##  local (bool): Boolean flag to indicate whether the connection is using Local or Remote IP
  ##
  ## Return
  ##  res (Dataframe): Dataframe of the result of the query statement
  ##
  ## Example
  ##  df <- sql_query("select * from stock where date >= '2019-06-01' order by date desc")
  ##

  conn <- sql_connection(local)

  res <- as.data.frame(DBI::dbGetQuery(conn, sql))

  DBI::dbDisconnect(conn)

  return(res)

}


####
# Function interfaces
####



####
# Not implemented
####


get_pool <- function(){

  ## Try out

  # pool <- pool::dbPool(
  #
  #   drv = RPostgreSQL::PostgreSQL(),
  #   dbname = "stock",
  #   host = "206.189.149.240",
  #   user = "db_user",
  #   port = 4004,
  #   password = "P@ssw0rDB",
  #   maxSize = 10,
  #   idleTimeout = 120
  #
  # )
  # pool

}

