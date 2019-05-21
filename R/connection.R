sql_connection <- function(){

  ## Return connection for remote database

  conn <- DBI::dbConnect(drv    = RPostgreSQL::PostgreSQL(),
                         dbname = "stock",
                         host   = "206.189.149.240",
                         port   = 4004,
                         user   = "db_user",
                         password = 'P@ssw0rDB')
  return(conn)
}


sql_query <- function(sql){

  ### Get data from the provided sql

  conn <- sql_connection()

  res <- data.table(DBI::dbGetQuery(conn, sql))

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

