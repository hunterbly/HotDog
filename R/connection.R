get_connection <- function(){
	pool <- pool::dbPool(

	  drv = RPostgreSQL::PostgreSQL(),
	  dbname = "stock",
	  host = "127.0.0.1",
	  user = "db_user",
	  password = "P@ssw0rDB",
	  #password = "",
	  maxSize = 10,
	  idleTimeout = 120

	)
	pool

}

# DBI::dbDisconnect(pool
