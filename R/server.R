pool <- pool::dbPool(
  #drv = RPostgreSQL::PostgreSQL(),
  drv = DBI::dbDriver("PostgreSQL"),
  dbname = "stock",
  host = "127.0.0.1",
  user = "postgres",
  password = "P@ssw0rDB",
  #password = "",
  maxSize = 10,
  idleTimeout = 120

)

onStop(function() {
  poolClose(pool)
})
