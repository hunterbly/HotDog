remove.packages('HotDog')
devtools::install(pkg = ".", quick = TRUE, dependencies = FALSE, upgrade = "never", keep_source = TRUE)

# Host server
opencpu::ocpu_start_server(port = 4000)
