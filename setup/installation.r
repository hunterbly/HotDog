remove.packages('HotDog')
devtools::install(pkg = ".", quick = TRUE, dependencies = FALSE, upgrade = "never", keep_source = TRUE)
