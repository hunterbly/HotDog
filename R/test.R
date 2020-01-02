hotdog <- function(){
  print("Hotdog")
  print("Something else")
}

# rename signiture for python
py_get_hit_signal <- function(ref_date, format = 'wide', local = FALSE){

  df = get_hit_signal(ref.date = ref_date,
                      format = format,
                      local = FALSE)
  return(df)
}
