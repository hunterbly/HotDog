log_args <- function(func, arg.str){

  ## Some logging function
  ##
  ## Args:
  ##  func (str):
  ##  arg.str (str):

  tryCatch({
    init_logger()

    # Handling for opencpu call. Function name is missing for opencpu call from allargs function. Append back the function name
    if(grepl("^\\(.*$", arg.str, perl = TRUE)){
      arg.str = paste0(func, arg.str)
    }

    # Log non JSON arg
    flog.info(arg.str)
  },
  error = function(e){
    message(e)
  })

  return(NULL)
}
