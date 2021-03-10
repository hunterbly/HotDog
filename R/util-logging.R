log_args <- function(func, arg.str) {

  ## Some logging function
  ##
  ## Args:
  ##  func (str):
  ##  arg.str (str):

  tryCatch(
    {
      init_logger()

      # Handling for opencpu call. Function name is missing for opencpu call from allargs function. Append back the function name
      if (grepl("^\\(.*$", arg.str, perl = TRUE)) {
        arg.str <- paste0(func, arg.str)
      }

      # Log non JSON arg
      flog.info(arg.str)
    },
    error = function(e) {
      message(e)
    }
  )

  return(NULL)
}

init_logger <- function() {

  ## Initiate logging
  ##
  ## Args:
  ##  Nil
  ##
  ## Returns:
  ##  log.file (str): Full path for the log file.
  ##
  ## Example:
  ##  init_logger()               # /tmp/WTCHK/WTCHK.log

  suppressWarnings({
    library(futile.logger)
  })

  log.file <- get_log_file()
  flog.appender(appender.file(log.file))

  layout <- layout.format("[~l] [~t] [~n.~f] ~m")
  flog.layout(layout)

  return(log.file)
}


get_log_folder <- function() {

  ## Get absolute path for the log root folder. Under /tmp in Linux
  ##
  ## Args:
  ##  NULL
  ##
  ## Returns:
  ##  log.folder (str): Absolute path for log root folder
  ##
  ## Example:
  ##  get_log_folder()

  # Handle different separator for Window and Linux, \\ and /
  temp <- gsub("\\\\", "/", tempdir())

  # Hanlde different base folder for Window and Linux. (Some folder)/Temp/ocpu-temp or /tmp/ocpu-temp
  base.folder <- gsub("^(.*?)(Temp|tmp)(.*?)$", "\\1\\2", temp, perl = TRUE)


  log.folder <- file.path(base.folder, "HotDog")
  create_folder_if_not_exists(log.folder)

  return(log.folder)
}

get_log_file <- function() {

  ## Get absolute path for the log file
  ##
  ## Args:
  ##  NULL
  ##
  ## Returns:
  ##  log.file (str): Absolute path for log file
  ##
  ## Example:
  ##  get_log_file()

  log.folder <- get_log_folder()
  log.file <- file.path(log.folder, paste0("HotDog", ".log"))
  return(log.file)
}

create_folder_if_not_exists <- function(folder) {

  ## Create folder recursively if not exists. Used in create directory for the local SQLite folder location
  ##
  ## Args:
  ##  folder (str): relative path of the folder location
  ##
  ## Returns:
  ##  NULL: If folder is created successfully, return NULL
  ##
  ## Example:
  ##  local.database = get_sqlite_path("WTCHK")
  ##  local.dir = dirname(local.database)
  ##  create_folder_if_not_exists(local.dir)

  if (file.exists(folder)) {
    # Do nowthing
  } else {
    tryCatch(dir.create(folder, showWarnings = TRUE, recursive = TRUE),
      error = function(e) {
        print(sprintf("Failed to create folder %s", folder))
      }
    )
  }
}

allargs <- function(orig_values = FALSE) {

  ## Used as evaluating functon arguments in parent function call. Use primarily on external function logging.
  ## Long JSON format like pog.hierarchy, template, usage.setup is removed before logging
  ##
  ## Args:
  ##  orig_values (bool): Flag to display the orginal variable name or not. FALSE to parse the value instead of variable name
  ##
  ## Returns:
  ##  func_call (str): A string with the function calls with argumets being evaluated
  ##
  ## Example:
  ##  test <- function(a = 1, b = 2) {
  ##     myargs = allargs()
  ##     print(myargs)
  ##  }
  ##
  ## Todo:
  ##  1. Support ... arguments for function call

  # get formals for outmost function
  parent_formals <- formals(sys.function(sys.parent(n = 1)))
  parent_func <- deparse(sys.calls()[[1]])[1]
  parent_func_name <- gsub("^(\\w*)(.*?)$", replacement = "\\1", x = parent_func, fixed = F)

  # Get names of implied arguments
  fnames <- names(parent_formals)

  # Remove '...' from list of parameter names if it exists
  # fnames <- fnames[-which(fnames == '...')]

  # Get currently set values for named variables in the parent frame
  args <- evalq(as.list(environment()), envir = parent.frame())

  # Remove from long JSON input from args
  args <- suppressWarnings(within(args, remove("pog.hierarchy", "template", "usage.setup", "optimize.result")))

  # Get the list of variables defined in '...'
  # args <- c(args[fnames], evalq(list(...), envir = parent.frame()))


  if (orig_values) {
    # get default values
    defargs <- as.list(parent_formals)
    defargs <- defargs[unlist(lapply(defargs, FUN = function(x) class(x) != "name"))]
    args[names(defargs)] <- defargs
    setargs <- evalq(as.list(match.call())[-1], envir = parent.frame())
    args[names(setargs)] <- setargs
  }

  # Remove NULL element, Add additional quote for character input
  args <- Filter(Negate(is.null), args) # Remove NULL elements in a list
  args <- lapply(args, function(x) {
    x <- ifelse(length(x) > 1, paste0("c(", paste0(x, collapse = ","), ")"), x) # Handle list input
    x <- ifelse(is.character(x), paste0("'", x, "'"), x) # Handle Character, add quote

    return(x)
  })


  str_arg <- paste(paste(names(args), "=", args), collapse = ", ")
  str_func <- ifelse(parent_func_name == "opencpu", "", parent_func_name)

  func_call <- sprintf("%s(%s)", str_func, str_arg)
  return(func_call)
}
