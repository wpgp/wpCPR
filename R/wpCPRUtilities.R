# Function to check if N is integer
#
check.integer <- function(N){
  !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
}


is_url <-function(x) {
  grepl("www.|http:|https:", x)
}


is_dir_exist <-function(d) {
  
  if (!is.null(d)) {
    mssg <- sprintf("Please check dest Dir exists :: %s ", d)
    if (!dir.exists(d)) stop(mssg, call. = FALSE)
  }
  return(invisible(NULL))
}

is_year_corect <-function(y) {
  
  if (is.null(y)) stop("Error: Enter a year", call. = FALSE)
  if (!check.integer(y)) stop(sprintf("Year parameter should be integer :: %s ", y), call. = FALSE)
  if (y < 2000 | y > 2020) stop(sprintf("Year should be between 2000 and 2020 :: %s ", y), call. = FALSE)
  
  return(invisible(NULL))
}


is_file_exist <-function(f) {
  
  if (is.null(f)) stop("Error: Enter a valid path to a shape file" , call. = FALSE)
  if (!file.exists(f)) stop( paste0("Please check file exists: ", f), call. = FALSE)
  
  f.path = dirname(normalizePath(f))
  f.basename = basename(f)
  f.name = substr(basename(f.basename), 1, nchar(basename(f.basename)) - 4)  
  
  return(c(fpath=f.path,fbasename=f.basename,fname=f.name))
}

