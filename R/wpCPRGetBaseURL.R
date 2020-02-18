# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  Jan 2019
# Version 0.1
#
#' wpCPRGetBaseURL function to return a URL of API
#'
#' @param url URL path to WorldPop API
#' @rdname wpCPRGetBaseURL
#' @return string
wpCPRGetBaseURL  <- function(url){
  
  if (is.null(url)) {
    options(BASE_WP_API_URL='https://api.worldpop.org')
  }else{
    if (is_url(url)){
      options(BASE_WP_API_URL=url)
    }else{
      mssg <- sprintf("Please check your URL :: %s ", url)
      stop(mssg, call. = FALSE)      
    }
  }  
  return(invisible(NULL))
}
