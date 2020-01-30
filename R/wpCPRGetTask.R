# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  Jan 2019
# Version 0.1
#
#' wpCPRGetTask function to get respond 
#'
#' @rdname wpCPRGetTask
#' @return json
wpCPRGetTask  <- function(path='v1/tasks/', taskid) {
  
  base_url <- getOption("BASE_WP_API_URL")
  
  url <- modify_url(base_url, path = paste0(path,taskid))
  
  resp <- GET(url)
  
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(suppressMessages(content(resp, 
                                                        "text", 
                                                        encoding = "UTF-8")
  ), simplifyVector = TRUE)
  
  if (http_error(resp)) {
    mssg <- sprintf("WorldPop API request failed :: %s ", parsed$error_message)
    stop(mssg, call. = FALSE)
  }
  
  return(parsed)
  
}