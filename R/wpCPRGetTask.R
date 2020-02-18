# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  Jan 2019
# Version 0.1
#
#' wpCPRGetTask function to get respond 
#'
#' @param path Sub-path to the API 
#' @param taskid Task ID of the job on WorldPop API
#' @rdname wpCPRGetTask
#' @return json
wpCPRGetTask  <- function(path='v1/tasks/', taskid) {
  
  base_url <- getOption("BASE_WP_API_URL")
  
  url <- httr::modify_url(base_url, path = paste0(path,taskid))
  
  resp <- httr::GET(url)
  
  
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(suppressMessages(httr::content(resp,
                                                              "text",
                                                              encoding = "UTF-8")), 
                               simplifyVector = TRUE)
  
  if (http_error(resp)) {
    mssg <- sprintf("WorldPop API request failed :: %s ", parsed$error_message)
    stop(mssg, call. = FALSE)
  }
  
  return(parsed)
  
}
