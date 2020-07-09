# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  Jan 2019
# Version 0.1
#
#' wpCPRPostRequest function to send POST request 
#'
#' @importFrom httr modify_url POST http_type http_error
#' @importFrom jsonlite fromJSON  
#' @param path Sub-path to the API 
#' @param args Extra arguments for httr::POST
#' @rdname wpCPRPostRequest
#' @return string
wpCPRPostRequest <- function(path='v1/services/stats', args) {
  
  base_url <- getOption("BASE_WP_API_URL")
  
  url <- modify_url(base_url, path = path)
  
  resp <- POST(url, body=args, encode = "json") # verbose(info = TRUE)
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- fromJSON(suppressMessages(httr::content(resp,
                                                    "text",
                                                    encoding = "UTF-8")
                                      ),simplifyVector = FALSE)
  
  tidymess <- function(..., prefix = " ", initial = ""){
    message(strwrap(..., prefix = prefix, initial = initial))
  }
  
  if (httr::status_code(resp)==429) {
    stop("Your application is sending too many requests per day. (Limit is 1000 requests per day. Please contact WorldPop for an API Key to lift a limit)", call. = FALSE)
  }
  
  if (httr::status_code(resp)==403) {
    stop("Wrong KEY was provided or REST API is not enabled for this key", call. = FALSE)
  }  
  
  if (httr::http_error(resp)) {
    mssg <- sprintf("WorldPop API request failed :: %s ", parsed$error_message)
    stop(mssg, call. = FALSE)
  }
  
  return(parsed)
  
}


