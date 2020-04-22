# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  Jan 2019
# Version 0.1
#
#' wpCPRGetlog function will return a data.frame
#' 
#' @importFrom utils write.csv 
#' @rdname wpCPRGetlog
#' @param file The path to the shpaefile
#' @return A data.frame with a log data of last API calls
#' @export
#' @examples
#' wpCPRGetlog()
#'
wpCPRGetlog <- function(file=NULL) {
  
  if (is.null(file)) {
    print(getOption("wpCPR_LOG_DF"))
  }else{
    write.csv(getOption("wpCPR_LOG_DF"), file = file, row.names=FALSE)
  }  
  
}
