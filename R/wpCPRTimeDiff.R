# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  Jan 2019
# Version 0.1
#
#' wpCPRTimeDiff function to get time difference in human readable format
#' Input is start time and end time
#' If "frm" is set to "hms" then output will be h:m:s
#' otherwise only hours will be returned
#'
#' @param start Starting time
#' @param end Ending time
#' @param frm If "frm" is set to "hms" then output will be h:m:s
#' @rdname wpCPRTimeDiff
#' @return numeric
wpCPRTimeDiff <- function(start, end, frm="hms") {
  
  dsec <- as.numeric(difftime(end, start, units = c("secs")))
  hours <- floor(dsec / 3600)
  
  if (frm == "hms" ){
    minutes <- floor((dsec - 3600 * hours) / 60)
    seconds <- dsec - 3600*hours - 60*minutes
    
    out=paste0(
      sapply(c(hours, minutes, seconds), function(x) {
        formatC(x, width = 2, format = "d", flag = "0")
      }), collapse = ":")
    
    return(out)
  }else{
    return(hours)
  }
}