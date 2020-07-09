# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  Jan 2019
# Version 0.1

#' wpCPRPopulation function to get a toptal population
#' based on shapefile
#' 
#' @importFrom sf st_read 
#' @importFrom utils write.csv 
#' @param year The year of the population (2000-2020)
#' @param shapeFilePath The path to the shpaefile
#' @param outputFilePath The opath to the output CVS file. If filename does not exist, the file is created. 
#' @param apikey API key to access the WorldPop API
#' @param callbacktime Default is 5 sec. TIme to call the API server
#' @param maxexectime Default is 3600 sec. Max execution time of the request.
#' @param apiurl URL to WorldPop API
#' @param verbose If TRUE then the progress will be shown
#' @return DataFrame* object
#' @export
#' @examples
#' wpCPRPopulation(year=2013, shapeFilePath = file.path("/tmp/Inputs/BFA-HDX-Borders.shp"))
#' 	
wpCPRPopulation <-function(year=2000,
                           shapeFilePath=NULL,
                           outputFilePath=NULL,
                           apikey=NULL,
                           callbacktime=5,
                           maxexectime=3600,
                           apiurl=NULL,
                           verbose=FALSE) {

  tmStartDw  <- Sys.time()

  stopifnot(!missing(shapeFilePath), !is.null(shapeFilePath))
  
  # Check if SHP file exist and get a name
  fshp <- is_file_exist(shapeFilePath)

  # Check if output file exist and get a name
  fout <- is_output_file_exist(outputFilePath)
  
  # get API URL
  wpCPRGetBaseURL(apiurl)

  # Check if year is between 2000 and 2020
  is_year_corect(year)
  
  # print the information about parameters
  if (verbose){
    message(paste("Year: ", year))
    message(paste("Path to shape file: ", fshp[['fpath']]))
    message(paste("Shape file name: ", fshp[['fname']]))
  }  

  # read a shapefile 
  nc <- st_read(shapeFilePath, quiet = ifelse(verbose, FALSE, TRUE))
  
  spdframe <- as.data.frame(nc)
  spdframe$geometry <- NULL

  
  # creating a dataframe to keep results from api
  df <- stats::setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
                 c('wpid', 
                   'year',
                   "pop", 
                   "taskid", 
                   "status", 
                   "message",
                   "executiontime"))
  
  if (verbose) {
    message(paste0("Start sending polygon to ", getOption("BASE_WP_API_URL")))
    tStartSending <- Sys.time()
  }  

  for(i in 1:nrow(nc)) { 
    
    p <- nc[i,]$geometry 
    p_json <- geojson_json(p)

    if (verbose) {
      tEndSending <-  Sys.time()
      wpCPRProgressMessage(i, max=nrow(nc), label= paste0("sending ",i,"'s out of ",nrow(nc),". Processing Time: ", wpCPRTimeDiff(tStartSending,tEndSending)))
    }
    
    # create a body request
    body <- list(dataset='wpgppop',
                 year=year,
                 geojson=p_json,
                 runasync='true')
    
    # add a API key to the body request if exist
    if (!is.null(apikey) && !is.na(apikey) && !is.nan(apikey)) {
      body[['key']] <- apikey
    }
    
    resp <- wpCPRPostRequest('v1/services/stats', body)
    
    df <- rbind(df,
                data.frame(
                  wpid=i,
                  year=year,
                  pop=0,
                  taskid=resp$taskid,
                  status=resp$status,
                  message="NA",
                  executiontime="NA",
                  stringsAsFactors=FALSE 
                )
    )
  }
  

  if (verbose) {
    message(paste0("Sart checing the tasks .....")) 
    tStartChecing <- Sys.time()
  }
  
  tmpt <- 0
  while( nrow(df[df$status=='created',]) != 0) {
    
    if (tmpt > maxexectime){
      stop(paste0("You have reached a maximum execution time of ", maxexectime," sec.",
                  "Change param [maxexectime] in the function if you need to run it longer" ))
      break;
    }
    
    for(i in 1:nrow(df)) { 
      
      if (df[i,'status'] != 'finished' | df[i,'status'] != 'ERROR'){
        
        taskid <- df[i,'taskid']
        
        rsp_task <- wpCPRGetTask('v1/tasks/',taskid=taskid)
        
        if (rsp_task$status == 'finished' & rsp_task$error == FALSE){
          df[i,'status'] <- 'finished'
          df[i,'message'] <- ''
          df[i,'pop'] <- rsp_task$data$total_population
          df[i,'executiontime'] <- paste0(rsp_task$executionTime)
        }else if ( rsp_task$error == TRUE ){
          df[i,'status'] <- 'ERROR'
          df[i,'message'] <- rsp_task$data$error_message
          df[i,'pop'] <- "NA"
          df[i,'executiontime'] <- paste0(rsp_task$executionTime)        
        }
        
      }
      
      if (verbose) {
        tEndChecing <- Sys.time()
        ddone <- nrow(df[df[,'status'] == 'finished' | df[,'status'] == 'ERROR',])
        wpCPRProgressMessage(ddone, max=nrow(df), label= paste0("done. Processing Time: ", wpCPRTimeDiff(tStartChecing,tEndChecing)))
      }       
      
    }
    tmpt <- tmpt + callbacktime
    
    if (nrow(df[df$status=='finished',]) != nrow(nc)) Sys.sleep(callbacktime)
    
     
    
  }   
  
  if ( nrow(df[df[,'status'] == 'ERROR',]) > 0 ) {
    message(paste("There were some errors with the following tasks:"))
    message(df[df[,'status'] == 'ERROR',])
  }
  
  options(wpCPR_LOG_DF=df)

  spdframe <- cbind(spdframe, df$year, df$pop)
  names(spdframe)[length(names(spdframe))-1] <- "year" 
  names(spdframe)[length(names(spdframe))] <- "pop"

  if (!is.null(outputFilePath)) {
    if (verbose) message(paste("Results were saved at ", outputFilePath))
    write.csv(spdframe, file = outputFilePath, row.names=FALSE)
  }

  tmEndDw  <- Sys.time()
  if (verbose) message(paste("It took ", wpCPRTimeDiff(tmStartDw ,tmEndDw,frm="hms"), " to execute. " ))
  
  return(spdframe)
  
}


