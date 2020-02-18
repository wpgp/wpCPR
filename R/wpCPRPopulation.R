# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  Jan 2019
# Version 0.1

#' wpCPRPopulation function to get a toptal population
#' based on shapefile
#' 
#'
#' @param year The year of the population (2000-2020)
#' @param shapefile The path to the shpaefile
#' @param layer The layer name.
#' @param attribute_key The name of the attribute in the SHP file
#' @param outputCVSDir The opath to the output CVS file If filename does not exist, the file is created. Otherwise, the existing file is overwritten, unless the FILE_APPEND flag is set
#' @param addpopshp If TRUE then the total population of each polygon will be added to a new SHP file
#' @param api_key API key to access the WorldPop API
#' @param callback_time Default is 5 sec. TIme to call the API server
#' @param max_exec_time Default is 600 sec. Max execution time of the request.
#' @param wp_API_url URL to WorldPop API
#' @param verbose If TRUE then the progress will be shown
#' @return DataFrame* object
#' @export
#' @examples
#' wpCPRPopulation(year=2013, shapefile = file.path("/tmp/Inputs/BFA-HDX-Borders.shp"),attribute_key="admin2Pcod")
#' 	
wpCPRPopulation <-function(year=2000,
                           shapeFilePath=NULL,
                           outputFilePath=NULL,
                           apikey=NULL,
                           callbacktime=5,
                           maxexectime=1800,
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
  nc <- st_read(shapeFilePath)
  
  spdframe <- as.data.frame(nc)
  spdframe$geometry <- NULL

  
  # creating a dataframe to keep results from api
  df <- stats::setNames(data.frame(matrix(ncol = 7, nrow = nrow(nc))), 
                 c('wpid', 
                   'year',
                   "pop", 
                   "taskid", 
                   "status", 
                   "message",
                   "executiontime"))

  for(i in 1:nrow(nc)) { 
    
    p <- nc[i,]$geometry 
    p_json <- geojsonio::geojson_json(p)
    
    if (verbose) print(paste0("Sending polygon ",
                              i , 
                              " to ", 
                              getOption("BASE_WP_API_URL"))
    )
    
    
    # create a body request
    body <- list(dataset='wpgppop',
                 year=year,
                 geojson=p_json,
                 runasync='true')
    
    # add a API key to hte body request if exist
    if (!is.null(apikey) && !is.na(apikey) && !is.nan(apikey)) {
      body[['api_key']] <- apikey
    }
    
    resp <- wpCPRPostRequest('v1/services/stats', body)
    
    df <- rbind(df,
                data.frame(
                  wpid=i,
                  year=year,
                  pop=0,
                  taskid=resp$taskid,
                  status=resp$status,
                  message='',
                  executiontime='',
                  stringsAsFactors=FALSE 
                )
    )
  }
  
  
  
  if (verbose) cat(paste0("Sart checing the tasks .....\n")) 
  
  tmpt <- 0
  while( nrow(df[df$status=='created',]) != 0) {
    
    if (tmpt > maxexectime){
      stop(paste0("You have reached a maximum execution time of ", maxexectime," sec." ))
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
          if (verbose) print(paste0("+ ", df[i,'wpid'] , " :: task ID: ", taskid ," finished")) 
        }else if ( rsp_task$error == TRUE ){
          df[i,'status'] <- 'ERROR'
          df[i,'message'] <- rsp_task$data$error_message
          df[i,'pop'] <- 0
          df[i,'executiontime'] <- paste0(rsp_task$executionTime)
          if (verbose) print(paste0("- ", df[i,'wpid'] , " :: task ID: ", taskid ," ERROR"))           
        }
        
      }
      
    }
    tmpt <- tmpt + callbacktime
    
    if (nrow(df[df$status=='finished',]) != nrow(nc)) Sys.sleep(callbacktime)
    
  }   
  
 
  if (verbose) {
    print(df)
    keeps <- cln
  }else{
    keeps <- c('wpid', "year", "pop")
  }
  

  spdframe <- cbind(spdframe, df$year, df$pop)
  names(spdframe)[length(names(spdframe))-1] <- "year" 
  names(spdframe)[length(names(spdframe))] <- "pop"
  
  
  if (!is.null(outputFilePath)) {
    # cvs.flname <- paste0(fshp[['fname']],"_POP_",year,".csv")
    # cvs.dir.flname <-file.path(outputCVSDir,cvs.flname)
    if (verbose) message(paste("Results are saved at ", outputFilePath))
    write.csv(spdframe, file = outputFilePath, row.names=FALSE)
  }

  
  
  tmEndDw  <- Sys.time()
  if (verbose) message(paste("It took ", wpCPRTimeDiff(tmStartDw ,tmEndDw,frm="hms"), " to execute. " ))
  
  return(df[keeps])
  
}

