# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  Jan 2019
# Version 0.1

#' wpCPRDemographic function to get a toptal population
#' based on shapefile
#' 
#'
#' @param year The year of the population (2000-2020)
#' @param shapeFilePath The path to the shpaefile
#' @param outputFilePath The opath to the output CVS file
#' @param apikey API key to access the WorldPop API
#' @param callbacktime Default is 5 sec. TIme to call the API server
#' @param maxexectime Default is 600 sec. Max execution time of the request.
#' @param apiurl URL to WorldPop API
#' @param verbose If TRUE then the progress will be shown
#' @return DataFrame* object
#' @export
#' @examples
#' wpCPRDemographic(year=2013, shapeFilePath = file.path("/tmp/Inputs/BFA-HDX-Borders.shp"))
#' 	
wpCPRDemographic <-function(year=2000,
                            shapeFilePath=NULL,
                            outputFilePath=NULL,
                            apikey=NULL,
                            callbacktime=5,
                            maxexectime=3600,
                            apiurl=NULL,
                            verbose=FALSE) {

  tmStartDw  <- Sys.time()
  
  # get API URL
  wpCPRGetBaseURL(apiurl)
  
  # Check if SHP file exist and get a name
  fshp <- is_file_exist(shapeFilePath)

  
  # Check if output file exist and get a name
  fout <- is_output_file_exist(outputFilePath)
  
  # Check if year is between 2000 and 2020
  is_year_corect(year)
  
  # print the information about parameters
  if (verbose){
    cat(paste("Year: ", year,"\n"))
    cat(paste("Path to shape file: ", fshp[['fpath']],"\n"))
    cat(paste("Shape file name: ", fshp[['fname']],"\n"))
  }  
  

  
  # read a shapefile 
  nc <- st_read(shapeFilePath)
  
  spdframe <- as.data.frame(nc)
  spdframe$geometry <- NULL
  
  # creating a dataframe to keep results from api
  df <- stats::setNames(data.frame(matrix(ncol = 42, nrow = 0)), 
                        c('wpid', 
                          'year',
                          'f_0','f_1','f_5','f_10','f_15','f_20','f_25','f_30','f_35',
                          'f_40','f_45','f_50','f_55','f_60','f_65','f_70','f_75','f_80',
                          'm_0','m_1','m_5','m_10','m_15','m_20','m_25','m_30','m_35',
                          'm_40','m_45','m_50','m_55','m_60','m_65','m_70','m_75','m_80',
                          'taskid', 
                          'status', 
                          'message',
                          'executiontime'))
  
  if (verbose) {
    cat(paste0("Start sending polygon to ", getOption("BASE_WP_API_URL")))
    tStartSending <- Sys.time()
  }   
  
  for(i in 1:nrow(nc)) { 
    
    p <- nc[i,]$geometry 
    p_json <- geojsonio::geojson_json(p)
    
    if (verbose) {
      tEndSending <-  Sys.time()
      wpCPRProgressMessage(i, max=nrow(nc), label= paste0("sending ",i,"'s out of ",nrow(nc),". Processing Time: ", wpCPRTimeDiff(tStartSending,tEndSending)))
    }
    
    
    # create a body request
    body <- list(dataset='wpgpas',
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
                  f_0='',f_1='',f_5='',f_10='',f_15='',f_20='',f_25='',f_30='',f_35='',
                  f_40='',f_45='',f_50='',f_55='',f_60='',f_65='',f_70='',f_75='',f_80='',
                  m_0='',m_1='',m_5='',m_10='',m_15='',m_20='',m_25='',m_30='',m_35='',
                  m_40='',m_45='',m_50='',m_55='',m_60='',m_65='',m_70='',m_75='',m_80='',
                  taskid=resp$taskid,
                  status=resp$status,
                  message="NA",
                  executiontime="",
                  stringsAsFactors=FALSE 
                )
    )
  }
  
  
  
  if (verbose) {
    cat(paste0("Sart checing the tasks .....\n")) 
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
          
          df[i,'f_0'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "0", ]$female
          df[i,'f_1'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "1", ]$female
          
          for(ax in seq(5,80,5)) {
            df[i,paste0('f_',ax)] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == ax, ]$female
          } 
          
          # df[i,'f_5'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "5", ]$female
          # df[i,'f_10'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "10", ]$female
          # df[i,'f_15'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "15", ]$female
          # df[i,'f_20'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "20", ]$female
          # df[i,'f_25'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "25", ]$female
          # df[i,'f_30'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "30", ]$female
          # df[i,'f_35'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "35", ]$female
          # df[i,'f_40'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "40", ]$female
          # df[i,'f_45'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "45", ]$female
          # df[i,'f_50'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "50", ]$female
          # df[i,'f_55'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "55", ]$female
          # df[i,'f_60'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "60", ]$female
          # df[i,'f_65'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "65", ]$female
          # df[i,'f_70'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "70", ]$female
          # df[i,'f_75'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "75", ]$female
          # df[i,'f_80'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "80", ]$female
        
          df[i,'m_0'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "0", ]$male
          df[i,'m_1'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "1", ]$male
          
          
          for(ax in seq(5,80,5)) {
            df[i,paste0('m_',ax)] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == ax, ]$male
          }          
          # df[i,'m_5'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "5", ]$male
          # df[i,'m_10'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "10", ]$male
          # df[i,'m_15'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "15", ]$male
          # df[i,'m_20'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "20", ]$male
          # df[i,'m_25'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "25", ]$male
          # df[i,'m_30'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "30", ]$male
          # df[i,'m_35'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "35", ]$male
          # df[i,'m_40'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "40", ]$male
          # df[i,'m_45'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "45", ]$male
          # df[i,'m_50'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "50", ]$male
          # df[i,'m_55'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "55", ]$male
          # df[i,'m_60'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "60", ]$male
          # df[i,'m_65'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "65", ]$male
          # df[i,'m_70'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "70", ]$male
          # df[i,'m_75'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "75", ]$male
          # df[i,'m_80'] <- rsp_task$data$agesexpyramid[rsp_task$data$agesexpyramid$class == "80", ]$male            
        
          df[i,'executiontime'] <- paste0(rsp_task$executionTime)
        }else if ( rsp_task$error == TRUE ){
          df[i,] <-"NA"
          df[i,'status'] <- 'ERROR'
          df[i,'message'] <- rsp_task$data$error_message
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
    
    if (nrow(df[df$status=='finished',]) != nrow(spdframe)) Sys.sleep(callbacktime)
    
  } 
  
  if ( nrow(df[df[,'status'] == 'ERROR',]) > 0 ) {
    message(paste("There were some errors with the following tasks:"))
    print(df[df[,'status'] == 'ERROR',])
  }
  
  options(wpCPR_LOG_DF=df)
  
  spdframe <- cbind(spdframe, df[c("year", 
           "f_0","f_1","f_5","f_10","f_15","f_20","f_25","f_30","f_35",
           "f_40","f_45","f_50","f_55","f_60","f_65","f_70","f_75","f_80",
           "m_0","m_1","m_5","m_10","m_15","m_20","m_25","m_30","m_35",
           "m_40","m_45","m_50","m_55","m_60","m_65","m_70","m_75","m_80")])
 
  if (!is.null(outputFilePath)) {
    if (verbose) message(paste("Results were saved at ", outputFilePath))
    write.csv(spdframe, file = outputFilePath, row.names=FALSE)
  }

  if (!is.null(outputFilePath)) {
    if (verbose) message(paste("Results were saved at ", outputFilePath))
    write.csv(spdframe, file = outputFilePath, row.names=FALSE)
  }
  
  
  tmEndDw  <- Sys.time()
  if (verbose) message(paste("It took ", wpCPRTimeDiff(tmStartDw ,tmEndDw,frm="hms"), " to execute. " ))
  
  return(spdframe)
  
}

