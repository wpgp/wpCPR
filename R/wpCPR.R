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
#' @param outputCVSDir The opath to the output CVS file
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
                          shapefile=NULL,
                          layer=NULL,
                          attribute_key=NULL,
                          outputCVSDir=NULL,
                          addpopshp=FALSE,
                          api_key=NULL,
                          callback_time=5,
                          max_exec_time=1800,
                          wp_API_url=NULL,
                          verbose=FALSE) {
  
  tmStartDw  <- Sys.time()
  
  
  
  # get API URL
  wpCPRGetBaseURL(wp_API_url)
  
  # Check if SHP file exist and get a name
  fshp <- is_file_exist(shapefile)
  
  # check if output SHP file already exist
  if (addpopshp){
    outputSHP <- file.path(fshp[['fpath']],paste0(fshp[['fname']],"_POP_",year,".shp"))
    if (file.exists(outputSHP)) stop( paste0("Please remove old output SHP file: ", outputSHP), call. = FALSE)
  }  
  
  
  # Check if output directory exist
  is_dir_exist(outputCVSDir)
  
  # Check if year is between 2000 and 2020
  is_year_corect(year)
  
  # print the information about parameters
  if (verbose){
    message(paste("Year: ", year))
    message(paste("Path to shape file: ", fshp[['fpath']]))
    message(paste("Shape file name: ", fshp[['fname']]))
  }  
  
  # if user did not confirm a layer name in shapefilewe 
  # will use the name of the file
  if (is.null(layer)) layer <- fshp[['fname']]
  
  # read a shapefile 
  spdframe <- rgdal::readOGR(dsn = fshp[['fpath']], layer = layer, verbose = verbose)
  
  # check if entered attribute_key exist in shape file db
  if (!attribute_key %in% colnames(spdframe@data)){
    stop( paste0("Attribute key '", attribute_key,"' does not exist in ", fshp[['fbasename']] ))
  }
  
  # creating a dataframe to keep results from api
  df <- stats::setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
                 c('index', 
                   'year',
                   "pop", 
                   "taskid", 
                   "status", 
                   "message",
                   "executiontime"))
  
  for(i in 1:nrow(spdframe)) { 
    
    p <- spdframe[i,] 
    p_json <- geojsonio::geojson_json(p)
    
    if (verbose) print(paste0("Sending ",
                              as.character(p@data[[attribute_key]]) , 
                              " shape file to ", 
                              getOption("BASE_WP_API_URL"))
    )
    
    
    # create a body request
    body <- list(dataset='wpgppop',
                 year=year,
                 geojson=p_json,
                 runasync='true')
    
    # add a API key to hte body request if exist
    if (!is.null(api_key) && !is.na(api_key) && !is.nan(api_key)) {
      body[['api_key']] <- api_key
    }
    
    resp <- wpCPRPostRequest('v1/services/stats', body)
    
    df <- rbind(df,
                data.frame(
                  index=as.character(p@data[[attribute_key]]),
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
    
    if (tmpt > max_exec_time){
      stop(paste0("You have reached a maximum execution time of ", max_exec_time," sec." ))
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
          if (verbose) print(paste0("+ ", df[i,'index'] , " :: task ID: ", taskid ," finished")) 
        }else if ( rsp_task$error == TRUE ){
          df[i,'status'] <- 'ERROR'
          df[i,'message'] <- rsp_task$data$error_message
          df[i,'pop'] <- 0
          df[i,'executiontime'] <- paste0(rsp_task$executionTime)
          if (verbose) print(paste0("- ", df[i,'index'] , " :: task ID: ", taskid ," ERROR"))           
        }
        
      }
      
    }
    tmpt <- tmpt + callback_time
    
    if (nrow(df[df$status=='finished',]) != nrow(spdframe)) Sys.sleep(callback_time)
    
  }   
  
  cln <- c(attribute_key, "year", "pop", "taskid", "status", "message", "exec_time")
  colnames(df) <- cln
  
  if (verbose) {
    print(df)
    keeps <- cln
  }else{
    keeps <- c(attribute_key, "year", "pop")
  }
  
  
  if (!is.null(outputCVSDir)) {
    cvs.flname <- paste0(fshp[['fname']],"_POP_",year,".csv")
    cvs.dir.flname <-file.path(outputCVSDir,cvs.flname)
    if (verbose) message(paste("Results are saved at ", cvs.dir.flname))
    write.csv(df[keeps], file = cvs.dir.flname, row.names=FALSE)
  }
  
  
  if (addpopshp){
    
    spdframe.merged <- merge(spdframe, df[c(attribute_key, "pop")], by = attribute_key)
    writeOGR(obj=spdframe.merged, 
             dsn=fshp[['fpath']], 
             layer=paste0(fshp[['fname']],"_POP_",year), 
             driver="ESRI Shapefile")
    
    if (verbose) message(paste("A new shapefile with population was saved at ", fshp[['fpath']]))
    
  }
  
  
  tmEndDw  <- Sys.time()
  if (verbose) message(paste("It took ", wpCPRTimeDiff(tmStartDw ,tmEndDw,frm="hms"), " to execute. " ))
  
  return(df[keeps])
  
}

