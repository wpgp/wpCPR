## wpCPR :: An R package to submit a custom polygon request to the WorldPop API 

### Installation

Installation wpCPR isn't available from CRAN yet, but you can get it from github with:

```
install.packages("devtools")
devtools::install_github("wpgp/wpCPR")

# load package
library(wpCPR)
```
Dependencies for the wpCPR package include: R (>= 3.5.0), rgdal,geojsonio,httr,sf,raster,jsonlite

### Basic usage

After installation you should be able to use a main function from the library:
 - wpCPRPopulation 
 
 This function will submitt a custom polygon to WorldPop API to get a estimated population for each polygons.
 
```
wpCPRPopulation (year=2000,
                 shapeFilePath=NULL,
                 outputFilePath=NULL,
                 apikey=NULL,
                 callbacktime=5,
                 maxexectime=3600,
                 apiurl=NULL,
                 verbose=FALSE)
```

Where
 - **year** is a year of dataset (2000-2020), default is 2000
 - **shapeFilePath** path to the shapefile
 - **outputFilePath**  Optional parameterThe path to the output CVS file. If filename does not exist, the file is created. 
 - **apikey** Optional parameter. API key to access the WorldPop API if not specify user will have limitation on functionality of hte API
 - **callbacktime** Default is 5 sec. TIme to call the API server               
 - **maxexectime** Default is 3600 sec. Max execution time of the request.
 - **apiurl**  URL to WorldPop API (Defaiilt https://api.worldpop.org/)
 - **verbose** If TRUE then the progress will be shown (Defailt FALSE)

 **wpCPRDemographic**  function can be used yo get a toptal subnational population age-sex structures
 
## License
* [GNU General Public License v3.0 (GNU GPLv3)](https://github.com/wpgp/wopr/blob/master/COPYING) 
