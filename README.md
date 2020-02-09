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

After installation you should be able to a main function from the library:
 - wpCPRPopulation 
 
 This function will submitt a custom polygon to WorldPop API to get a estimated population for each polygons.
 
```
 wpCPRPopulation(year,
                 shapefile,
                 attribute_key, 
                 outputCVSDi,
                 addpopshp=FALSE,
                 api_key=NULL,
                 verbose=FALSE)
```

Where
 - **year** is a year of dataset (2000-2020), default is 2000
 - **shapefile** path to the shapefile
 - **attribute_key** attribute key name in the shape file which identifies the polygons
 - **outputCVSDi** path to the folder for the output results
 - **addpopshp** If TRUE a new shapefile will be created with total population for each polygons
 - **api_key** API key to access WorldPop API. If not provided then limited access will be granted                
 - **verbose**  If TRUE then the progress will be shown  
 
## License
* [GNU General Public License v3.0 (GNU GPLv3)](https://github.com/wpgp/wopr/blob/master/COPYING) 
