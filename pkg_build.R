# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T);

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# package documentation
devtools::document()

# restart R
rstudioapi::restartSession()

# install from source
install.packages(getwd(), repo=NULL, type='source')

# load
library(wpCPR)

# check citation
citation('wpCPR')

