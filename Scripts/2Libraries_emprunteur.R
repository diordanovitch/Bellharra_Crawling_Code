# # installing/loading the package:
# if(!require(installr)) {
#   install.packages("installr"); require(installr)} #load / install+load installr
# 
# # using the package:
# updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.



#### Load library ####

#neededPacks = c("scales","ggplot2","scales", "plyr", "RColorBrewer", "data.table", "plyr", "reshape","stringr","gtools",
#               "grid","chron","TTR","data.table", "RPostgreSQL","gridExtra","sqldf", "ggplot2","colorspace", "reshape2", "RDCOMClient", 
#                "lme4", "doBy", "DBI", "xts","zoo","Matrix", "Rcpp","foreach", "parallel", "doParallel", "rJava", "xlsxjars", "xlsx", "dplyr", "R2PPT")

neededPacks = c("scales","ggplot2", "plyr", "RColorBrewer", "data.table", "reshape","stringr","gtools",
	"grid","chron","TTR", "RPostgreSQL","gridExtra","sqldf", "colorspace", "reshape2", 
	"lme4", "doBy", "DBI", "xts","zoo","Matrix", "Rcpp","foreach", "parallel", "doParallel", "dplyr", "plotly")
				
for (packLoc in neededPacks) {
  if (!is.element(packLoc, installed.packages()[,1])) {
    install.packages(packLoc)
  }
  library(packLoc,character.only = TRUE)
}

