############ functions###########

## store clean prices 


store_cleanprice <- function(tab){

  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  #   dbGetQuery(con,"DELETE FROM report_france.\"Clean_Price_assurland\";")
  dataW= dbWriteTable(con,c("report_france","Clean_Price_assurland"), tab,row.names=FALSE,append = TRUE)
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
}



## download periods from clean prices

downloadcleanprice_period  <- function(){
  
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  cleanprice_period = dbGetQuery(conMetis,paste("SELECT \"period\" FROM report_france.\"Clean_Price_assurland\";",sep=""))
  
  ## Closes the connection
  dbDisconnect(conMetis)
  #### Metis connection closed ####
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
  return(cleanprice_period)
  
}

##store_rank_price
store_rank_price <- function(tab){

  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
# dbGetQuery(con,"DELETE FROM report_france.\"FrData_monthly_assurland\";")
  
  dataW= dbWriteTable(con,c("report_france","FrData_monthly_assurland"), tab,row.names=FALSE,append = TRUE)
  
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
}

### download cumulated evolution 


downloadCumulatedEvolution<- function(tab){
  
    drv <- dbDriver("PostgreSQL")
    conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
    #   dbGetQuery(con,"DELETE FROM report_france.\"Cumul_monthly_assurland\";")
    cumul_period = dbGetQuery(conMetis,paste("SELECT \"period\" FROM report_france.\"Cumul_monthly_assurland\";",sep=""))
    
    ## disconnect correctly 
    lapply(dbListConnections(drv), FUN = dbDisconnect)
    
    ## Frees all the resources on the driver
    dbUnloadDriver(drv)
    
    return(cumul_period)
  }

### store cumulated evolution 
StorecumulatedEvolution<- function(tab){

  drv <- dbDriver("PostgreSQL")
  
  con <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  dataW= dbWriteTable(con,c("report_france","Cumul_monthly_assurland"), tab,row.names=FALSE,append = TRUE)
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
}

## store display rare


StoredisplayEvolution<- function(tab){

  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
#   dbGetQuery(con,"DELETE FROM report_france.\"Display_monthly_assurland\";")
  dataW= dbWriteTable(con,c("report_france","Display_monthly_assurland"), tab,row.names=FALSE,append = TRUE)
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
}


## store average premium 



Storeavepremium<- function(tab){
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
#   dbGetQuery(con,"DELETE FROM report_france.\"AvgPrem_monthly_assurland\";")
  dataW= dbWriteTable(con,c("report_france","AvgPrem_monthly_assurland"), tab,row.names=FALSE,append = TRUE)
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
}


######### store data for lynx


Store_clean_prices_lynx<- function(tab){

  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  #   dbGetQuery(con,"DELETE FROM report_france.\"Clean_Price_lelynx\";")
  dataW= dbWriteTable(con,c("report_france","Clean_Price_lelynx"), tab,row.names=FALSE,append = TRUE)
  
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  dbUnloadDriver(drv)

}

### download period for data

downloadcleanprice_period_lynx  <- function(){
  
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  cleanprice_period = dbGetQuery(conMetis,paste("SELECT \"period\" FROM report_france.\"Clean_Price_lelynx\";",sep=""))
  
  ## Closes the connection
  dbDisconnect(conMetis)
  #### Metis connection closed ####
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
  return(cleanprice_period)
  
}

### store the ranking prices 

store_rank_price_lynx <- function(tab){
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  #   dbGetQuery(con,"DELETE FROM report_france.\"FrData_monthly_lelynx\";")
  dataW= dbWriteTable(con,c("report_france","FrData_monthly_lelynx"), tab,row.names=FALSE,append = TRUE)
  
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
}

## store cumulated evolution 


store_Cumulated_lynx<- function(tab){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
#   dbGetQuery(con,"DELETE FROM report_france.\"Cumul_monthly_lelynx\";")
  dataW= dbWriteTable(con,c("report_france","Cumul_monthly_lelynx"), tab,row.names=FALSE,append = TRUE)
  
lapply(dbListConnections(drv), FUN = dbDisconnect)

## Frees all the resources on the driver
dbUnloadDriver(drv)
}

#### dowload the period for cumulated evol

downloadCumulated_lynx<- function(tab){
  
  drv <- dbDriver("PostgreSQL")
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)

  cumul_period = dbGetQuery(conMetis,paste("SELECT \"period\" FROM report_france.\"Cumul_monthly_lelynx\";",sep=""))
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
  return(cumul_period)
}


######## store average premium

store_average_lynx<- function(tab){
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
#   dbGetQuery(con,"DELETE FROM report_france.\"AvgPrem_monthly_lelynx\";")
  dataW= dbWriteTable(con,c("report_france","AvgPrem_monthly_lelynx"), tab,row.names=FALSE,append = TRUE)
## disconnect correctly 
lapply(dbListConnections(drv), FUN = dbDisconnect)

## Frees all the resources on the driver
dbUnloadDriver(drv)

}

#### store display

store_display_lynx<- function(tab){
  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  #   dbGetQuery(con,"DELETE FROM report_france.\"Display_monthly_lelynx\";")
  dataW= dbWriteTable(con,c("report_france","Display_monthly_lelynx"), tab,row.names=FALSE,append = TRUE)
  ###
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
}


