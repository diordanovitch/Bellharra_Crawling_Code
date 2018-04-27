 n    ##### Save in old dataset ######
 #TO be rem
#  dbHost = "localhost"
#  dbPort ="5432"
#  dbUser = "postgres"
#  dbPassword = "R@litsa86!"
 
 
# Download the Current dataset
  dbMetis = "metis"
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  ## Open a connection on Metis
  conMetis <- dbConnect(drv, dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser, password=dbPassword)

    if(Report == "Assurland_Report"){
      data = dbGetQuery(conMetis,  "select * from report_france.\"Clean_Price_assurland\" ;")
    }else if(Report == "LeLynx_Report"){
      data = dbGetQuery(conMetis,  "select * from report_france.\"Clean_Price_lelynx\";")
    }else if(Report == "Italy_Report"){
      data = dbGetQuery(conMetis,  "select * from report_italy.\"Clean_Price_Italy\" where \"coverage\" like 'RCA' ;")
    }else if(Report == "Spain_Report"){
      data = dbGetQuery(conMetis,  "select * from report_spain.\"Clean_Price_Spain\" where \"period\" >= 'Y15W01';")
    }else if(Report == "Japan_Report"){
      data = dbGetQuery(conMetis,  "select *,\"period\" as \"yearmonth\", \"profilid\" as \"profilID\" 
                        from report_japan.\"clean_prices_jp\";")
    }

  
  ## Closes the connection
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)

  
  #Before adding to previous dataset, ensure that there is no conflict of period (period aly present)
  test <- which(unique(as.character(New_Table$period)) %in% unique(as.character(data$period)) )
  if(!identical(test,integer(0)) & ForceUpdate == F){
    stop("Period already exists. Change the variable ForceUpdate to 'True' do delete already present period from CleanPriceTable.")
  }else{
    
    
    
    #if no conflict, put together
    data <- data[!data$period %in% unique(as.character(New_Table$period)),!colnames(data) %in% c("type","id","formula","profilid")]
    data <- rbind(data,New_Table[,colnames(data)])
    data <- New_Table ######To modify  when there is a new crawling#####
    
    data <- data[!duplicated(data[,c("profilID","period","yearmonth","insurer","coverage" )]),]
    
    
    data$Segment <- "Global"
    save(data, file = "./output_MR_all/Assurland_emprunteur/data_Assurland.RData")
      }
  
  