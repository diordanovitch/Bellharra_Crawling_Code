## We import the last crawling

crawling_new <- read.csv(file="./Crawlings/ASSURLAND_LOAN_prices_March.csv", header=TRUE, sep=";")




## First small transformations


crawling_new = crawling_new[,c(1,2,4,5,6,7,14,15,16,8)]

crawling_new = subset(crawling_new, crawling_new[,3]!= "NO_INSURER")

crawling_new$fees <- gsub("[\r\n]", "", crawling_new$fees)

crawling_new$fees[as.character(crawling_new$fees)=='Gratuits'] <- 0

names(crawling_new)[names(crawling_new)=="prix"] <- "price"

names(crawling_new)[names(crawling_new)=="date_extraction"] <- "date_aspiration"



## We create a new column price+fees

crawling_new$price <- as.numeric(crawling_new$price)
crawling_new$fees <- as.numeric(crawling_new$fees)
crawling_new$priceYCfees <- crawling_new$price + crawling_new$fees



## Re-arrangement of columns

crawling_new <- crawling_new[c(1,2,3,4,5,10,6,7,8,9,11)]



## Define period and yearmonth

crawling_new$period <- paste( "Y",substr(crawling_new$year,3,4), "W",formatC(crawling_new$week,width=2, flag="0") , sep = "")
crawling_new$yearmonth <- paste( "Y",substr(crawling_new$year,3,4), "M",formatC(crawling_new$month,width=2, flag="0") , sep = "")



## Remove useless columns

crawling_new <- subset(crawling_new, select = -c(year,month,week,campaignID,fees))

crawling_new=crawling_new[!duplicated(crawling_new[c("profilID","insurer","coverage","period","yearmonth","price")]),]






### Define scopes of Insurers and covers

## Covers

covfr = c("Formule Optimum","Minimum")

crawling_new=crawling_new[crawling_new$coverage%in%covfr,]




## Scopes of insurer: can change according to AL report or LL report.

MUTUELLEPlayers <- c("MAAF Assurances")

BANCASSUREURPlayers <- c("Cardif") 

CLASSIQUEPlayers <- c("Groupe AVIVA", "AXA","Groupe AXA")

ALTERNATIFSPlayers <- c("AFI-ESCA","ALPTIS","AsCourtage","CSF Assurances","HODEVA","Zen'up", "SIMPL'ASSUR")

All<-c("MAAF Assurances","Cardif","Groupe AVIVA", "AXA", "SIMPL'ASSUR","Groupe AXA","AFI-ESCA","ALPTIS","AsCourtage","CSF Assurances","HODEVA","Zen'up")



## Filter on the selected scope  

crawling_new <- crawling_new[crawling_new$insurer %in% All,]



New_Table <- crawling_new





