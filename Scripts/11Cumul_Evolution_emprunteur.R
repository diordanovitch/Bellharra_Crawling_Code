#### Cumulative evolution #### 


  
frdata_cumul=crawling_all
summarybefore <- NULL

res=data.frame(frdata_cumul$period,frdata_cumul$price,frdata_cumul$insurer,frdata_cumul$profilID,frdata_cumul$coverage)


## We rename the columns

names(res)=c("period","price","insurer","profilID","coverage")



## We re-arrange the database.

res=na.omit(res)
res$period=as.character(res$period)
res=res[res$coverage%in%covfr,]
res=res[order(res$period),]



## We chose the period from whom we calculate.

res=res[res$period>=lp,]



## We create 2 vectors with the differents periods and insurers.

periods <- unique(as.character(res$period))
players <- levels(as.factor(res$insurer))




## We create an empty dataframe.

summaryTab <- data.frame("insurer"=NaN,"coverage"=NaN,"period"=NaN,
                         "PlayerType"=NaN,"AvgEvolByProfile"=NaN,"ImpactedProfile"=NaN,
                         "AvgPremium"=NaN,
                         "AvgPremium_var"=NaN,
                         "Display_prop" = NaN,
                         "Display_var" = NaN,
                         "NbProfiles"=NaN)




## Calcul with the function onePeriodStats for every period.

for(wi in 1:length(periods)){

  w1 <- periods[wi]
  w2 <- periods[wi+1]
  summaryTab <- rbind(summaryTab,onePeriodStats(res,w1,w2))
}

summaryTab <- summaryTab[-1,] # new


##  We process the summary tab. 

summaryTab <- na.omit(summaryTab)
summaryTab$insurer <- as.factor(summaryTab$insurer)
summaryTab$coverage <- as.factor(summaryTab$coverage)
summaryTab$period <- as.factor(summaryTab$period)
summaryTab$PlayerType <- as.factor(summaryTab$PlayerType)
summaryTab=unique(summaryTab)



## We save the data and load it.


save(summaryTab,file=("./Tables/summaryTab_emp.RData"))




## ?

summaryTab <- data.table(summaryTab) 

summaryTab <-rbind(summaryTab, summarybefore)
summaryTab=unique(summaryTab)

# display_graphs(summaryTab,formulaNames,TypesC,Types,PathNameDE)
 
# average_graphs(summaryTab,formulaNames,TypesC,Types,PathNameAPE)
  








### Cumulated evolution ###

## Calculate log variation between two periods in using  "onePeriodlog" function.



summaryCumulTab<- data.frame("profilID"=NaN,"insurer"=NaN,"coverage"=NaN,"period"=NaN,"LNEvolByProfile"=NaN)

cl <- makeCluster(4)
registerDoParallel(cl)


for(wi in 1:length(periods)){
  
  wi
  w1 <- periods[wi]
  w2 <- periods[wi+1]
  
  summaryCumulTab <- rbind(summaryCumulTab,onePeriodlog(res,w1,w2))
  
}

summaryCumulTab_check=na.omit(summaryCumulTab)



## Coherent variations check


# onePeriodlog(res,w1,w2,threshold1,threshold2): for unusual variation, set threshold1 and threshold2 to correct thoses changes
# when ln(P2/P1)>threshold1, then remplace this value with NA, for exemple when price increase 100% compare to last period, set this variation to 0, maybe crawling problem
# when ln(P2/P1)< threshold2, then remplace this value with NA,for exemple when price decrease 100% compare to last period, set this variation to 0, maybe crawling problem


threshold1=0.26 # price increase 30% (50% = 0.6931)
threshold2= -0.26 # price decrease 30% (50% = -0.6931)



## Put a 'check' in the incoherent rows.

summaryCumulTab_check$LNEvolByProfile=ifelse(summaryCumulTab_check$LNEvolByProfile>threshold1,"check",ifelse(summaryCumulTab_check$LNEvolByProfile<threshold2,"check",summaryCumulTab_check$LNEvolByProfile))

check_bigvar=summaryCumulTab_check[summaryCumulTab_check$LNEvolByProfile=="check",]
check_bigvar=unique(check_bigvar)
# Write.csv2(check_bigvar,file=paste(outputPath,"check_bigvar.csv",sep="/"),row.names=F)



## We keep only the coherent variation profiles.

summaryCumulTab = summaryCumulTab_check[!summaryCumulTab_check$LNEvolByProfile=="check",]



## We put the log variation on a 100 scale, and we apply some transformations to the DB.

summaryCumulTab$LNEvolByProfile = as.numeric(summaryCumulTab$LNEvolByProfile)
summaryCumulTab$LNEvolByProfile = round(100*summaryCumulTab$LNEvolByProfile)
summaryCumulTab = na.omit(summaryCumulTab)
summaryCumulTab$insurer <- as.factor(summaryCumulTab$insurer)
summaryCumulTab$coverage <- as.factor(summaryCumulTab$coverage)
summaryCumulTab$period <- as.factor(summaryCumulTab$period)




## Remove years before 2015 

for (y in c("Y15","Y16","Y17", "Y18")) {
  summaryCumulTab1=summaryCumulTab[grepl(y,summaryCumulTab$period),]
}

summaryCumulTab1$period <- factor(summaryCumulTab1$period)



## Compute a key and format dataframe as data table.
summaryCumulTab1$ind=paste(summaryCumulTab1$insurer,summaryCumulTab1$coverage,summaryCumulTab1$profilID)
summaryCumulTab1 <- data.table(summaryCumulTab1)



## Reorder.
summaryCumulTab1=summaryCumulTab1[order(summaryCumulTab1$ind,summaryCumulTab1$period),]



## Compute cumul evolution for same ind. 
## Not very useful, cumullog here is not used, because there are no 2 lines with same combination.
newcumul=summaryCumulTab1[  ,list(cumullog=cumevollogFunc(LNEvolByProfile)),by=list(ind,insurer,coverage,profilID)]


## Compute exponential
newcumul$Exp=round(exp(newcumul$cumullog/100)-1,4)

## Join new results 
JoinResult=cbind(summaryCumulTab1, newcumul)

meanna=function(x) {
  return(mean(x,na.rm=T))
}

## Remove duplicated columns

JoinResult=subset(JoinResult,select=c(profilID,insurer,coverage,period,LNEvolByProfile,cumullog,Exp))
JoinResult=unique(JoinResult)


## For each insurer/period/coverage, we compute the mean of the variation.

logevolfinal <- data.table(JoinResult)
logevolfinal <- logevolfinal[,list(mean=meanna(Exp)), by = c("coverage","period","insurer")]
logevolfinal=unique(logevolfinal)



# plot_ly(x = summaryTab$insurer, y = summaryTab$AvgPremium, name = "Avg Premium by Players",type = "bar")



## Finally, we construct the final database with player type, and we put the variation on a 100 scale.



logevolfinal2 = logevolfinal

logevolfinal2$Playertype =""
logevolfinal2[logevolfinal2$insurer%in%CLASSIQUEPlayers,]$Playertype="CLASSIQUE"
logevolfinal2[logevolfinal2$insurer%in%ALTERNATIFSPlayers,]$Playertype="ALTERNATIFS"
logevolfinal2[logevolfinal2$insurer%in%BANCASSUREURPlayers,]$Playertype="BANCASSUREUR"
logevolfinal2[logevolfinal2$insurer%in%MUTUELLEPlayers,]$Playertype="MUTUELLE"
logevolfinal2$mean=round(logevolfinal2$mean*100,2)
logevolfinal2=data.frame(logevolfinal2)

logevolfinal2=logevolfinal2[order(logevolfinal2$period),]

save(logevolfinal2,file= ("./Tables/logevolfinal2_emp.RData")) 




# cumullog_graphs(logevolfinal2,formulaNames,TypesC,Types,PathNameCEBP)


