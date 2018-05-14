
####### Functions for reporting #######


### getWeekActu ###
## Gives the period in the format Y--M-- corresponding to 'prev" weeks before 'date', use to exclude too ancient periods.
 
getWeekActu <-function(date,prev)
{
  wk = week(as.Date(date)-7*prev)
  y1 = as.numeric(year(as.Date(date)-7*prev))
  y2 = as.numeric(year(as.Date(date)))
  diff = y2-y1
  if(wk == 53)
  {
    return(paste("Y",as.numeric(str_sub(year(as.Date(date)),3,4))+1-diff,"W01",sep=""))
  }
  else if(wk < 10){
    return(paste("Y",as.numeric(str_sub(year(as.Date(date)),3,4))-diff,"W0",wk,sep=""))
  }else{
    return(paste("Y",as.numeric(str_sub(year(as.Date(date)),3,4))-diff,"W",wk,sep=""))
  }
}


### previous month ###
## Gives the previous month of 'dt' in the period format.

pm <- function(dt)
{
  yy = as.numeric(str_sub(dt,2,3))
  mm = as.numeric(str_sub(dt,5,6))
  mm = mm - 1
  if(mm == 0){
    mm = 12
    yy = yy - 1
  }
  if(mm < 10)
  {
    c=paste("Y",yy,"M0",mm,sep="")
  }else{
    c=paste("Y",yy,"M",mm,sep="")
  }
  return(c)
}



### genrankovermonths ### 
## Function for ranking over months.

# Parameters: data = prankd, AnalysedCompetitor = agdentity, Title of analysis = agdentitysn, Xaxis = Period (could be other variable, such as age).
genrankovermonths = function (prankd,agdentity,agdentitysn,coventity, Period = "yearmonth") {
  all <- NULL
  prankd$yearmonth <- prankd[,Period]
  dtrankd=as.data.table(prankd)
  dtrankd[,rank:=rank(price,ties.method="first"),by=c("profilID","coverage","yearmonth")]
  prankd=as.data.frame(dtrankd)
  
  for (k in 1:length(coventity)) {
    agrankall=NULL
    for(m in unique(prankd$yearmonth)) {
      prankdc=as.data.frame(table((cut((prankd$rank[grepl(agdentity,prankd$insurer) & prankd$yearmonth==m & prankd$coverage==coventity[k]]),labels=F,breaks=c(0,1,2,3,4,5))),exclude=NULL)/length(prankd$rank[grepl(agdentity,prankd$insurer) & prankd$yearmonth==m & prankd$coverage==coventity[k]]))
      
      names(prankdc)[1]="rankd"
      names(prankdc)[2]="percd"
      prankdc$yearmonth=m
      
      
      
      grankall=as.data.frame(x=c(1,2,3,4,5,NA))
      names(grankall)[1]="ranks"
      grankall$rankstr=c("1st","2nd","3rd","4th","5th","6th +")
      grankall=merge(grankall,prankdc,by.x="ranks",by.y="rankd",all.x=T)
      
      
      grankall$coord=seq(10,60,by=10)
      grankall$yearmonth=m
      grankall$percd[is.na(grankall$percd)]=0
      grankall$cs=1-(cumsum(grankall$percd))+grankall$percd/2
      
      
      agrankall=rbind(agrankall,grankall)
    }
    
    rankcols=rev(brewer.pal(6, "Blues"))
    print(agrankall)
    agrankall$coverage = coventity[k]
    all = rbind(all, agrankall)
    print(
      ggplot(agrankall,aes(x=yearmonth,y=percd,ymax=1,fill=rankstr,label = gsub("^0%|^1%|^2%|^3%|^4%|^5%","",paste(round(percd*100,0),"%",sep=""))))+
        
        #theme(legend.key.size = unit(1.5, "cm"))+
        geom_bar(stat="identity",position="stack")  +
        scale_y_continuous(labels = percent) +ylab(label="")+xlab(label="") + geom_text(aes(y=c(cs)),position = position_identity(), hjust = 0.5, size=8) +
        ggtitle(paste(paste(agdentitysn,"Ranking by Month on displayed profiles"),sep="\n\n"))+
        scale_fill_manual(values = rankcols) + labs(fill="") #+  wtl
    )
    ggsave(paste("./Tables/",file=paste("CI-RankingByMonth-",agdentitysn,"-",coventity[k],".png",sep=""),sep="/"),width=20,height=10,dpi=100)
    
  }
  return(all)
}




### one Period stats ### 
## Very important, allow to output all stats for cumulated evolution, for...

onePeriodStats <- function(res,w1,w2) {
  weeksLoc <- c(w1,w2)
  

  bigDB <- data.frame("insurer"=NaN,"coverage"=NaN,"period"=NaN,
                      "PlayerType"=NaN,"AvgEvolByProfile"=NaN,"ImpactedProfile"=NaN,
                      "AvgPremium"=NaN,
                      "AvgPremium_var"=NaN,
                      "Display_prop" = NaN,
                      "Display_var" = NaN,
                      "NbProfiles"=NaN)
  
  
  for (j in 1:length(formulaNames)){
    
    for (k in 1:length(Types)){
      
      formulaLoc <- formulaNames[j]
      typeLoc <- Types[k]
      s <- paste(typeLoc,"Players",sep="")
      groupLoc <- get(s)
      
      
      # Extraction of local DB.
      resloc0 <- res[which(res$period %in% weeksLoc),] # Period
      resloc1 <- resloc0[which(resloc0$coverage %in% formulaMapping[formulaMapping[,1]==formulaLoc,2]),] # Coverage
      resloc2 <- resloc1[which(resloc1$insurer%in% groupLoc),] # Insurer group
      resloc <- resloc2
      resW1 <- resloc[which(resloc$period %in% weeksLoc[1]),] # Period 1
      resW2 <- resloc[which(resloc$period %in% weeksLoc[2]),] # Period 2
      
      # Initialization of quantites to be computed.
      nLoc <- length(groupLoc)+1
      nbProfiles <- matrix(0,nLoc,4)
      nbProfiles[1,1] <- length(unique(resW1$profilID))
      nbProfiles[1,2] <- length(unique(resW2$profilID))
      avgPremium <- matrix(0,nLoc,3)
      byProfile <- matrix(0,nLoc,2)
      
      for (i in 1:length(groupLoc)) {
     
        # We extract DB corresponding to the assureur company under study.
        idxW1 <- which(resW1$insurer == groupLoc[i],arr.ind=T)
        idxW2 <- which(resW2$insurer == groupLoc[i],arr.ind=T) 
        resW1loc <- resW1[idxW1,]; resW2loc <- resW2[idxW2,]
        
        # Keeping only 3 columns we need. 
        resW1loc <- subset(resW1loc,select=c(period,profilID,price))
        resW2loc <- subset(resW2loc,select=c(period,profilID,price))
        
        # Average of price on same profilID queries (if there are some lines with same profilID).
        resW1loc$price<- ave(resW1loc$price,resW1loc$profilID)
        resW2loc$price <- ave(resW2loc$price,resW2loc$profilID)
        
        # Now we remove duplicates (if they exist).
        resW1loc <- unique(resW1loc)
        resW2loc <- unique(resW2loc)
        
        # Build usefull tab for computations.
        r1merge <- resW1loc
        r2merge <- resW2loc
        colnames(r1merge) <- c("periode1","profilID","price1")
        colnames(r2merge) <- c("periode2","profilID","price2")
        usefullTab <- merge(r1merge,r2merge) # Table with price for period 1 and period 2, for every ID.
        usefullTab$diffP <- usefullTab$price2/usefullTab$price1-1 # Give the variation of price.

        
        # compute statistics.
        
        byProfile[i+1,1] <- round(100*mean(usefullTab$diffP),2) # Mean of total variation. 
        byProfile[i+1,2] <- round(100*(sum(as.numeric(abs(usefullTab$diffP)>0))/length(usefullTab$profilID)),2) # Mean of total variation. 
        nbProfiles[i+1,1] <- length(resW1loc$profilID)
        nbProfiles[i+1,2] <- length(resW2loc$profilID)
        avgPremium[i+1,1] <- round(mean(resW1loc$price),0)
        avgPremium[i+1,2] <- round(mean(resW2loc$price),0)
        
        
      }
      
      # We compute now our final metrics, for one insurer.
      nbProfiles[,3] <- round(100*(nbProfiles[,2]/nbProfiles[1,2]),2) # Proportion of display (only for last period).
      nbProfiles[,4] <- round(100*(nbProfiles[,2]/nbProfiles[,1]-1),2)  # Variation of nb of profiles between period 1 & 2.
      avgPremium[,3] <- round(100*((avgPremium[,2]/avgPremium[,1])-1),2) # Variation of price between period 1 & 2.
      
      rnames <- t(cbind("Total",t(groupLoc)))
      tabOut <- data.frame("AvgEvolByProfile"=byProfile[,1],
                           "ImpactedProfile"=byProfile[,2],
                           "AvgPremium"=avgPremium[,2],
                           "AvgPremium_var"=avgPremium[,3],
                           "Display_prop" = nbProfiles[,3],
                           "Display_var" = nbProfiles[,4],row.names = rnames,
                           "NbProfiles"=nbProfiles[,2])
      
      temp <- cbind("insurer"=groupLoc,"coverage"=rep(formulaLoc,nLoc-1),
                    "period"=rep(weeksLoc[2],nLoc-1),"PlayerType"=rep(typeLoc,nLoc-1),tabOut[2:nLoc,])
      bigDB <- rbind(bigDB,temp)
      
    }
  }
  
  
  bigDB <- data.frame(bigDB,row.names=NULL)
  bigDB <- na.omit(bigDB)
  return(bigDB);
}



### OnePeriodLog ###

onePeriodlog <- function(res,w1,w2) {
  weeksLoc <- c(w1,w2)
  Cumullogtable <- data.frame("profilID"=NaN,"insurer"=NaN,"coverage"=NaN,"period"=NaN,"LNEvolByProfile"=NaN)
  
  
  for (j in 1:length(formulaNames)){
    
    for (k in 1:length(Types)){
      
      formulaLoc <- formulaNames[j]
      typeLoc <- Types[k]
      s <- paste(typeLoc,"Players",sep="")
      groupLoc <- get(s)
      
      # Extraction of local DB
      resloc0 <- res[which(res$period %in% weeksLoc),]
      resloc1 <- resloc0[which(resloc0$coverage %in% formulaMapping[formulaMapping[,1]==formulaLoc,2]),]
      resloc2 <- resloc1[which(resloc1$insurer%in% groupLoc),]
      resloc <- resloc2
      resW1 <- resloc[which(resloc$period %in% weeksLoc[1]),]
      resW2 <- resloc[which(resloc$period %in% weeksLoc[2]),]
      
      for (i in 1:length(groupLoc)) {
        # We extract DB corresponding to the assureur company under study.
        idxW1 <- which(resW1$insurer == groupLoc[i],arr.ind=T)
        idxW2 <- which(resW2$insurer == groupLoc[i],arr.ind=T) 
        resW1loc <- resW1[idxW1,]; resW2loc <- resW2[idxW2,]
        
        # Keeping only 5 columns we need. 
        resW1loc <- subset(resW1loc,select=c(coverage,insurer,period,profilID,price))
        resW2loc <- subset(resW2loc,select=c(coverage,insurer,period,profilID,price))
        
        # Average of price on same profilID queries (if there are some lines with same profilID).
        resW1loc$price<- ave(resW1loc$price,resW1loc$profilID)
        resW2loc$price <- ave(resW2loc$price,resW2loc$profilID)
        
        # Now we remove duplicates (if they exist).
        resW1loc <- unique(resW1loc)
        resW2loc <- unique(resW2loc)
        
        # Build usefull tab for computations.
        r1merge <- resW1loc
        r2merge <- resW2loc
        colnames(r1merge) <- c("coverage","insurer", "periode1","profilID","price1")
        colnames(r2merge) <- c("coverage","insurer","periode2","profilID","price2")
        usefullTab <- merge(r1merge,r2merge) # Table with price for period 1 and period 2, for every ID.
        
        usefullTab$diffP <- log(usefullTab$price2/usefullTab$price1,base=exp(1))  # Give the log variation of price.
        usefullTab=na.omit(usefullTab)
        
        temp <- usefullTab
        temp1=subset(temp,select=c(profilID,insurer,coverage,periode2,diffP))
        names(temp1)=c("profilID","insurer","coverage","period","LNEvolByProfile")
        
        temp1=na.omit(temp1)
        if(i==1)  {temp2=as.data.frame(temp1)} else temp2=rbind(temp1,temp2)
        
      }
      
      Cumullogtable <- rbind(Cumullogtable,temp2)
      
    }
  }
  
  Cumullogtable <- data.frame(Cumullogtable,row.names=NULL)
  Cumullogtable <- na.omit(Cumullogtable)
  return(Cumullogtable);
}


### cumulated evolution log function ### 
## Compute a cumulation line by line

cumevollogFunc <- function(rs){
  
  cumrs = rs
  cumrs[1] =0
  
  if (length(cumrs) == 1) {
    cumrs = rs}
  
  else{
    
    for (lm in 1:(length(cumrs)-1)){
      cumrs[lm+1] = cumrs[lm]+rs[lm+1]
    }
  }
  
  return(cumrs)
}




### Function to compute the ranking.

top_propor_Generic <- function(tables,top = 1){
  
  
  ListFormulas=c(as.character(unique(tables$coverage)))
  
  ListPeriod=c(as.character(unique(tables$period)))
  
  for(i in 1:length(ListFormulas)){
    LocFomulas=ListFormulas[i]
    
    for (j in 1: length(ListPeriod)){
      LocPeriod=ListPeriod[j]
      
      temp=tables[tables$period==LocPeriod,]
      temp=temp[temp$coverage==LocFomulas,]
      
      temp=as.data.table(temp)
      temp[,rank:=rank(price,ties.method="first"),by=c("profilID","coverage","period","Segment")]
      r= 1:top 
      temp=temp[temp$rank%in%r,]
      temp=na.omit(temp)
      temp$ct=1
      
      temp[, cumsum:=lapply(.SD, sum), by = c("insurer","coverage","period","Segment"),.SDcols=c("ct")] # Nb of rank 1 for one insurer during one period for selected coverage
      temp[, cumsum2:=lapply(.SD, sum), by = c("coverage","period","Segment"),.SDcols=c("ct")] # Nb of rank 1 for all insurers during one period for selected coverage
      
      
      if(j==1)  {savefrdata=as.data.frame(temp)} else savefrdata=rbind(savefrdata,temp)
      rm(temp)
      
    }
    if(i==1)  {savefrdataTotal=as.data.frame(savefrdata)} else savefrdataTotal=rbind(savefrdataTotal,savefrdata)
    rm(savefrdata)
  }
  return(savefrdataTotal)
}




### Top_Generic : Output graphs for ranking over months.

top_Generic<- function(data,covfr, exclude_insurer, coveragenames = c("Top 1 Minimum","Top 1 Optimum"), TitleComplement = "All Players", PathComplement = "1st Ranking Proportion by month all"){
  
  if(length(covfr) != length(coveragenames)){stop("Parameter covfr and coveragenames must have the same length!")}
  
  data$proportion=data$cumsum/data$cumsum2
  data$proportion=round(data$proportion*100)
  
  data = data[!data$insurer %in% exclude_insurer]
  
  for(i in 1:length(covfr)){
    
    coverageapping <- cbind(coveragenames,covfr)
    
    datatemp=data[data$coverage==covfr[i],]
    
    datatemp=data.frame(datatemp$insurer,datatemp$coverage,datatemp$period,datatemp$proportion,datatemp$Segment)
    
    names(datatemp)=c("insurer","coverage","period","proportion","Segment")
    
    datatemp=datatemp[!duplicated(datatemp[c("coverage","period","insurer","proportion","Segment")]),]
    
    datatemp =datatemp[with(datatemp, order(coverage,desc(period),as.character(insurer),as.character(Segment))), ]
    
    datatemp <- ddply(datatemp, .(coverage,period,Segment), mutate, csum = cumsum(proportion)-proportion/2)
    
    datatemp =datatemp[with(datatemp, order(desc(insurer),proportion,coverage,period,Segment,csum)), ]
    
    datatemp$insurer= factor(datatemp$insurer,levels=sort(levels(datatemp$insurer), TRUE))
    
    print(ggplot(datatemp,aes(x = period, y = proportion, fill = insurer,order=desc(insurer))) +
            theme(legend.key.size = unit(1, "cm"))+
            geom_bar(position = "fill",stat="identity") + facet_grid(.~Segment)+
            scale_fill_manual(values = colorpalette,name=coverageapping[i])+ scale_y_continuous(labels = percent_format()) + wtl+
            ggtitle(paste(paste(coveragenames[i],"Ranking Proportion by Month", TitleComplement,sep=" "),sep="\n\n"))+
            geom_text(aes(y = csum/100, label = paste(proportion,"%",sep="")), size =7, hjust = 0.5, vjust = 0,stat="identity")+
            xlab("Period") + ylab(""))
    ggsave(paste(PathNamerank,file=paste(PathComplement,"-",covfr[i],".png",sep=""),sep="/"),width=20,height=14,dpi=100)
    
  }
}

# 
# 
# ####function for calcul variation between two periods### TODO: find where it is used.
# evoloneperiod <- function(sum,p1,p2) {
#   
#   sum1=sum[sum$period==p1,]
#   sum2=sum[sum$period==p2,]
#   sumfinal=merge(sum1,sum2, by=("insurer"))
#   
#   sumfinal$var= sumfinal$cumevol.y- sumfinal$cumevol.x
#   sumfinal=data.frame(sumfinal$insurer,sumfinal$var)
#   
# }
# 
# 
# ######## cumul evolution function######
# ####function for calcul variation###
# 
# #TODO: find where it is used
# addplus <- function(var) {
#   ifelse(var>0, paste("+", as.numeric(round(var,00)), sep=""),round(var,00))
# }
# 
# #TODO: find where it is used
# cumevolFunc <- function(rs){
#   rs = rs/100
#   cumrs = rs
#   cumrs[1] = 100
#   
#   for (lm in 1:(length(cumrs))){
#     cumrs[lm+1] = cumrs[lm]*(rs[lm]+1)
#   }
#   
#   cumrs = cumrs-100
#   cumrs = cumrs[2:length(cumrs)]
#   cumrs
# }
# 
# 
# #TODO: find where it is used
# cumulindex <- function(rs){
#        cumrs=rs
#        cumrs[1] = 100
#   for (lm in 2:(length(rs))){
#     cumrs[lm+1] = (cumrs[lm]+rs(lm+1))
#   }
# }



### top 3 propor ###

# top3propor <- function(pro){
#   
#   ListFormulas=c(as.character(unique(pro$coverage)))
#   
#   ListPeriod=c(as.character(unique(pro$period)))
#   
#   
#   for(i in 1:length(ListFormulas)){
#     LocFomulas=ListFormulas[i]
#     
#     for (j in 1: length(ListPeriod)){
#       LocPeriod=ListPeriod[j]
#       
#       temp3=pro[pro$period==LocPeriod,]
#       temp3=temp3[temp3$coverage==LocFomulas,]
#       
#       temp3=as.data.table(temp3)
#       temp3[,rank:=rank(price,ties.method="first"),by=c("profilID","coverage","period")]
#       r=c(1,2,3)
#       temp3=temp3[temp3$rank%in%r,]
#       temp3=na.omit(temp3)
#       temp3$ct=1
#       
#       temp3[, cumsum:=lapply(.SD, sum), by = c("insurer","coverage","period"),.SDcols=c("ct")]
#       temp3[, cumsum2:=lapply(.SD, sum), by = c("coverage","period"),.SDcols=c("ct")]
#       
#       if(j==1)  {savesfrdatat3=as.data.frame(temp3)} else savesfrdatat3=rbind(savesfrdatat3,temp3)
#       rm(temp3)
#     }
#     if(i==1)  {savesfrdataTotal3=as.data.frame(savesfrdatat3)} else savesfrdataTotal3=rbind(savesfrdataTotal3,savesfrdatat3)
#     rm(savesfrdatat3)
#   }
#   
#   return(savesfrdataTotal3)
# }
# 
# 
# 
# ### max na  and min na functions ###
# 
# maxna=function(x) {
#   return(max(x,na.rm=T))
# }
# 
# minna=function(x) {
#   return(min(x,na.rm=T))
# }
# 
# 
# 
# ### summary table indicators ###
# 
# onePeriodStats_max_min <- function(res,w1,w2) {
#   weeksLoc <- c(w1,w2)
#   
#   
#   bigDB <- data.frame("insurer"=NaN,"coverage"=NaN,"period"=NaN,
#                       "PlayerType"=NaN,"AvgEvolByProfile"=NaN,"ImpactedProfile"=NaN,
#                       "AvgPremium"=NaN,
#                       "AvgPremium_var"=NaN,
#                       "Max_variation"=NaN,
#                       "Min_variation"=NaN,
#                       "Display_prop" = NaN,
#                       "Display_var" = NaN,
#                       "NbProfiles"=NaN)
#   
#   
#   for (j in 1:length(formulaNames)){
#     
#     #     for (k in 1:length(Types)){
#     
#     formulaLoc <- formulaNames[j]
#     
#     
#     groupLoc <- unique(res$insurer)
#     
#     # extraction of the local DB
#     resloc0 <- res[which(res$period %in% weeksLoc),]
#     resloc1 <- resloc0[which(resloc0$coverage %in% formulaMapping[formulaMapping[,1]==formulaLoc,2]),]
#     resloc2 <- resloc1[which(resloc1$insurer%in% groupLoc),]
#     resloc <- resloc2
#     resW1 <- resloc[which(resloc$period %in% weeksLoc[1]),]
#     resW2 <- resloc[which(resloc$period %in% weeksLoc[2]),]
#     
#     # initialization of quantites to be computed
#     nLoc <- length(groupLoc)+1
#     nbProfiles <- matrix(0,nLoc,4)
#     nbProfiles[1,1] <- length(unique(resW1$profilID))
#     nbProfiles[1,2] <- length(unique(resW2$profilID))
#     avgPremium <- matrix(0,nLoc,3)
#     byProfile <- matrix(0,nLoc,4)
#     
#     for (i in 1:length(groupLoc)) {
#       # keep only data specific to the assureur company under study
#       idxW1 <- which(resW1$insurer == groupLoc[i],arr.ind=T)
#       idxW2 <- which(resW2$insurer == groupLoc[i],arr.ind=T) 
#       resW1loc <- resW1[idxW1,]; resW2loc <- resW2[idxW2,]
#       
#       # modify 
#       resW1loc <- subset(resW1loc,select=c(period,profilID,price))
#       resW2loc <- subset(resW2loc,select=c(period,profilID,price))
#       
#       # average on same profilID queries (if so)
#       resW1loc$price<- ave(resW1loc$price,resW1loc$profilID)
#       resW2loc$price <- ave(resW2loc$price,resW2loc$profilID)
#       
#       
#       resW1loc <- unique(resW1loc)
#       resW2loc <- unique(resW2loc)
#       
#       # build usefull tab for computations
#       r1merge <- resW1loc
#       r2merge <- resW2loc
#       colnames(r1merge) <- c("periode1","profilID","price1")
#       colnames(r2merge) <- c("periode2","profilID","price2")
#       usefullTab <- merge(r1merge,r2merge)
#       usefullTab$diffP <- usefullTab$price2/usefullTab$price1-1
#       usefullTab=na.omit(usefullTab)
#       
#       # compute statistics
#       
#       if(nrow(usefullTab)!=0){ 
#         
#         byProfile[i+1,1] <- round(100*mean(usefullTab$diffP),2)
#         byProfile[i+1,2] <- round(100*(sum(as.numeric(abs(usefullTab$diffP)>0))/length(usefullTab$profilID)),2)
#         byProfile[i+1,3]<- round(100*maxna(usefullTab$diffP),2)
#         byProfile[i+1,4]<- round(100*minna(usefullTab$diffP),2)
#         
#         
#         nbProfiles[i+1,1] <- length(resW1loc$profilID)
#         nbProfiles[i+1,2] <- length(resW2loc$profilID)
#         avgPremium[i+1,1] <- round(mean(resW1loc$price),0)
#         avgPremium[i+1,2] <- round(mean(resW2loc$price),0)
#         
#       }
#       
#       ## compute here relative display rate and rate rank among the groupLoc competitors
#       
#     }
#     
#     nbProfiles[,3] <- round(100*(nbProfiles[,2]/nbProfiles[1,2]),2)
#     nbProfiles[,4] <- round(100*(nbProfiles[,2]/nbProfiles[,1]-1),2)
#     avgPremium[,3] <- round(100*((avgPremium[,2]/avgPremium[,1])-1),2)
#     
#     rnames <- t(cbind("Total",t(groupLoc)))
#     tabOut <- data.frame("AvgEvolByProfile"=byProfile[,1],
#                          "ImpactedProfile"=byProfile[,2],
#                          "Max_variation"=byProfile[,3],
#                          "Min_variation"=byProfile[,4],
#                          "AvgPremium"=avgPremium[,2],
#                          "AvgPremium_var"=avgPremium[,3],
#                          "Display_prop" = nbProfiles[,3],
#                          "Display_var" = nbProfiles[,4],row.names = rnames,
#                          "NbProfiles"=nbProfiles[,2])
#     
#     temp <- cbind("insurer"=groupLoc,"coverage"=rep(formulaLoc,nLoc-1),
#                   "period"=rep(weeksLoc[2],nLoc-1),"PlayerType"="",tabOut[2:nLoc,])
#     bigDB <- rbind(bigDB,temp)
#     
#   }
#   
#   bigDB <- data.frame(bigDB,row.names=NULL)
#   bigDB <- na.omit(bigDB)
#   return(bigDB);
# }




####### output graphs for checking ####### 

#### function cumul graph#####

cumul_graphs=function(data,forNames,Typc,Typ,path){
  
  # write those tables for calculing the cumul price evolution 
  for (formulaLoc in forNames){
    
    for (typeLoc in Typ){
      s <- paste(typeLoc,"Players",sep="")
      s1 <- Typc$typesComplete[which(Typc$types == typeLoc)]
      groupLoc <- get(s)
      atx <- which(data$insurer %in% get(s) & data$coverage == formulaLoc)
      df <- data[atx,]
      result = data.frame(insurer = NaN,period = NaN, cumevol = NaN)
      for (kl in 1:length(groupLoc)) {
        dfperiodeLoc = df$period[which(df$insurer == groupLoc[kl])]
        tempp = length(dfperiodeLoc)
        repn = rep(groupLoc[kl],tempp)
        repw = dfperiodeLoc
        evolLoc = df$AvgEvolByProfile[which(df$insurer == groupLoc[kl])]
        if (length(evolLoc) == 0) {next}
        evolLocCum = cumevolFunc(evolLoc)
        resultloc = data.frame(insurer= repn, period=repw,cumevol=evolLocCum)
        result = rbind(result,resultloc)
        
        
        result = na.omit(result)
        DF_n = merge(df,result,by=intersect(names(df),names(result)))
        gname <- paste(s,formulaLoc,sep="_")
        
      }
      
      filename <- paste(s, formulaLoc,".csv", sep="")
      write.table(DF_n, file=paste("./output_MR_all",filename,sep="/"), col.names=TRUE,row.names=FALSE,sep=";",quote=FALSE)
      
      # cum evol graph
      print(DF_n)
      
      print(ggplot(DF_n,aes(period,cumevol,group=insurer),ymin=200)+
              geom_line(aes(colour=insurer),size=1.25)+
              xlab("Week")+ylab("Cumulated Average Evolution")+
              ggtitle(paste(paste(formulaLoc,"MRP sample - Cumulated Evolution"," "),s1,sep="\n\n"))+
              theme(plot.title = element_text(lineheight=.6, face="bold"))+
              scale_x_discrete("Week", labels = levels(DF_n$period))+
              #scale_y_continuous(limits = c(-10, 30))+
              theme(axis.text.x=element_text(angle=90))+
              labs(colour="Company",linetype="Company",shape="Company") +
              scale_colour_manual(values = colorpalette))
      
      #save the graph
      ggsave(paste(path,paste(paste("CUMEVOL",s,formulaLoc,sep="_"),"png",sep="."),sep="/"),
             scale = .8, width = 20,height = 10, units = "in") 
      
      
    }
  } 
}



### average evolution function ###

average_graphs=function(data,forNames,Typc,Typ,path){
  
  # write those tables for calculing the cumul price evolution 
  for (formulaLoc in forNames){
    
    for (typeLoc in Typ){
      s <- paste(typeLoc,"Players",sep="")
      s1 <- Typc$typesComplete[which(Typc$types == typeLoc)]
      groupLoc <- get(s)
      atx <- which(data$insurer %in% get(s) & data$coverage == formulaLoc)
      df <- data[atx,]
      result = data.frame(insurer = NaN,period = NaN, cumevol = NaN)
      for (kl in 1:length(groupLoc)) {
        dfperiodeLoc = df$period[which(df$insurer == groupLoc[kl])]
        tempp = length(dfperiodeLoc)
        repn = rep(groupLoc[kl],tempp)
        repw = dfperiodeLoc
        evolLoc = df$AvgEvolByProfile[which(df$insurer == groupLoc[kl])]
        if (length(evolLoc) == 0) {next}
        evolLocCum = cumevolFunc(evolLoc)
        resultloc = data.frame(insurer= repn, period=repw,cumevol=evolLocCum)
        result = rbind(result,resultloc)
        
        
        result = na.omit(result)
        df <- data.frame(df)
        DF_n = merge(df,result,by=intersect(names(df),names(result)))
        gname <- paste(s,formulaLoc,sep="_")
        
      }
      
      print(DF_n)
      print(ggplot(DF_n,aes(x=period,y=AvgPremium,group=insurer),ymin=200,height=500, width=800)+
              geom_line(aes(x=period,y=AvgPremium, colour = insurer,group=insurer),size=2,alpha=1)+
              xlab("Week")+ylab("Average Premium")+
              ggtitle(paste(paste(formulaLoc,"MRP sample - Average Premium"," "),sep=""))+
              theme(plot.title = element_text(lineheight=.6, face="bold"))+
              scale_fill_manual(values = colorpalette,name="insurer")+
              theme(axis.text.x=element_text(angle=80))+
              labs(colour="Company",linetype="Company",shape="Company")+
              scale_colour_manual(values = colorpalette))
      
      ggsave(paste(path, paste(paste("AVGPREMIUM",s,formulaLoc,sep="_"),"png",sep="."),sep="/"),
             scale = .8, width = 20,height = 10, units = "in")
      
    }
  } 
}



### display function ###

display_graphs=function(data,forNames,Typc,Typ,path){
  

  # write those tables for calculing the cumul price evolution 
  for (formulaLoc in forNames){
    
    for (typeLoc in Typ){
  
      
      s <- paste(typeLoc,"Players",sep="")
      s1 <- Typc$typesComplete[which(Typc$types == typeLoc)]
      groupLoc <- get(s)
      atx <- which(data$insurer %in% get(s) & data$coverage == formulaLoc)
      df <- data[atx,]
      result = data.frame(insurer = NaN,period = NaN, cumevol = NaN)
      for (kl in 1:length(groupLoc)) {
        dfperiodeLoc = df$period[which(df$insurer == groupLoc[kl])]
        tempp = length(dfperiodeLoc)
        repn = rep(groupLoc[kl],tempp)
        repw = dfperiodeLoc
        evolLoc = df$AvgEvolByProfile[which(df$insurer == groupLoc[kl])]
        if (length(evolLoc) == 0) {next}
        evolLocCum = cumevolFunc(evolLoc)
        resultloc = data.frame(insurer= repn, period=repw,cumevol=evolLocCum)
        result = rbind(result,resultloc)
        
        
        result = na.omit(result)
        df <- data.frame(df)
        DF_n = merge(df,result,by=intersect(names(df),names(result))) # this line can be the cause of a problem: vecseq...
        gname <- paste(s,formulaLoc,sep="_")
        
      }
      
      DF_n$numeric_periode<-as.numeric(as.factor(DF_n$period))
      last_week=max(DF_n$numeric_periode)
      weekListLocHalfYear = c(round(last_week/2,0):last_week)
      weekList1Year =c(1:last_week)
      weekList10week = c((last_week-9):last_week)
      #weekChoice <- weekListLocHalfYear # Enter as you want !!
      weekChoice <- weekList1Year # Enter as you want !!
      #weekChoice <- weekList10week # Enter as you want !!
      DF_new<-subset(DF_n,numeric_periode  %in%  weekChoice) 
      print(DF_new)
      print(ggplot(DF_new,aes(factor(period),insurer),height=600, width=800)+
              geom_point(aes(size = (2*Display_prop),colour = factor(insurer)),show_guide=FALSE)+
              scale_size(range = c(0, 20*min(max(10/length(weekChoice),0.5),1)))+
              xlab("Week")+ylab("")+
              ggtitle(paste(paste(formulaLoc,"MRP sample - Trend in Display Rate"," "),s1,sep="\n\n"))+
              theme(plot.title = element_text(lineheight=.6, face="bold"), 
                    text=element_text(size=10), axis.text.x = element_text(angle=90, vjust=1)) +
              scale_colour_manual(values = colorpalette))
      ggsave(paste(path,paste(paste("TRDDISPRATE",s,formulaLoc,sep="_"),"png",sep="."),sep="/"),
             scale = .8, width = 20,height = 10, units = "in")
      
    }
  } 
}


### cumul log graphs ###


cumullog_graphs=function(data,forNames,Typc,Typ,path){
  
  # write those tables for calculing the cumul price evolution 
  for (formulaLoc in forNames){
    
    for (typeLoc in Typ){
      s <- paste(typeLoc,"Players",sep="")
      s1 <- Typc$typesComplete[which(Typc$types == typeLoc)]
      groupLoc <- get(s)
      atx =data[data$insurer %in% get(s) & data$coverage == formulaLoc,]
      
      print(ggplot(atx,aes(x=as.factor(unique(period)),y=mean,group=insurer),ymin=200,height=500, width=800)+
              geom_line(aes(x=as.factor(period),y=mean, colour = insurer,group=insurer),size=2,alpha=1)+
              xlab("Week")+ylab("Cumulated Average Evolution")+
              ggtitle(paste(paste(formulaLoc,"MRP sample - Cumulated Evolution"," "),s1,sep="\n\n"))+
              theme(plot.title = element_text(lineheight=.6, face="bold"))+
              scale_x_discrete("Week", labels = levels(as.factor(atx$period)))+
              #scale_y_continuous(limits = c(-20, 20))+
              theme(axis.text.x=element_text(angle=90))+
              labs(colour="Company",linetype="Company",shape="Company") +
              scale_colour_manual(values = colorpalette))
      
      ggsave(paste(path, paste(paste("Clog",s,formulaLoc,sep="_"),"png",sep="."),sep="/"),
             scale = .8, width = 20,height = 10, units = "in")
      
    }
  }
}







####### Functions to store and download database ####### 


### cumulated evolution ###

storeCumulatedEvolution <- function(data){
  
  cumtab=data.frame(period=data$period,insurer=data$insurer,coverage=data$coverage,type=data$type,cumul=data$cumul)
  cumtab = unique(cumtab)
  
  
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  # Delete all prices to rebuild the table
  #   dbGetQuery(conMetis,"DELETE FROM report_italy.\"Cumul_IT_monthly\";")
  
  dataW=dbWriteTable(conMetis,c("report_italy","Cumul_IT_monthly"),cumtab,row.names=FALSE,append = TRUE)
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
}




### downloadCumulatedEvolution ###

downloadCumulatedEvolution <- function(){
  
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  cumul_old = dbGetQuery(conMetis,paste("SELECT * FROM report_italy.\"Cumul_IT_monthly\";",sep=""))
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
  return(cumul_old)
  
}



### Store the average premium ###
# SCHEMA : {period,insurer,coverage,type,avg}
storeAveragePremium <- function(data){
  
  avetab=data.frame(period=data$period,insurer=data$insurer,coverage=data$coverage,type=data$type,avg=data$avg)
  avetab = unique(avetab)
  
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  # Delete all prices to rebuild the table
  #   dbGetQuery(conMetis,"DELETE FROM report_italy.\"Avg_IT_monthly\";")
  
  dataW=dbWriteTable(conMetis,c("report_italy","Avg_IT_monthly"),avetab,row.names=FALSE,append = TRUE)
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
  
}




### downloadAveragePremium ###

downloadAveragePremium  <- function(){
  
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  average_old = dbGetQuery(conMetis,paste("SELECT * FROM report_italy.\"Avg_IT_monthly\";",sep=""))
  
  ## Closes the connection
  dbDisconnect(conMetis)
  #### Metis connection closed ####
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  return(average_old)
  
}


### Store the display rate ###
# SCHEMA : {period,insurer,coverage,type,avg}
storeDisplay <- function(data){
  
  distab=data.frame(period=data$period,insurer=data$insurer,coverage=data$coverage,display=data$Display_prop)
  distab = unique(distab)
  
  for (i in unique(distab$insurer)) {
    for (j in unique(distab$period)) {
      for (k in unique(distab$coverage)) {
        if(as.numeric(nrow(distab[distab$insurer == i & distab$period == j & distab$coverage == k,]))==0)
        {
          distab = rbind(distab,data.frame(period=j,insurer=i,coverage=k,display=0))
        }
      }
    }
  }
  
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv, dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser, password=dbPassword)
  
  # Delete all prices to rebuild the table
  #   dbGetQuery(conMetis,"DELETE FROM report_italy.\"Dis_IT_monthly\";")
  
  
  dataW=dbWriteTable(conMetis,c("report_italy","Dis_IT_monthly"),distab,row.names=FALSE,append = TRUE)
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
}



### downloaddisplay ###

downloaddisplay  <- function(){
  
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  display_old = dbGetQuery(conMetis,paste("SELECT * FROM report_italy.\"Dis_IT_monthly\";",sep=""))
  
  ## Closes the connection
  dbDisconnect(conMetis)
  #### Metis connection closed ####
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
  
}




### store data for ranking calculation ###

storeItData_Direct <- function(itdata_dir){
  
  itdata_tmp=data.frame(period=itdata_dir$yearmonth,profilID=itdata_dir$profilID,insurer=itdata_dir$insurer,coverage=itdata_dir$coverage,price=itdata_dir$price)
  itdata_tmp=unique(itdata_tmp)
  
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  
  dataW=dbWriteTable(conMetis,c("report_italy","ItData_Direct_monthly"),itdata_tmp,row.names=FALSE,append = TRUE)
  
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
  
}




### download periods from ItData_Direct_monthly ###

downloaddirect_period  <- function(){
  
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  directdata_period = dbGetQuery(conMetis,paste("SELECT \"period\" FROM report_italy.\"ItData_Direct_monthly\";",sep=""))
  
  ## Closes the connection
  dbDisconnect(conMetis)
  #### Metis connection closed ####
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  return(directdata_period)
  
}



### store direct players scope ###

storeDIRECTPLAYERS <- function(data){
  
  itdata_tmp=data.frame(insurer=data$insurer,period=data$period,coverage=data$coverage,PlayerType=data$PlayerType,AvgEvolByProfile=data$AvgEvolByProfile,ImpactedProfile=data$ImpactedProfile,AvgPremium=data$AvgPremium,
                        AvgPremium_var=data$AvgPremium_var,Display_prop=data$Display_prop,Display_var=data$Display_var,NbProfiles=data$NbProfiles,Playertype=data$Playertype,cumevol=data$cumevol)
  itdata_tmp=unique(itdata_tmp)
  
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  dataW=dbWriteTable(conMetis,c("report_italy","DIRECTPlayers"),itdata_tmp,row.names=FALSE,append = TRUE)
  
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
  
}




### Download periods from clean prices ###

downloadcleanprice_period  <- function(){
  
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv,dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser , password=dbPassword)
  
  cleanprice_period = dbGetQuery(conMetis,paste("SELECT \"period\" FROM report_italy.\"Clean_Price_Italy\";",sep=""))
  
  ## Closes the connection
  dbDisconnect(conMetis)
  #### Metis connection closed ####
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  return(cleanprice_period)
  
}




## Download market index ###

downloadmarketindex<- function(){
  ## loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  #### Open a connection on Metis####
  conMetis <- dbConnect(drv, dbname=dbMetis, host=dbHost, port=dbPort , user=dbUser, password=dbPassword)
  
  marketindex_old = dbGetQuery(conMetis,paste("SELECT * FROM report_italy.\"MarketIndex_IT_monthly\";",sep=""))
  
  ## disconnect correctly 
  lapply(dbListConnections(drv), FUN = dbDisconnect)
  
  ## Frees all the resources on the driver
  dbUnloadDriver(drv)
  
  return(marketindex_old)
}