### Assurland - Loan ###



moiactu=strftime(as.Date(Sys.Date(),format="%Y-%m-%d"),format="Y%yM%m")   
moiactu_1=pm(moiactu) 
a = moiactu

l = c()

for(i in 1:13)
{
  tmp = pm(a)
  l = c(l,tmp)
  a = tmp
}



## We load the data, and we transform it.

load("./Tables/New_Table_Assurland_emp.RData") # New crawling
load("./Tables/data_Assurland_emp.RData") # Merged crawling
New_Table$type = "UNKNOWN"
New_Table[New_Table$insurer%in%CLASSIQUEPlayers,]$type = "CLASSIQUE"
New_Table[New_Table$insurer%in%ALTERNATIFSPlayers,]$type = "ALTERNATIFS"
New_Table[New_Table$insurer%in%BANCASSUREURPlayers,]$type = "BANCASSUREUR"
New_Table[New_Table$insurer%in%MUTUELLEPlayers,]$type = "MUTUELLE"







## We compute the pricegap table and we save it. 
# 
# load("./Tables/data_Assurland_emp.RData")

frdata1_new <- New_Table[New_Table$insurer%in%CLASSIQUEPlayers,]


pricegap_predata = NULL


for(p in sort(unique(frdata1_new$yearmonth)))
{
  lastmonth=frdata1_new[frdata1_new$yearmonth==p & frdata1_new$insurer%in%All,]
  lweekes=aggregate(lastmonth$price ~ lastmonth$profilID + lastmonth$insurer + lastmonth$coverage ,FUN=meanna)
  names(lweekes)=c("profilID","insurer","coverage","price")
  
  lweekeskminmarket=lweekes[!grepl("Groupe AXA",lweekes$insurer),]
  
  lweekeskminmarket=lweekeskminmarket[order(lweekeskminmarket$price),]
  
  lweekeskminmarket=lweekeskminmarket[!duplicated(lweekeskminmarket[c("profilID","coverage")]),]
  
  three=lweekes[grepl("Groupe AXA", lweekes$insurer),]
  
  deltamineslast=merge(three,lweekeskminmarket,by=c("profilID","coverage"))
  
  
  deltamineslast$delta=(deltamineslast$price.x/deltamineslast$price.y)-1
  
  for(c in sort(unique(deltamineslast$coverage)))
  {
    deltaminesmars=deltamineslast[deltamineslast$coverage == c,] # 'Formule Optimum' -> c
     
    a=density(deltaminesmars$delta,bw = "sj")
    res=data.frame(x=a$x,y=a$y)
    
    pricegap_predata_1 = data.frame(period = p, coverage = c, x = res$x,y = res$y)
    pricegap_predata = rbind(pricegap_predata, pricegap_predata_1)

  }
}

save(pricegap_predata,file="./Tables/pricegap_emp.RData")










## We compute the market intensity table and we save it. 


frdatatemp_classique <- data.frame(frdata1_new$yearmonth,frdata1_new$profilID,frdata1_new$insurer,frdata1_new$coverage,frdata1_new$price,frdata1_new$Segment)
names(frdatatemp_classique) <- c("period","profilID","insurer","coverage","price","Segment")



marketdispersion_predata  <-  NULL


top3_classique <- top_propor_Generic(frdatatemp_classique,top = 3)
top3_classique <- subset(top3_classique, select = - c(Segment)) # Remove Segment column. 

  
for(p in sort(unique(top3_classique$period)))
{
  lastmonth=top3_classique[top3_classique$period==p,]
  lweekes=aggregate(lastmonth$price ~ lastmonth$profilID + lastmonth$insurer + lastmonth$coverage ,FUN=meanna)
  names(lweekes)=c("profilID","insurer","coverage","price")
  lweekes=unique(lweekes)
    
  for(cov in sort(unique(top3_classique$coverage)))
  {
    if(nrow(lweekes[lweekes$coverage==cov,]) != 0){
    # Now ce compute the coefficient of variation (= sd/mean), which is a standardized measure of dispersion.
    dispesRCmar=aggregate(lweekes$price[lweekes$coverage==cov] ~ lweekes$profilID[lweekes$coverage==cov], FUN= sd)[[2]]/
      aggregate(lweekes$price[lweekes$coverage==cov] ~ lweekes$profilID[lweekes$coverage==cov], FUN=mean)[[2]]
        
    dispesRCmar = na.omit(dispesRCmar)
    res = density(dispesRCmar,bw = "sj")
    res$x=abs(min(res$x))+res$x # We put a very small shift (<0.1) in the variation coefficient values to begin at 0.
    res= data.frame(res$x,res$y)
    colnames(res) = c("x","y")
        
    marketdispersion_predata_1 = data.frame(period=p,coverage=cov,x=res$x,y=res$y)
    marketdispersion_predata = rbind(marketdispersion_predata, marketdispersion_predata_1)
    
    }
  }
}




save(marketdispersion_predata,file="./Tables/marketint_emp.RData")





## Graphs ##

library(plotly)


plot(marketdispersion_predata$x, marketdispersion_predata$y)
plot(pricegap_predata$x, pricegap_predata$y)


axe <- seq(-1,1,0.1)
axe2 <- seq(0,5,1)

g <- ggplot(pricegap_predata)+aes(x=x, y=y, color=coverage)+geom_line()+scale_x_continuous(breaks=axe)+scale_y_continuous(limits=c(0,5))
ggplotly(g)





axe <- seq(-1,1,0.1)
axe2 <- seq(0,5,1)

g <- ggplot(marketdispersion_predata)+aes(x=x, y=y, color=coverage)+geom_line()+scale_x_continuous(breaks=axe)
ggplotly(g)
