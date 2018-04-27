moiactu=strftime(as.Date(Sys.Date(),format="%Y-%m-%d"),format="Y%yM%m")   
moiactu_1=pm(moiactu) 

##### Parameters for functions



## Allowed maximum price variations

threshold1=0.6931 # price increase 50%
threshold2= -0.6931# price decrease 50%


## Insurers

MUTUELLEPlayers <- c("MAAF Assurances")

BANCASSUREURPlayers <- c("Cardif") 

CLASSIQUEPlayers <- c("Groupe AVIVA", "AXA", "SIMPL'ASSUR","Groupe AXA")

ALTERNATIFSPlayers <- c("AFI-ESCA","ALPTIS","AsCourtage","CSF Assurances","HODEVA","Zen'up")

All<-c("MAAF Assurances","Cardif","Groupe AVIVA", "AXA", "SIMPL'ASSUR","Groupe AXA","AFI-ESCA","ALPTIS","AsCourtage","CSF Assurances","HODEVA","Zen'up")



Types <- c("CLASSIQUE","MUTUELLE","BANCASSUREUR","ALTERNATIFS")  # For spain: LC = "Low Cost", For Italy: only Direct
TypesC <- data.frame(types = Types,typesComplete = c("CLASSIQUE","MUTUELLE","BANCASSUREUR", "ALTERNATIFS"))



## Covers

covfr = c("Formule Optimum","Minimum")
coveragenames = c("Top 1 Formule Optimum","Top 1 Minimum")
formulaNames <- covfr 
formulaTypes <- coveragenames 
formulaMapping <- cbind(formulaNames,formulaNames)




## Scope of Age

"18-25"<-18:25
"26-39"<-26:29
"40-54"<-40:54
"55-67"<-55:67



## Scope of loan amount


## Scope of loan duration
"10-17"<-10:17
"18-25"<-18:25
"+25"<-26:25

## Scope of CSP


## Scope of smoking 

## Scope of area




##### Graphical parameters

## Policy, theme, size...

wt=theme(panel.background = element_rect(fill = '#ffffff'),panel.grid.major = element_line(colour = '#dddddd'),panel.grid.minor = element_line(colour = '#eeeeee'),text = element_text(size=24))
wtsl=theme(panel.background = element_rect(fill = '#ffffff'),panel.grid.major = element_line(colour = '#dddddd'),panel.grid.minor = element_line(colour = '#eeeeee'),text = element_text(size=24), axis.text.x = element_text(angle = 90, hjust = 1), legend.background = element_rect(fill = '#ffffff'), legend.key = element_rect(fill = '#ffffff'))
wtl=theme(panel.background = element_rect(fill = '#ffffff'),panel.grid.major = element_line(colour = '#dddddd'),panel.grid.minor = element_line(colour = '#eeeeee'),text = element_text(size=24), axis.text.x = element_text(angle = 90, hjust = 1),legend.background = element_rect(fill = '#ffffff'), legend.key = element_rect(fill = '#ffffff'))
wtsl1=theme(panel.background = element_rect(fill = '#ffffff'),panel.grid.major = element_line(colour = '#dddddd'),panel.grid.minor = element_line(colour = '#eeeeee'),text = element_text(size=24), axis.text.x = element_text(angle = 0, hjust =1,vjust=1 ), legend.background = element_rect(fill = '#ffffff'), legend.key = element_rect(fill = '#ffffff'))


## Colors

my.cols <- c("#c7eae5","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#c51b7d","#6a3d9a","#ffff99","#b15928",
             "#8dd3c7","#004529","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#756bb1","#252525","#fccde5","#737373","#67001f","#decbe4",
             "#c994c7", "#3690c0","#fdae6b","#88419d" ,"#bdbdbd","#1d91c0","#3f007d","#66c2a4","#980043","#9ecae1","#fee0d2" )

colorpalette<-c("AFI-ESCA"=my.cols[1],
                "ALPTIS"=my.cols[2],
                "AsCourtage"=my.cols[3],
                "AXA"=my.cols[4],
                "Cardif"=my.cols[5],
                "CSF Assurances"=my.cols[6],
                "Groupe AVIVA"=my.cols[7],
                "Groupe AXA"=my.cols[8],
                "HODEVA"=my.cols[9],
                "MAAF Assurances"=my.cols[10],
                "SIMPL'ASSUR"=my.cols[11],
                "Zen'up"=my.cols[12])


## Selection period for ranking only. 

seperiod=c(pm(moiactu),pm(pm(moiactu)),pm(pm(pm(moiactu))),
           pm(pm(pm(pm(moiactu)))), pm(pm(pm(pm(pm(moiactu))))),pm(pm(pm(pm(pm(pm(moiactu)))))),
           pm(pm(pm(pm(pm(pm(pm(moiactu))))))),pm(pm(pm(pm(pm(pm(pm(pm(moiactu)))))))),
           pm(pm(pm(pm(pm(pm(pm(pm(pm(moiactu))))))))),  pm(pm(pm(pm(pm(pm(pm(pm(pm(pm(moiactu)))))))))),
           pm(pm(pm(pm(pm(pm(pm(pm(pm(pm(pm(moiactu))))))))))), pm(pm(pm(pm(pm(pm(pm(pm(pm(pm(pm(pm(moiactu)))))))))))),pm(pm(pm(pm(pm(pm(pm(pm(pm(pm(pm(pm(pm(moiactu))))))))))))))