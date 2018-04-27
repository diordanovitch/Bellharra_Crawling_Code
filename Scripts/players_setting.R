
####  Scope of insurer.


if(Report == "LeLynx_Report"){
  DIRECTPlayers <- c("Aon","l'Olivier","Assu2000","Active Assurances","AllSecur","Eurofil", "Direct Assurance","SOS Malus","4Assur","Assurpeople","AssurOne","Amaguiz","AcommeAssure","BestAssurances","AXA_FR","YouDrive")
  RESIDUALPlayers <- c("AprilAuto", "ThelemAssur","DirectAssurancePack","l'olivierPack","AllianzPremium","AllSecurPremium","BONUS50","AssurOnLine","Assureo","AssurBonPlan","TEACerede","CarY","Carrefour")
  TRADIMUTUALPlayers <- c("AllianzCE","Allianz","MDP") 
  All <- c(DIRECTPlayers,RESIDUALPlayers,TRADIMUTUALPlayers)
  
  
  
}else if(Report == "Assurland_Loan"){
  MUTUELLEPlayers <- c("MAAF Assurances")
  BANCASSUREURPlayers <- c("Cardif") 
  CLASSIQUEPlayers <- c("Groupe AVIVA", "AXA", "SIMPL'ASSUR","Groupe AXA")
  ALTERNATIFSPlayers <- c("AFI-ESCA","ALPTIS","AsCourtage","CSF Assurances","HODEVA","Zen'up")
  All<-c("MAAF Assurances","Cardif","Groupe AVIVA", "AXA", "SIMPL'ASSUR","Groupe AXA","AFI-ESCA","ALPTIS","AsCourtage","CSF Assurances","HODEVA","Zen'up")

    
  
  
}else if(Report == "Assurland_Report"){
  DIRECTPlayers <- c("AllSecur","clicMAAF","l'Olivier","Assu2000","Eurofil","Direct Assurance","ALOA","Amaguiz","IDMacif","Assureo","SIMPL'ASSUR","AcommeAssure","AXA_FR")
  TRADIMUTUALPlayers <- c("GMF","eAllianz","Allianz","MAAF","MMA") 
  RESIDUALPlayers <- c("Active Assurances","AssurOne","Autofirst","Carrefour","SOS Malus","4assur","Assurpeople","LUXIOR Assurance","Assurbike")
  All <- c(DIRECTPlayers,RESIDUALPlayers,TRADIMUTUALPlayers)
  
  
}else if(Report == "Italy_Report"){
  
  DIRECTPlayers=c("Zurich-Connect","Dialogo","Quixa","ConTe","Genialloyd","Direct Line","Genertel","Linear","Prima.it","Direct Assicurazioni")
  all=c(DIRECTPlayers, "Unipol Assicurazioni")
  RESIDUALPlayers <- all[!all %in% DIRECTPlayers]
  
  
}else if(Report == "Spain_Report"){ 
  all=c("YCARMAPFRE","VERTI", "TERRANEA","REGAL","REALE","RACC","QUALITASAUTO","PONTCASER","PONTGRUP","FIATC",
        "PELAYONEX","PELAYO","NUEZ","NEXT","MUTUAMADRILENA","MMT","MAPFRE","LINEA_PENELOPE","LIBERTY","LAGUNARO","GENESIS","GENERALIUM","GENERALI",
        "FIATCDIRECTO", "FENIX","DIRECT","CLICKSEGUROS","BALUMBA","AMA","ONYX","AXA","AMIC" ,"SEGUROSBILBAO", "CATALANAOCCIDENTE" ,"NATIONALSUISSE","MUSSAP", "PLUSULTRA", "ZURICH","FIDELIDADE")
  
  DIRECTPlayers <- c("DIRECT","FENIX","GENESIS","MUTUAMADRILENA","PELAYO","PELAYONEX","LINEA_PENELOPE","QUALITASAUTO","VERTI","AXA")
  TRADIMUTUALPlayers <- c("GENERALI","MAPFRE","REALE","PONTCASER", "CATALANAOCCIDENTE", "PLUSULTRA", "ZURICH") 
  LCPlayers<- c("BALUMBA","CLICKSEGUROS","NUEZ","BALUMBAMYBOX")
  RESIDUALPlayers<-c( "LAGUNARO","MMT" , "REGAL","YCARMAPFRE","AMA"  , "FENIXPREMIUM" ,"FIATCDIRECTO" , "FIATC","GENERALIUM","RACC",
                      "INTERNAUTO",  "LIBERTY" ,"NEXT", "PONTGRUP", "SEGURISIMA","TERRANEA","ONYX","CLICKSEGUROS","AMIC" ,"SEGUROSBILBAO" ,"NATIONALSUISSE","MUSSAP","FIDELIDADE")
  
  
}else if(Report == "Japan_Report"){
  DIRECTPlayers=c("ADJ","SBI","MITSUI","ZURICH","SAISON")                                               
  
}else{stop("Name of report is not defined!")}



#### Scope of coverage. 

if(Report == "LeLynx_Report"){ 
  covfr = c("RC","RC-EXT","RC-TR")
  coveragenames = c("Top 1 MTPL","Top 1 MTPL EXT","Top 1 Full Co.")
  formulaNames <- covfr
  formulaTypes <- coveragenames
  formulaMapping <- cbind(formulaNames,formulaNames)
  
}else if(Report == "Assurland_Loan"){ 
  covfr = c("Formule Optimum","Minimum")
  coveragenames = c("Top 1 Formule Optimum","Top 1 Minimum")
  formulaNames <- covfr 
  formulaTypes <- coveragenames 
  formulaMapping <- cbind(formulaNames,formulaNames)
  
}else if(Report == "Assurland_Report"){ 
  covfr = c("RC","RC-BDG","RC-BDG-VI","RC-TR")
  coveragenames = c("Top 1 MTPL","Top 1 MTPL BDG","Top 1 MTPL BDG VI","Top 1 Full Co.")
  formulaNames <- covfr #c("RC","RC-TR")
  formulaTypes <- coveragenames #c("RC","RC-TR")
  formulaMapping <- cbind(formulaNames,formulaNames)
}else if(Report == "Italy_Report"){
  covfr = c("RCA")  # RCA-FEI-CRI 
  coveragenames <- c("RCA")  # RCA-FEI-CRI
  formulaNames <- covfr
  formulaTypes <- coveragenames
  formulaMapping <- cbind(formulaNames,formulaTypes)
}else if(Report == "Spain_Report"){
  covfr=c("T1","T2","T3","T4")
  coveragenames = c("T1","T2","T3","T4")
  formulaNames <- covfr
  formulaTypes <- coveragenames
  formulaMapping <- cbind(formulaNames,formulaTypes)
}else if(Report == "Japan_Report"){
  covfr=c("F1","F2","F3","F4","F5")
  coveragenames = c("F1","F2","F3","F4","F5")
  formulaNames <- c("F1","F2","F3","F4","F5")
  formulaTypes <- c("F1","F2","F3","F4","F5")
  formulaMapping <- cbind(formulaNames,formulaTypes)
}





