


##########Output path##############################################

# adress
outputPath = paste("CIA",as.character(format(Sys.Date()+5,"%Y%b")))
dir.create(outputPath)

PathNameCEBP = paste(outputPath,"Cumulated Evolution By Profile _ graphs",sep="/")
PathNameOWES = paste(outputPath,"One Week Evolution Statistics _ tables",sep="/")
PathNamevar = paste(outputPath,"variation for index",sep="/")
PathNameindex = paste(outputPath,"index graphs",sep="/")
PathNamerank = paste(outputPath,"ranking over time",sep="/")
PathNamelines = paste(outputPath,"price gap",sep="/")
PathNameDE = paste(outputPath,"Display Evolution _ graphs",sep="/")
PathNameAPE = paste(outputPath,"Average premium graphs",sep="/")
PathNameAGE = paste(outputPath,"Ranking by age graphs",sep="/")
PathNamecrm = paste(outputPath,"CRM",sep="/")

dir.create(PathNameCEBP,showWarnings = F)
dir.create(PathNameOWES,showWarnings = F)
dir.create(PathNamevar,showWarnings = F)
dir.create(PathNameindex,showWarnings = F)
dir.create(PathNamerank,showWarnings = F)
dir.create(PathNamelines,showWarnings = F)
dir.create(PathNameDE,showWarnings = F)
dir.create(PathNameAGE,showWarnings = F)
dir.create(PathNamecrm,showWarnings = F)
dir.create(PathNameAPE,showWarnings = F)

