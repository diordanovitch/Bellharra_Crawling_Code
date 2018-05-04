profils <- read.csv(file="./Crawlings/profils_LOAN_FR.csv", header=TRUE, sep=";")


colnames(profils)[3] = "profilID"







New_Table_PCA = New_Table[, c(1, 2,3,4)]

New_Table_PCA$type =""
New_Table_PCA[New_Table_PCA$insurer%in%CLASSIQUEPlayers,]$type="CLASSIQUE"
New_Table_PCA[New_Table_PCA$insurer%in%ALTERNATIFSPlayers,]$type="ALTERNATIFS"
New_Table_PCA[New_Table_PCA$insurer%in%BANCASSUREURPlayers,]$type="BANCASSUREUR"
New_Table_PCA[New_Table_PCA$insurer%in%MUTUELLEPlayers,]$type="MUTUELLE"

New_Table_PCA$type = as.factor(New_Table_PCA$type)

New_Table_complete = merge(New_Table_PCA, profils, by=c('profilID'), all.x = TRUE, all.y = TRUE)

New_Table_complete_PCA = New_Table_complete[, c(2,3,4,5,10,11,12,13,14,15,17,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36)]


New_Table_complete_PCA$firstloan_amount = as.numeric(New_Table_complete_PCA$firstloan_amount)



# crawling_mfa = FAMD(New_Table_complete_PCA, type = c("n", "n", "c", "n", "c", "n", "n", "c", "c", "c", "n", "c", "n", "n", "n","n",
#                                                     "n", "n", "n", "c", "n", "c", "n", "n", "n", "n", "n", "n", "n"),
#                    group=c(12,2, 4, 2, 2, 2, 2, 16, 6, 3, 5, 3, 3, 3, 7, 14, 6, 3, 5, 3, 4), num.group.sup = c(1,2,4))




New_Table_complete_PCA = na.omit(New_Table_complete_PCA)

crawling_mfa = FAMD(New_Table_complete_PCA,  sup.var = c(1,2,4))

plot.FAMD(crawling_mfa, choix = c("ind"), lab.var = F, lab.ind = F, habillage = "type",  invisible = c("quali"))
plot.FAMD(crawling_mfa, choix = c("quali"), lab.var = F, lab.ind = F, habillage = "insurer", invisible = c("ind", "ind.sup"))
