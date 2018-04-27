
### Global Ranking ###

ranking_top1 = top_propor_Generic(New_Table,top = 1)

ranking_top1$proportion=ranking_top1$cumsum/ranking_top1$cumsum2
ranking_top1$proportion=round(ranking_top1$proportion*100)
ranking_top1 = ranking_top1[,-c(1,4,5,6,8,10,11,12,13)]
ranking_top1 <- as.data.table(ranking_top1)
ranking_top1 <- unique(ranking_top1, by=c("insurer", "coverage"))


save(ranking_top1,file= ("./Tables/ranking_top1.RData")) 




ranking_top3 = top_propor_Generic(New_Table,top = 3)

ranking_top3$proportion=ranking_top3$cumsum/ranking_top3$cumsum2
ranking_top3$proportion=round(ranking_top3$proportion*100)
ranking_top3 = ranking_top3[,-c(1,4,5,6,8,10,11,12,13)]
ranking_top3 <- as.data.table(ranking_top3)
ranking_top3 <- unique(ranking_top3, by=c("insurer", "coverage"))

save(ranking_top3,file= ("./Tables/ranking_top3.RData")) 







### Ranking by player ###

# ranking_player <- genrankovermonths(New_Table, "AXA", "AXA", formulaNames)




ranking_by_player_all <- NULL

for (insurer in All) {
  tryCatch({
    nam <- paste("ranking_player_", insurer, sep = "")
    rank <- genrankovermonths(New_Table, insurer, insurer, formulaNames)
    rank$insurer <- insurer
    assign(nam, rank)
    ranking_by_player_all <- rbind(ranking_by_player_all, rank) 
  }, error=function(e){})
} 

ranking_by_player_all <- ranking_by_player_all[,-c(5,6)]


save(ranking_by_player_all,file= ("./Tables/ranking_by_player_all.RData")) 




# ## Compute ranking graphs : TO BE FIXED ##
# 
# ## Top 1
# 
# # All competitors
# top_Generic(top1_classique,covfr,coveragenames)
# 
# # Without alternatifs players
# top_Generic(top1_classique,covfr, coveragenames, exclude_insurer = ALTERNATIFSPlayers, TitleComplement = "Without Alternatifs Players")
# 
# 
# 
# ## Top 3
# 
# # All competitors
# top_Generic(top3_classique,covfr,coveragenames)
# 
# # Without alternatifs players
# top_Generic(top3_classique,covfr, coveragenames,  exclude_insurer = ALTERNATIFSPlayers, TitleComplement = "Without Alternatifs Players")
# 
# 
