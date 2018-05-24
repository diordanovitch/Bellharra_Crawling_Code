library("dplyr")

## We chose the companies we want to compare.

BENCHMARK <-c("MAAF Assurances","Cardif","Groupe AVIVA", "AXA", "SIMPL'ASSUR","Groupe AXA","Zen'up")


## We create the table for the benchmark, from New_Table_Complete_PCA created for PCA with all infos from the 'profils' file.

Table_Benchmark <- New_Table_complete_PCA[New_Table_complete_PCA$insurer %in% BENCHMARK,]



## Part of single applicant compared to sharing loan.

sum(Table_Benchmark$primary_applicant_share == 1) / length(Table_Benchmark$primary_applicant_share)


## We compare the number of prestations between 2 periods to see the width of the filtration (just DA).


Old_Table <- crawling_all[crawling_all$period %in% "Y17W46",]

Filtration = ( nrow(New_Table[New_Table$insurer %in% "Groupe AXA",]) / nrow(Old_Table[Old_Table$insurer %in% "Groupe AXA",]) ) - 1

Filtration_Opt = ( nrow(New_Table[New_Table$insurer %in% "Groupe AXA" & New_Table$coverage %in% 'Formule Optimum',]) / nrow(Old_Table[Old_Table$insurer %in% "Groupe AXA" & Old_Table$coverage %in% 'Formule Optimum',]) ) - 1

Filtration_Min = ( nrow(New_Table[New_Table$insurer %in% "Groupe AXA" & New_Table$coverage %in% 'Minimum',]) / nrow(Old_Table[Old_Table$insurer %in% "Groupe AXA" & Old_Table$coverage %in% 'Minimum',]) ) - 1



## Computation of average loan amount by insurer.

Table_Benchmark_Global <- Table_Benchmark %>% group_by(insurer, coverage) %>% mutate(Avg.Loan.Amount = mean(firstloan_amount))


## Computation of average loan duration by insurer.

Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate(Avg.Loan.Duration = mean(firstloan_duration_years))



## Computation of average loan rate by insurer.

Table_Benchmark_Global$firstloan_rate <- gsub(",", ".", as.vector(Table_Benchmark_Global$firstloan_rate), fixed=TRUE)

Table_Benchmark_Global$firstloan_rate <- as.numeric(Table_Benchmark_Global$firstloan_rate)

Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate(Avg.Loan.Rate = mean(firstloan_rate))




## Computation of loan type (amortissable/in fine) by insurer.

Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate( Prop.Loan.Type = sum(firstloan_type == 'Amortissable') / length(firstloan_type) )



## Computation of loan rate statut (fixe/variable) by insurer.

Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate( Prop.Loan.Statut = sum(firstloan_type_tax == 'Fixe') / length(firstloan_type_tax) )



## Computation of male proportion for primary applicant by insurer.

Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate( Prop.Male = sum(primary_applicant_sex == 'Homme') / length(primary_applicant_sex) )



## Computation of marital statut (maried) by insurer.

Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate( Prop.Marital.Statut = sum(primary_applicant_marital_status == 'Marié') / length(primary_applicant_marital_status) )



## Computation of job statut (CDI) by insurer.

Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate( Prop.CDI = sum(primary_applicant_contract == 'CDI') / length(primary_applicant_contract) )



## We create a new column to compute the mean age for 2 sharing applicants, and then we compute the average age by insurer.

Table_Benchmark_Global$secondary_applicant_age <- as.numeric(as.vector(Table_Benchmark_Global$secondary_applicant_age))

Table_Benchmark_Global$Age_global <- as.vector(Table_Benchmark_Global$primary_applicant_share) * as.vector(Table_Benchmark_Global$primary_applicant_age) + (1 - as.vector(Table_Benchmark_Global$primary_applicant_share)) * as.vector(Table_Benchmark_Global$secondary_applicant_age)

Table_Benchmark_Global[which(is.na(Table_Benchmark_Global$Age_global)),"Age_global"]<-Table_Benchmark_Global$primary_applicant_age[which(is.na(Table_Benchmark_Global$Age_global))]

      
Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate(Avg.Age = mean(Age_global))




## Computation of travel proportion.

Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate( Travel.Prop = 1 - ( sum(primary_applicant_travel == 'Non' | primary_applicant_travel ==  'None') / length(primary_applicant_travel) ) )


## Computation of risk proportion.

Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate( Risk.Prop = 1 - ( sum(primary_applicant_risk == 'Non' | primary_applicant_risk ==  'None') / length(primary_applicant_risk) ) )


# Calcul des prix pour les clients à risk.

Price_Risk <- Table_Benchmark %>% filter(primary_applicant_risk == 'Oui')
Price_Risk <- Price_Risk %>% group_by(coverage,insurer) %>% summarise(Price.Risk = mean(price) )

write.csv(Price_Risk, "./Tables/Price_Risk.csv")




## Computation of smokers proportion.

Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate( Smoke.Prop = 1 - ( sum(primary_applicant_smoke == 'Non') / length(primary_applicant_smoke) ) )

# Calcul des prix pour les smokers.

Price_Smokers <- Table_Benchmark %>% filter(primary_applicant_smoke == 'Oui' | primary_applicant_smoke == 'oui' )
Price_Smokers <- Price_Smokers %>% group_by(coverage,insurer) %>% summarise(Price.Smokers = mean(price) )
  
write.csv(Price_Smokers, "./Tables/Price_Smokers.csv")





## We remove useless columns and we round.

Table_Benchmark_Global$primary_applicant_age <- as.numeric(Table_Benchmark_Global$primary_applicant_age)

Table_Benchmark_Global_all <- Table_Benchmark_Global # we keep it in this form.

Table_Benchmark_Global <- Table_Benchmark_Global[, c(1,2,4,30,31,32,33,34,35,36,37,39,40,41, 42)]


round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

Table_Benchmark_Global <- round_df(Table_Benchmark_Global,2)


Table_Benchmark_Global <- unique(Table_Benchmark_Global)


write.csv(Table_Benchmark_Global, "./Tables/Table_Benchmark_Global.csv")





## We compute proportion of job status for every insurer.

# jobs_status <- unique(Table_Benchmark_Global_all$primary_applicant_occupation_code)

# names = names(Table_Benchmark_Global_jobs)
# 
# 
# for (job in jobs_status) {
#   job <- as.character(job)
#   nam <- paste("Prop", job, sep=".")
#   Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs %>% group_by(insurer, coverage) %>% mutate(variable = sum( (primary_applicant_occupation_code == job) / length(primary_applicant_occupation_code) ) )
#   names(Table_Benchmark_Global_jobs) <- c(names, nam)
#   names <- names(Table_Benchmark_Global_jobs)
# }

Table_Benchmark_Global_jobs = Table_Benchmark_Global_all


Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs %>% group_by(insurer, coverage, primary_applicant_occupation_code) %>% 
  mutate(Nb.Job = n() )
Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs %>% group_by(insurer, coverage) %>%
  mutate(Nb.Total.Job = n())

Table_Benchmark_Global_jobs$Prop.Job = Table_Benchmark_Global_jobs$Nb.Job / Table_Benchmark_Global_jobs$Nb.Total.Job




Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs[, c(1,2,14,45)]
Table_Benchmark_Global_jobs <- round_df(Table_Benchmark_Global_jobs, 2)
Table_Benchmark_Global_jobs <- unique(Table_Benchmark_Global_jobs)

Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs[order(Table_Benchmark_Global_jobs$coverage),]
Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs[order(Table_Benchmark_Global_jobs$primary_applicant_occupation_code),]

write.csv(Table_Benchmark_Global_jobs, "./Tables/Table_Jobs.csv")


# Now we compute the average price for every profession. 

Table_Benchmark_Global_jobs = Table_Benchmark_Global_all


Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs %>% group_by(insurer, coverage, primary_applicant_occupation_code) %>%
  mutate(Avg.Price = mean(price))

Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs[, c(1,2,14,43)]

Table_Benchmark_Global_jobs <- unique(Table_Benchmark_Global_jobs)

Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs[order(Table_Benchmark_Global_jobs$coverage),]
Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs[order(Table_Benchmark_Global_jobs$primary_applicant_occupation_code),]



write.csv(Table_Benchmark_Global_jobs, "./Tables/Price_by_Job.csv")




## We create a comparative table, to compare price from one period to another without the filtering bias (uniquely the lines
## which are both in 2 periods).


Old_Table <- crawling_all[crawling_all$period %in% "Y17W46",]


Comparative_Table = merge(New_Table, Old_Table, by=c('profilID', 'insurer', 'coverage'), all.x = TRUE, all.y = TRUE)

Comparative_Table = na.omit(Comparative_Table)


Comparative_Table <- Comparative_Table %>% group_by(insurer, coverage) %>% mutate(Avg_Price_1 = mean(price.y) )
Comparative_Table <- Comparative_Table %>% group_by(insurer, coverage) %>% mutate(Avg_Price_2 = mean(price.x) )


Comparative_Table$Var_Price = (Comparative_Table$price.x / Comparative_Table$price.y) - 1


Comparative_Table$Avg_Var_Price <-  ( Comparative_Table$Avg_Price_2  / Comparative_Table$Avg_Price_1 ) - 1


Comparative_Table <- Comparative_Table %>% group_by(insurer, coverage) %>% mutate(Avg_Var_Price_2 = mean((price.x / price.y) - 1) )


Comparative_Table_save <- Comparative_Table
Comparative_Table_all <- merge(Comparative_Table, New_Table_complete, by=c('profilID', 'insurer', 'coverage'), all.x = F, all.y = F)

Comparative_Table <- Comparative_Table[,c(2,3,16,17,19,20)]


Comparative_Table <- unique(Comparative_Table)



Comparative_Table <- Comparative_Table[Comparative_Table$insurer %in% BENCHMARK,]

write.csv(Comparative_Table, "./Tables/Comparative_Table.csv")


# We compare the bias of 'contract_start_days' between insurers.

Comparative_Table_bias <- Comparative_Table_all %>% group_by(insurer, coverage, contract_start_days) %>% mutate(Var.Price = mean(Var_Price))
Comparative_Table_bias <- Comparative_Table_bias[Comparative_Table_bias$insurer %in% BENCHMARK,]
Comparative_Table_bias <- Comparative_Table_bias %>% filter(insurer!='AXA' & insurer!="Zen'up" & insurer!="SIMPL'ASSUR")
Comparative_Table_bias <-  Comparative_Table_bias %>% group_by(coverage, contract_start_days) %>% mutate(Standard = mean(Var.Price))
Comparative_Table_bias$Mean.Var[Comparative_Table_bias$insurer =='Groupe AXA'] = Comparative_Table_bias$Mean.Var[Comparative_Table_bias$insurer =='Groupe AXA'] - 0.07
Comparative_Table_bias$Delta = Comparative_Table_bias$Standard - Comparative_Table_bias$Var.Price



Comparative_Table_bias <- Comparative_Table_bias %>% filter(abs(Delta) > 0.2)



#




## Same, but more precise, for Direct Assurance only.

Old_Table <- crawling_all[crawling_all$period %in% "Y17W46",]


Comparative_Table_DA = merge(New_Table, Old_Table, by=c('profilID', 'insurer', 'coverage'), all.x = TRUE, all.y = TRUE)

Comparative_Table_DA = na.omit(Comparative_Table_DA)


Comparative_Table_DA <- Comparative_Table_DA %>% group_by(insurer, coverage) %>% mutate(Avg_Price_1 = mean(price.y) )
Comparative_Table_DA <- Comparative_Table_DA %>% group_by(insurer, coverage) %>% mutate(Avg_Price_2 = mean(price.x) )


Comparative_Table_DA$Var_Price = (Comparative_Table_DA$price.x / Comparative_Table_DA$price.y) - 1



Comparative_Table_DA <- Comparative_Table_DA %>% group_by(insurer, coverage) %>% mutate(Avg_Var_Price = mean((price.x / price.y) - 1) )

Comparative_Table_DA <- Comparative_Table_DA[,c(1,2,3,4,14,18)]

Comparative_Table_DA <- Comparative_Table_DA[Comparative_Table_DA$insurer %in% "Groupe AXA",]

Comparative_Table_DA <- merge(Comparative_Table_DA, New_Table_complete, by=c('profilID', 'insurer', 'coverage'), all.x = F, all.y = F)


Comparative_Table_DA <- unique(Comparative_Table_DA)

profils_DA <- unique(Comparative_Table_DA$profilID)


write.csv(Comparative_Table_DA, "./Tables/Comparative_Table_DA.csv")




## Plot des fréquences pour chaque segment d'âge.


# for (age in age_segments) {
#   indices <- ( dtrankd$primary_applicant_age >= ( as.numeric ( substring(age, 1, 2) ) ) & dtrankd$primary_applicant_age < ( as.numeric ( str_sub(age, -2) ) ) )
#   dtrankd$age_segment[which(indices)] <- age
# }



Data = New_Table_complete_PCA

table <- NULL
table_age_all <-  NULL
coventity = covfr

for (insurer in BENCHMARK) {
  for (k in 1:length(coventity)) {
    table <- as.data.frame( table( cut( (Data$primary_applicant_age[Data$insurer==insurer & Data$coverage==coventity[k] ]) 
                                       ,breaks=c(18,25,35,45,55,100))) / length(Data$primary_applicant_age[Data$insurer==insurer & Data$coverage==coventity[k] ] ))
    table$coverage = coventity[k]
    table$insurer = insurer
    table_age_all = rbind(table_age_all, table)
  }
}


write.csv(table_age_all, "./Tables/Table_Age.csv")





# Calcul des prix pour chacun des segments d'âge.

segments_age <- c(18,25,35,45,55,100)
Price_by_Age <- New_Table_complete_PCA[,c(1,2,3, 12)]
Price_by_Age <- Price_by_Age[Price_by_Age$insurer %in% BENCHMARK,]
Price_by_Age_all <- Price_by_Age



for (age in seq(1, length(segments_age)-1,1)) {
  Price_by_Age_bis <- Price_by_Age[Price_by_Age$primary_applicant_age  >= segments_age[age] 
                                         & Price_by_Age$primary_applicant_age < segments_age[age+1] ,] %>% 
    group_by(insurer, coverage) %>% mutate(Avg_Price = mean(price))
  
  Price_by_Age_bis <- Price_by_Age_bis[, c(1,2,5)]
  Price_by_Age_bis <- unique(Price_by_Age_bis)
  
  Price_by_Age_all <- merge(Price_by_Age_all, Price_by_Age_bis, by=c('insurer','coverage'), all.x = T, all.y = T)
  
  Price_by_Age_bis <- Price_by_Age

}

unique(Price_by_Age_all$insurer)

Price_by_Age_all <- Price_by_Age_all[,-c(3,4)]
Price_by_Age_all <- unique(Price_by_Age_all)

Price_by_Age_all <- Price_by_Age_all[order(Price_by_Age_all$coverage),]


write.csv(Price_by_Age_all, "./Tables/Price_by_Age.csv")





## Plot des fréquences pour le montant de l'emprunt.


Data = New_Table_complete_PCA
Data$firstloan_amount = as.numeric(Data$firstloan_amount)

table <- NULL
table_amount_all <-  NULL
coventity = 'Formule Optimum'

for (insurer in BENCHMARK) {
  for (k in 1:length(coventity)) {
    table <- as.data.frame( table( cut( (Data$firstloan_amount[grepl(insurer,Data$insurer) & Data$coverage==coventity[k] ]) 
                                        , breaks=c(1e+04, 1e+05, 2e+05, 3e+05, 4.5e+05, 1e+06))) / length(Data$firstloan_amount[grepl(insurer,Data$insurer) &  Data$coverage==coventity[k]]) )
    table$coverage = coventity[k]
    table$insurer = insurer
    table_amount_all = rbind(table_amount_all, table)
  }
}



write.csv(table_amount_all, "./Tables/Table_Amount.csv")



# Calcul des prix pour chaque segment de montant d'emprunt.

segments_amount <- c(1e+04, 1e+05, 2e+05, 3e+05, 4.5e+05, 1e+06)
Price_by_Amount <- New_Table_complete_PCA[,c(1,2,3, 5)]
Price_by_Amount <- Price_by_Amount[Price_by_Amount$insurer %in% BENCHMARK,]
Price_by_Amount_all <- Price_by_Amount



for (amount in seq(1, length(segments_amount)-1,1)) {
  Price_by_Amount_bis <- Price_by_Amount[Price_by_Amount$firstloan_amount  >= segments_amount[amount] 
                                   & Price_by_Amount$firstloan_amount < segments_amount[amount+1] ,] %>% 
    group_by(insurer, coverage) %>% mutate(Avg_Price = mean(price))
  
  Price_by_Amount_bis <- Price_by_Amount_bis[, c(1,2,5)]
  Price_by_Amount_bis <- unique(Price_by_Amount_bis)
  
  Price_by_Amount_all <- merge(Price_by_Amount_all, Price_by_Amount_bis, by=c('insurer','coverage'), all.x = T, all.y = T)
  
  Price_by_Amount_bis <- Price_by_Amount
  
}

unique(Price_by_Amount_all$insurer)

Price_by_Amount_all <- Price_by_Amount_all[,-c(3,4)]
Price_by_Amount_all <- unique(Price_by_Amount_all)

Price_by_Amount_all <- Price_by_Amount_all[order(Price_by_Amount_all$coverage),]


write.csv(Price_by_Amount_all, "./Tables/Price_by_Amount.csv")



## Plot des fréquences pour la durée de l'emprunt.


Data = New_Table_complete_PCA
median(Data$firstloan_duration_years)

table <- NULL
table_duration_all <-  NULL
coventity = covfr

for (insurer in BENCHMARK) {
  for (k in 1:length(coventity)) {
    table <- as.data.frame( table( cut( (Data$firstloan_duration_years[Data$insurer == insurer & Data$coverage==coventity[k] ]) 
                                        , breaks=c(10,15,20,25,30))) / length(Data$firstloan_amount[Data$insurer == insurer &  Data$coverage==coventity[k]]) )
    table$coverage = coventity[k]
    table$insurer = insurer
    table_duration_all = rbind(table_duration_all, table)
  }
}



write.csv(table_duration_all, "./Tables/Table_Duration.csv")




# Calcul des prix pour chaque segment de durée d'emprunt.

segments_duration <- c(10,15,20,25,30)
Price_by_Duration <- New_Table_complete_PCA[,c(1,2,3, 9)]
Price_by_Duration <- Price_by_Duration[Price_by_Duration$insurer %in% BENCHMARK,]
Price_by_Duration_all <- Price_by_Duration



for (duration in seq(1, length(segments_duration)-1,1)) {
  Price_by_Duration_bis <- Price_by_Duration[Price_by_Duration$firstloan_duration_years  >= segments_duration[duration] 
                                         & Price_by_Duration$firstloan_duration_years < segments_duration[duration+1] ,] %>% 
    group_by(insurer, coverage) %>% mutate(Avg_Price = mean(price))
  
  Price_by_Duration_bis <- Price_by_Duration_bis[, c(1,2,5)]
  Price_by_Duration_bis <- unique(Price_by_Duration_bis)
  
  Price_by_Duration_all <- merge(Price_by_Duration_all, Price_by_Duration_bis, by=c('insurer','coverage'), all.x = T, all.y = T)
  
  Price_by_Duration_bis <- Price_by_Duration
  
}

unique(Price_by_Duration_all$insurer)

Price_by_Duration_all <- Price_by_Duration_all[,-c(3,4)]
Price_by_Duration_all <- unique(Price_by_Duration_all)

Price_by_Duration_all <- Price_by_Duration_all[order(Price_by_Duration_all$coverage),]


write.csv(Price_by_Duration_all, "./Tables/Price_by_Duration.csv")
