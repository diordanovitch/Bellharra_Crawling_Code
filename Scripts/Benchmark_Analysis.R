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

## Computation of smokers proportion.

Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate( Smoke.Prop = 1 - ( sum(primary_applicant_smoke == 'Non') / length(primary_applicant_smoke) ) )





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

jobs_status <- unique(Table_Benchmark_Global_all$primary_applicant_occupation_code)
Table_Benchmark_Global_jobs = Table_Benchmark_Global_all
names = names(Table_Benchmark_Global_jobs)


for (job in jobs_status) {
  job <- as.character(job)
  nam <- paste("Prop", job, sep=".")
  Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs %>% group_by(insurer, coverage) %>% mutate(variable = sum( (primary_applicant_occupation_code == job) / length(primary_applicant_occupation_code) ) )
  names(Table_Benchmark_Global_jobs) <- c(names, nam)
  names <- names(Table_Benchmark_Global_jobs)
}




# test <- by(data = Table_Benchmark_Global_jobs$primary_applicant_occupation_code, INDICES = Table_Benchmark_Global_jobs$insurer, FUN =  count)



Table_Benchmark_Global_jobs <- Table_Benchmark_Global_jobs[, c(1,2,43:58)]
Table_Benchmark_Global_jobs <- round_df(Table_Benchmark_Global_jobs, 2)


Table_Benchmark_Global_jobs <- unique(Table_Benchmark_Global_jobs)



write.csv(Table_Benchmark_Global_jobs, "./Tables/Table_Jobs.csv")


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



Comparative_Table_all <- merge(Comparative_Table, New_Table_complete, by=c('profilID', 'insurer', 'coverage'), all.x = F, all.y = F)


Comparative_Table <- Comparative_Table[,c(2,3,16,17,19,20)]


Comparative_Table <- unique(Comparative_Table)



Comparative_Table <- Comparative_Table[Comparative_Table$insurer %in% BENCHMARK,]

write.csv(Comparative_Table, "./Tables/Comparative_Table.csv")




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


write.csv(Comparative_Table_DA, "./Tables/Comparative_Table_DA.csv")




## Plot des fréquences pour chaque segment d'âge.


# for (age in age_segments) {
#   indices <- ( dtrankd$primary_applicant_age >= ( as.numeric ( substring(age, 1, 2) ) ) & dtrankd$primary_applicant_age < ( as.numeric ( str_sub(age, -2) ) ) )
#   dtrankd$age_segment[which(indices)] <- age
# }



Data = New_Table_complete_PCA

table <- NULL
table_age_all <-  NULL
coventity = 'Minimum'

for (insurer in BENCHMARK) {
  for (k in 1:length(coventity)) {
    table <- as.data.frame( table( cut( (Data$primary_applicant_age[grepl(insurer,Data$insurer) & Data$coverage==coventity[k] ]) 
                                       ,breaks=c(18,25,35,45,55,100))) / length(Data$primary_applicant_age[grepl(insurer,Data$insurer) &  Data$coverage==coventity[k]]) )
    table$coverage = coventity[k]
    table$insurer = insurer
    table_age_all = rbind(table_age_all, table)
  }
}


write.csv(table_age_all, "./Tables/Table_Age_All.csv")

max(Data$primary_applicant_age[grepl('Groupe AVIVA', Data$insurer)])


# Plot des tarifs et ranking pour chacun de ces segments

segments_age <- c(18,25,35,45,55,100)
Price_by_Age <- New_Table_complete_PCA[,c(1,2,3, 12)]
Price_by_Age <- Price_by_Age[Price_by_Age$insurer %in% BENCHMARK,]
Price_by_Age_all <- Price_by_Age


age= 1


for (age in seq(1, length(segments_age)-1,1)) {
  Price_by_Age_bis <- Price_by_Age[Price_by_Age$primary_applicant_age  >= segments_age[age] 
                                         & Price_by_Age$primary_applicant_age < segments_age[age+1] ,] %>% 
    group_by(insurer, coverage) %>% mutate(Avg_Price = mean(price))
  
  Price_by_Age_bis <- Price_by_Age_bis[, c(1,2,5)]
  Price_by_Age_bis <- unique(Price_by_Age_bis)
  
  Price_by_Age_all <- merge(Price_by_Age_all, Price_by_Age_bis, by=c('insurer','coverage'), all.x = T)
  
  Price_by_Age_bis <- Price_by_Age

}

unique(Price_by_Age_all$insurer)

Price_by_Age_all <- Price_by_Age_all[,-c(3,4)]
Price_by_Age_all <- unique(Price_by_Age_all)

Price_by_Age_all <- Price_by_Age_all[order(Price_by_Age_all$coverage),]


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

min(Data$firstloan_amount)


write.csv(table_amount_all, "./Tables/Table_Amount_All.csv")

View(Comparative_Table)
