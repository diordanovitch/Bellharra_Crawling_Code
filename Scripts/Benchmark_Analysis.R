library("dplyr")

## We chose the companies we want to compare

BENCHMARK <-c("MAAF Assurances","Cardif","Groupe AVIVA", "AXA", "SIMPL'ASSUR","Groupe AXA","Zen'up")


## We create the table for the benchmark, from New_Table_Complete created for PCA.

Table_Benchmark <- New_Table_complete_PCA[New_Table_complete_PCA$insurer %in% BENCHMARK,]



## Part of single applicant compared to sharing loan.

sum(Table_Benchmark$primary_applicant_share == 1) / length(Table_Benchmark$primary_applicant_share)






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

Table_Benchmark_Global_all <- Table_Benchmark_Global # we keep it in this form.

Table_Benchmark_Global <- Table_Benchmark_Global[, c(1,2,4,30,31,32,33,34,35,36,37,39,40,41, 42)]

for (column in c(4,5,6,7,8,9,10,11,12,13,14,15)) {
  Table_Benchmark_Global[,column] <- round(Table_Benchmark_Global[,column], 2)
}

Table_Benchmark_Global <- unique(Table_Benchmark_Global)


write.csv(Table_Benchmark_Global, "./Tables/Table_Benchmark_Global.csv")



## We compute proportion of job status for every insurer.

jobs_status <- unique(Table_Benchmark_Global_all$primary_applicant_occupation_code)

i<- 1

for (job in jobs_status) {
  job <- as.character(job)
  variable <- sprintf('Prop.%s', job)
  Table_Benchmark_Global_jobs <- Table_Benchmark_Global_all %>% group_by(insurer, coverage) %>% mutate( variable = sum( (primary_applicant_occupation_code == job) / length(primary_applicant_occupation_code) ) )
  i <- i+1
}


Table_Benchmark_Global <- Table_Benchmark_Global %>% group_by(insurer, coverage) %>% mutate( Prop.Loan.Type = sum(firstloan_type == 'Amortissable') / length(firstloan_type) )

assi