## First we create CSP groups


New_Table_complete$primary_applicant_risk[which(New_Table_complete$primary_applicant_risk == 'None')] <- 'Non'
New_Table_complete$primary_applicant_travel[which(New_Table_complete$primary_applicant_travel == 'None')] <- 'Non'


New_Table_complete$CSP <- 0



CSP1_index <- which(New_Table_complete$primary_applicant_risk == 'Non' &
                    New_Table_complete$primary_applicant_travel != 'Plus de 20.000 km par an' & 
      New_Table_complete$primary_applicant_occupation_code != 'Agriculteur' & 
        New_Table_complete$primary_applicant_occupation_code != 'Ouvrier')

New_Table_complete$CSP[CSP1_index] <- 'cSP1'




CSP3_index <- which(New_Table_complete$primary_applicant_risk == 'Oui' | 
                    New_Table_complete$primary_applicant_travel == 'Plus de 20.000 km par an' | 
                    New_Table_complete$primary_applicant_occupation_code == 'Ouvrier' |
                      New_Table_complete$primary_applicant_occupation_code == 'Agriculteur')

New_Table_complete$CSP[CSP3_index] <- 'cSP3'


CSP2_index <- which(New_Table_complete$primary_applicant_risk == 'Non' & 
                      New_Table_complete$primary_applicant_travel == 'Plus de 20.000 km par an' & 
                      New_Table_complete$primary_applicant_occupation_code != 'Ouvrier' &
                      New_Table_complete$primary_applicant_occupation_code != 'Agriculteur')

New_Table_complete$CSP[CSP2_index] <- 'cSP2'



unique(New_Table_complete$CSP)

## Lasso


Table_ZENUP <- New_Table_complete[,-c(1,5,6,7,38)]

Table_ZENUP <- Table_ZENUP %>% filter(insurer=="Zen'up")

y_ZENUP <- Table_ZENUP$price 

y_ZENUP_Opt <- Table_ZENUP %>% filter(coverage=='Formule Optimum')
y_ZENUP_Opt <- y_ZENUP_Opt$price


Table_ZENUP <- Table_ZENUP[,-c(1,3)]

Table_ZENUP_Min <- Table_ZENUP %>% filter(coverage=='Minimum')
Table_ZENUP_Opt <- Table_ZENUP %>% filter(coverage=='Formule Optimum')

Table_ZENUP_Min <- Table_ZENUP_Min[,-c(1, 19:30)]
Table_ZENUP_Opt <- Table_ZENUP_Opt[,-c(1,19:30)]
 


Table_ZENUP_Opt$firstloan_rate <- sub(",", ".", Table_ZENUP_Opt$firstloan_rate)
Table_ZENUP_Opt[,c(2,3,6)] <- sapply(Table_ZENUP_Opt[,c(2,3,6)], as.numeric)


Dummy_ZENUP_Opt <- model.matrix( ~ .-1, Table_ZENUP_Opt[,c(1,4,5,10,12,13,14,15,16,17,18,19)])




fit = glmnet(Dummy_ZENUP_Opt, as.numeric(unlist(y_ZENUP_Opt)), family ="gaussian")


cvfit = cv.glmnet(Dummy_ZENUP_Opt, as.numeric(unlist(y_ZENUP_Opt)), alpha=1)

coef(cvfit, s="lambda.min")


plot(cvfit)

cvfit

cvfit$lambda.min


fit

coef(fit)[,76]



### Tree

# Zen'up

Table_ZENUP <- New_Table_complete[,-c(1,5,6,7,38)]

Table_ZENUP <- Table_ZENUP %>% filter(insurer=="Zen'up")

Table_ZENUP_light <- Table_ZENUP[,c(3,6,14,34)]
Table_ZENUP_light[,c(1,2,3)] <- sapply(Table_ZENUP_light[,c(1,2,3)], as.numeric)
Table_ZENUP_light <- as.data.frame(Table_ZENUP_light)

tree_ZENUP <- rpart(price~., data=Table_ZENUP_light, control=rpart.control(minsplit = 100, cp=0))

prp(tree_ZENUP, extra=1)

plotcp(tree_ZENUP)

sum(Table_ZENUP_light$CSP == 'cSP1')


# DA
Table_DA <- New_Table_complete[,-c(1,5,6,7,38)]

Table_DA <- Table_DA %>% filter(insurer=="Groupe AXA")

Table_DA_light <- Table_DA[,c(3,6,14, 34)]
Table_DA_light <- sapply(Table_DA_light, as.numeric)
Table_DA_light <- as.data.frame(Table_DA_light)

tree_DA <- rpart(price~., data=Table_DA_light, control=rpart.control(minsplit=15))

prp(tree_DA, extra=1)



node1 <- which(Table_DA_light$firstloan_amount<=166e+3 & Table_DA_light$primary_applicant_age<=30)
node2 <- which(Table_DA_light$firstloan_amount>166e+3 & Table_DA_light$primary_applicant_age<=30)
node3 <- which(Table_DA_light$firstloan_amount<=179e+3 & Table_DA_light$primary_applicant_age>30)
node4 <- which(Table_DA_light$firstloan_amount>179e+3 & Table_DA_light$primary_applicant_age>30)
node5 <- which(Table_DA_light$firstloan_amount<=319e+3 & Table_DA_light$primary_applicant_age>40)
node6 <- which(Table_DA_light$firstloan_amount>319e+3 & Table_DA_light$primary_applicant_age<=48)
node7 <- which(Table_DA_light$primary_applicant_age>48)

mean(Table_DA_light$price[node1])
mean(Table_DA_light$price[node2])
mean(Table_DA_light$price[node3])
mean(Table_DA_light$price[node4])
mean(Table_DA_light$price[node5])
mean(Table_DA_light$price[node6])
mean(Table_DA_light$price[node7])

mean(New_Table_complete$firstloan_amount)
