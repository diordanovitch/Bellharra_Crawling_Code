### Creation of CSP groups ###

New_Table_complete$primary_applicant_risk[which(New_Table_complete$primary_applicant_risk == 'None')] <- 'Non'
New_Table_complete$primary_applicant_travel[which(New_Table_complete$primary_applicant_travel == 'None')] <- 'Non'


New_Table_complete$CSP <- 0



CSP1_index <- which(New_Table_complete$primary_applicant_risk == 'Non' &
                    New_Table_complete$primary_applicant_travel != 'Plus de 20.000 km par an' & 
      New_Table_complete$primary_applicant_occupation_code != 'Agriculteur' & 
        New_Table_complete$primary_applicant_occupation_code != 'Ouvrier')

New_Table_complete$CSP[CSP1_index] <- 'CSP1'




CSP3_index <- which(New_Table_complete$primary_applicant_risk == 'Oui' | 
                    New_Table_complete$primary_applicant_travel == 'Plus de 20.000 km par an' | 
                    New_Table_complete$primary_applicant_occupation_code == 'Ouvrier' |
                      New_Table_complete$primary_applicant_occupation_code == 'Agriculteur')

New_Table_complete$CSP[CSP3_index] <- 'CSP3'


CSP2_index <- which(New_Table_complete$primary_applicant_risk == 'Non' & 
                      New_Table_complete$primary_applicant_travel == 'Plus de 20.000 km par an' & 
                      New_Table_complete$primary_applicant_occupation_code != 'Ouvrier' &
                      New_Table_complete$primary_applicant_occupation_code != 'Agriculteur')

New_Table_complete$CSP[CSP2_index] <- 'CSP2'



unique(New_Table_complete$CSP)





### Lasso ###


Table_ZENUP <- New_Table_complete[,-c(1,5,6,7,38)]

Table_ZENUP <- Table_ZENUP %>% filter(insurer=="Zen'up")

y_ZENUP <- Table_ZENUP$price 

y_ZENUP_Opt <- Table_ZENUP %>% filter(coverage=='Formule Optimum')
y_ZENUP_Opt <- y_ZENUP_Opt$price


Table_ZENUP <- Table_ZENUP[,-c(1,3)]

Table_ZENUP_Min <- Table_ZENUP %>% filter(coverage=='Minimum')
Table_ZENUP_Opt <- Table_ZENUP %>% filter(coverage=='Formule Optimum')

Table_ZENUP_Min <- Table_ZENUP_Min[,-c(1, 19:30)]
Table_ZENUP_Opt <- Table_ZENUP_Opt[,-c(1, 19:30)]
 


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






### Tree ###

## Zen'up

# Minimum - Complete


Table_ZENUP <- New_Table_complete[,-c(1,5,6,7,38)]
Table_ZENUP <- Table_ZENUP %>% filter(insurer=="Zen'up")
Table_ZENUP <- Table_ZENUP %>% filter(coverage == 'Minimum')
Table_ZENUP_light <- Table_ZENUP[,c(3,6,10,14,17,34)]
names(Table_ZENUP_light) <- c("Prime", "Montant emprunt", "Durée emprunt", "Âge", "Profession", "CSP")
Table_ZENUP_light <- Table_ZENUP_light[,c(1,2,3,4,5,6)]
Table_ZENUP_light[,c(1,2,3)] <- sapply(Table_ZENUP_light[,c(1,2,3)], as.numeric)
Table_ZENUP_light <- as.data.frame(Table_ZENUP_light)
tree_ZENUP_Min <- rpart(Prime~., data=Table_ZENUP_light, control=rpart.control(minsplit = 100, cp=0))
prp(tree_ZENUP_Min, extra=1)
plotcp(tree_ZENUP_Min)


# Minimum - Light

Table_ZENUP_light <- Table_ZENUP[,c(3,6,10,14,17,34)]
names(Table_ZENUP_light) <- c("Prime", "Montant emprunt", "Durée emprunt", "Âge", "Profession", "CSP")
Table_ZENUP_light <- Table_ZENUP_light[,c(1,2,4)]
Table_ZENUP_light[,c(1,2,3)] <- sapply(Table_ZENUP_light[,c(1,2,3)], as.numeric)
Table_ZENUP_light <- as.data.frame(Table_ZENUP_light)
tree_ZENUP_Min <- rpart(Prime~., data=Table_ZENUP_light, control=rpart.control(minsplit = 100, cp=0))
prp(tree_ZENUP_Min, extra=1, fallen.leaves = T)


# Optimum - Complete


Table_ZENUP <- New_Table_complete[,-c(1,5,6,7,38)]
Table_ZENUP <- Table_ZENUP %>% filter(insurer=="Zen'up")
Table_ZENUP <- Table_ZENUP %>% filter(coverage == 'Formule Optimum')
Table_ZENUP_light <- Table_ZENUP[,c(3,6,10,14,17,34)]
names(Table_ZENUP_light) <- c("Prime", "Montant emprunt", "Durée emprunt", "Âge", "Profession" ,"CSP")
Table_ZENUP_light <- Table_ZENUP_light[,c(1,2,3,4,5)]
Table_ZENUP_light[,c(1,2,3)] <- sapply(Table_ZENUP_light[,c(1,2,3)], as.numeric)
Table_ZENUP_light <- as.data.frame(Table_ZENUP_light)
tree_ZENUP_Opt <- rpart(Prime~., data=Table_ZENUP_light, control=rpart.control(minsplit = 50, cp=0))
prp(tree_ZENUP_Opt, extra=1)



# Optimum - Light

Table_ZENUP_light <- Table_ZENUP[,c(3,6,10,14,17,34)]
names(Table_ZENUP_light) <- c("Prime", "Montant emprunt", "Durée emprunt", "Âge", "Profession" ,"CSP")
Table_ZENUP_light <- Table_ZENUP_light[,c(1,2,4)]
Table_ZENUP_light[,c(1,2,3)] <- sapply(Table_ZENUP_light[,c(1,2,3)], as.numeric)
Table_ZENUP_light <- as.data.frame(Table_ZENUP_light)
tree_ZENUP_Opt <- rpart(Prime~., data=Table_ZENUP_light, control=rpart.control(minsplit = 50, cp=0))
prp(tree_ZENUP_Opt, extra=1, fallen.leaves = T)




## Comparaison with DA

#  Minimum - Light

Table_DA <- Table_Benchmark %>% filter(insurer=="Groupe AXA" & coverage == 'Minimum')
Table_Min <- Table_Benchmark %>% filter(coverage == 'Minimum')

node1 <- which(Table_DA$firstloan_amount<=277e+3 & Table_DA$primary_applicant_age<=30)
node2 <- which(Table_DA$firstloan_amount<=177e+3 & Table_DA$primary_applicant_age>30 & Table_DA$primary_applicant_age<=40)
node3 <- which(Table_DA$firstloan_amount>177e+3 & Table_DA$firstloan_amount<=277e+3 & Table_DA$primary_applicant_age>30 & Table_DA$primary_applicant_age<=40)
node4 <- which(Table_DA$firstloan_amount<277e+3 & Table_DA$primary_applicant_age>40 & Table_DA$primary_applicant_age<=48)
node5 <- which(Table_DA$firstloan_amount>277e+3 & Table_DA$primary_applicant_age<=48)
node6 <- which(Table_DA$primary_applicant_age>48)

mean(Table_DA$price[node1])
mean(Table_DA$price[node2])
mean(Table_DA$price[node3])
mean(Table_DA$price[node4])
mean(Table_DA$price[node5])
mean(Table_DA$price[node6])



node1 <- which(Table_Min$firstloan_amount<=277e+3 & Table_Min$primary_applicant_age<=30)
node2 <- which(Table_Min$firstloan_amount<=177e+3 & Table_Min$primary_applicant_age>30 & Table_Min$primary_applicant_age<=40)
node3 <- which(Table_Min$firstloan_amount>177e+3 & Table_Min$firstloan_amount<=277e+3 & Table_Min$primary_applicant_age>30 & Table_Min$primary_applicant_age<=40)
node4 <- which(Table_Min$firstloan_amount<277e+3 & Table_Min$primary_applicant_age>40 & Table_Min$primary_applicant_age<=48)
node5 <- which(Table_Min$firstloan_amount>277e+3 & Table_Min$primary_applicant_age<=48)
node6 <- which(Table_Min$primary_applicant_age>48)


mean(Table_Min$price[node1])
mean(Table_Min$price[node2])
mean(Table_Min$price[node3])
mean(Table_Min$price[node4])
mean(Table_Min$price[node5])
mean(Table_Min$price[node6])


boxplot(Table_Min$price[node1], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Min$price[node1]), labels =fivenum(Table_Min$price[node1]), x=1.28)
boxplot(Table_Min$price[node2], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Min$price[node2]), labels =fivenum(Table_Min$price[node2]), x=1.28)
boxplot(Table_Min$price[node3], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Min$price[node3]), labels =fivenum(Table_Min$price[node3]), x=1.28)
boxplot(Table_Min$price[node4], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Min$price[node4]), labels =fivenum(Table_Min$price[node4]), x=1.28)
boxplot(Table_Min$price[node5], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Min$price[node5]), labels =fivenum(Table_Min$price[node5]), x=1.28)
boxplot(Table_Min$price[node6], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Min$price[node6]), labels =fivenum(Table_Min$price[node6]), x=1.28)



#  Optimum - Light

Table_DA <- Table_Benchmark %>% filter(insurer=="Groupe AXA" & coverage == 'Formule Optimum')
Table_Opt <- Table_Benchmark %>% filter(coverage == 'Formule Optimum')


node1 <- which(Table_DA$firstloan_amount<=265e+3 & Table_DA$primary_applicant_age<=28)
node2 <- which(Table_DA$firstloan_amount<=179e+3 & Table_DA$primary_applicant_age>28 & Table_DA$primary_applicant_age<=40)
node3 <- which(Table_DA$firstloan_amount>179e+3 & Table_DA$firstloan_amount<=265e+3 & Table_DA$primary_applicant_age>28 & Table_DA$primary_applicant_age<=40)
node4 <- which(Table_DA$firstloan_amount<=265e+3 & Table_DA$primary_applicant_age>40 & Table_DA$primary_applicant_age<=50)
node5 <- which(Table_DA$firstloan_amount>265e+3 & Table_DA$primary_applicant_age<=50)
node6 <- which(Table_DA$primary_applicant_age>50)

mean(Table_DA$price[node1])
mean(Table_DA$price[node2])
mean(Table_DA$price[node3])
mean(Table_DA$price[node4])
mean(Table_DA$price[node5])
mean(Table_DA$price[node6])




node1 <- which(Table_Opt$firstloan_amount<=265e+3 & Table_Opt$primary_applicant_age<=28)
node2 <- which(Table_Opt$firstloan_amount<179e+3 & Table_Opt$primary_applicant_age>28 & Table_Opt$primary_applicant_age<=40)
node3 <- which(Table_Opt$firstloan_amount>179e+3 & Table_Opt$firstloan_amount<=265e+3 & Table_Opt$primary_applicant_age>28 & Table_Opt$primary_applicant_age<=40)
node4 <- which(Table_Opt$firstloan_amount<265e+3 & Table_Opt$primary_applicant_age>40 & Table_Opt$primary_applicant_age<=50)
node5 <- which(Table_Opt$firstloan_amount>265e+3 & Table_Opt$primary_applicant_age<=50)
node6 <- which(Table_Opt$primary_applicant_age>50)


mean(Table_Opt$price[node1])
mean(Table_Opt$price[node2])
mean(Table_Opt$price[node3])
mean(Table_Opt$price[node4])
mean(Table_Opt$price[node5])
mean(Table_Opt$price[node6])


boxplot(Table_Opt$price[node1], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Opt$price[node1]), labels =fivenum(Table_Opt$price[node1]), x=1.28)
boxplot(Table_Opt$price[node2], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Opt$price[node2]), labels =fivenum(Table_Opt$price[node2]), x=1.28)
boxplot(Table_Opt$price[node3], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Opt$price[node3]), labels =fivenum(Table_Opt$price[node3]), x=1.28)
boxplot(Table_Opt$price[node4], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Opt$price[node4]), labels =fivenum(Table_Opt$price[node4]), x=1.28)
boxplot(Table_Opt$price[node5], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Opt$price[node5]), labels =fivenum(Table_Opt$price[node5]), x=1.28)
boxplot(Table_Opt$price[node6], horizontal = F, col="blue", outline = F, staplewex = 1, axes = F)
text(y=fivenum(Table_Opt$price[node6]), labels =fivenum(Table_Opt$price[node6]), x=1.28)
