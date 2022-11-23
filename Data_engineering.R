library(tidyverse)
library(DataExplorer)
library(jtools)

setwd("C:/Users/ARFOT/Documents/Actulab_final")
data_shopping <- read.csv2("C:/Users/ARFOT/Documents/Actulab_final/ia_gf_shopping.csv")

str(data_shopping)


nrow(unique(data_shopping)) 

##
##transformer des variables en facteurs :
## 

data_shopping$Sexe <- factor(data_shopping$Sexe)
data_shopping$DecisionFinale <- factor(data_shopping$DecisionFinale)
data_shopping$ReassFinal <- factor(data_shopping$ReassFinal)

## Transformer les IndRe en facteur :
m1 <- which(colnames(data_shopping) == "IndRe1")
m6 <- which(colnames(data_shopping) == "IndRe6")

# Nombre de réassureurs consultés par assuré
data_shopping$num_reas <- rowSums(data_shopping[, m1:m6])

data_shopping[, m1:m6] <- lapply(data_shopping[, m1:m6], factor)
str(data_shopping)

##
## Rechercher les incohérences :
##

#1. Age <= 0 
summary(data_shopping$Age)
sum(data_shopping$Age <= 0)/nrow(data_shopping) # = 0.136 %

#2. Montant = 0 
summary(data_shopping$Montant)
sum(data_shopping$Montant == 0)/nrow(data_shopping) # = 0.701 %

#3. BMIDeclare = 0
summary(data_shopping$BMIDeclare)
sum(data_shopping$BMIDeclare <= 0)/nrow(data_shopping) # = 0.009 %


# Incohérences liées au réassureur final et à IndRei
# (S'il existe un réassureur final, alors il doit avoir été consulté)
  
table(data_shopping$ReassFinal == "Re1", data_shopping$IndRe1) ## 95 cas incohérents
table(data_shopping$ReassFinal == "Re2", data_shopping$IndRe2) ## 110 cas incohérents
table(data_shopping$ReassFinal  == "Re3", data_shopping$IndRe3) ## 145 cas incohérents
table(data_shopping$ReassFinal == "Re4", data_shopping$IndRe4) # 39 cas incohérents
table(data_shopping$ReassFinal  == "Re5", data_shopping$IndRe5) # 29 cas incohérents
table(data_shopping$ReassFinal == "Re6", data_shopping$IndRe6) # 46 cas incohérents
#(au total 0.464 % de lignes incohérentes)

# Incohérences liées au réassureur final et Decision finale 
# (S'il existe un réassureur final, alors la décision finale 
# devrait être RAT (surprimé) ou STD (standard))

table(data_shopping$ReassFinal != "None", data_shopping$DecisionFinale) ## 1.138 % de cas incohérents

# Si aucun réassureur n'a été consulté, alors la décision finale ne devrait pas être une surprime  !!

table(data_shopping$num_reas == 0, data_shopping$DecisionFinale) # aucun problème

# Si au moins un réassureur a été consulté, et personne n'a été retenu, alors la décision finale ne
# devrait pas être standard.

table(data_shopping$ReassFinal == "None" & data_shopping$num_reas != 0, data_shopping$DecisionFinale) # 0.209 % d'incohérence

# Vu que ces incohérences représentent un faible pourcentage de la base de donnée, 
# on va juste les enlever : 

shopping <- data_shopping %>%
  dplyr::filter(Age > 0) %>%
  dplyr::filter(Montant > 0) %>%
  dplyr::filter(BMIDeclare > 0) %>%
  dplyr::filter(!(ReassFinal == "Re1" & IndRe1 == "0")) %>%
  dplyr::filter(!(ReassFinal == "Re2" & IndRe2 == "0")) %>%
  dplyr::filter(!(ReassFinal == "Re3" & IndRe3 == "0")) %>%
  dplyr::filter(!(ReassFinal == "Re4" & IndRe4 == "0")) %>%
  dplyr::filter(!(ReassFinal == "Re5" & IndRe5 == "0")) %>%
  dplyr::filter(!(ReassFinal == "Re6" & IndRe6 == "0")) %>%
  dplyr::filter(!(ReassFinal != "None" & DecisionFinale %in% c("DEC", "PP", "WIT"))) %>%
  dplyr::filter(!(ReassFinal == "None" & num_reas != 0 & DecisionFinale == "STD"))

# Au final, 2.621 % des données ont été retirés de la base 

## J'enlève les conditions médicales les plus rarements présentées

n1 <- which(colnames(shopping) == "Condition1")
n50 <- which(colnames(shopping) == "Condition50")

facLevels <- lapply(shopping[, n1:n50], table)
to_change <- lapply(facLevels, function(x) names(x)[x >= 3])
to_change <- lapply(to_change, as.factor)

to_change2 <- lapply(to_change, function(x) nlevels(x) == 2)

to_change3 <- unlist(to_change2)

shopping <- shopping[, c(1:(n1 - 1), which(to_change3 == T) + n1 - 1) ]

## Nombre de conditions médicales par réassureur final
 
#n1 <- which(colnames(shopping) == "Condition1")
#n50 <- which(colnames(shopping) == "Condition50")

list_cond <- names(to_change3)[which(to_change3 == T)]
n_cond <- length(list_cond)   

data_cond <- data.frame(IA = numeric(n_cond), Re1 = numeric(n_cond), Re2 = numeric(n_cond), Re3 = numeric(n_cond),
                        Re4 = numeric(n_cond), Re5 = numeric(n_cond), Re6 = numeric(n_cond),
                        condition = list_cond)
 
data_cond$IA <- shopping %>% 
  dplyr::filter(ReassFinal == "None") %>%
  select(contains("Condition")) %>%
  colSums()  

data_cond$Re1 <- shopping %>% 
  dplyr::filter(ReassFinal == "Re1") %>%
  select(contains("Condition")) %>%
  colSums() 

data_cond$Re2 <- shopping %>% 
  dplyr::filter(ReassFinal == "Re2") %>%
  select(contains("Condition")) %>%
  colSums() 

data_cond$Re3 <- shopping %>% 
  dplyr::filter(ReassFinal == "Re3") %>%
  select(contains("Condition")) %>%
  colSums() 

data_cond$Re4 <- shopping %>% 
  dplyr::filter(ReassFinal == "Re4") %>%
  select(contains("Condition")) %>%
  colSums() 

data_cond$Re5 <- shopping %>% 
  dplyr::filter(ReassFinal == "Re5") %>%
  select(contains("Condition")) %>%
  colSums() 

data_cond$Re6 <- shopping %>% 
  dplyr::filter(ReassFinal == "Re6") %>%
  select(contains("Condition")) %>%
  colSums() 
 
## Transformer les conditions en facteur  

shopping[, list_cond] <- lapply(shopping[, list_cond], factor)
str(shopping)

# Enregistrer au format .rds
saveRDS(shopping, file = "C:/Users/ARFOT/Documents/Actulab_final/shopping.rds")

 