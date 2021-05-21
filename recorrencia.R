### Load packages ###
library(caret)
library(dplyr)
library(purrr)

### Read datasets ###
setwd("/home/pepper/dox/repos/amanda-masters/recorrencia/")
df <- read.csv('../data/banco-conversao-16-10-20.csv')
df_josi <- read.csv('../data/banco-conversao-josi.csv')
df_lu <- read.csv('../data/banco-apesm-3-anos.csv')

### Select 2nd wave observations ###
df <- df %>% filter(., !is.na(mora_t2))

# Rename identification variables ###
colnames(df_josi)[colnames(df_josi) == "a02rec"] <- "rec"
colnames(df_lu)[colnames(df_lu) == "a02rec"] <- "rec"

# Add variables from supplementary datasets ###
df <- left_join(df, df_josi %>%
   dplyr::select(rec, starts_with(c("CTQ", "Abuso", "Negligencia", "cluster")), miniA11, miniA12,
          miniA03ATa, miniA03ATb, miniA03ATc1, miniA03ATc2, miniA03ATd, miniA03ATe1,
          miniA03ATe2, miniA03ATf, miniA03ATg), by = "rec")

df <- left_join(df, df_lu %>%
   dplyr::select(rec, miniC04, miniC05), by = "rec")

### Calculate BDI total score ###
df <- df %>% mutate(bdi_total = rowSums(dplyr::select(., starts_with("BDI"))))

### Remove severely depressed subjects (suicide plan or attempt, or psychotic disorder) ###
df <- df %>% dplyr::filter(., !(miniC04 == 10 | miniC05 == 10 | tpsicoticoatual == 2))

### Dichotomize civil status variable ###

df <- df %>% mutate(., vive_companheiro = case_when(
                    a36relaciona == 1 | a36relaciona == 5 | a36relaciona == 6 ~ 0,
                    a36relaciona == 2 | a36relaciona == 3 | a36relaciona == 4 ~ 1
))

### Create outcome variable ###
df <- df %>% 
      mutate(dep_severa = case_when(
                            # No depression
                            bdi_total <= 13 &
                            TB_erros != 3 ~ 0,
                            # Mild depression
                            bdi_total >= 14 & bdi_total <= 19 &
                            TB_erros != 3 ~ 1,
                            # Moderate depression
                            bdi_total >= 20 & bdi_total <= 28 &
                            TB_erros != 3 ~ 2,
                            # Severe depression
                            bdi_total >= 29 &
                            TB_erros != 3 ~ 3
                      ))

df <- df %>% 
      mutate(dep_dic = case_when(
                            # Mild depression
                            dep_severa == 0  ~ 0,
                            # Severe/moderate depression
                            dep_severa == 3 | dep_severa == 1 | dep_severa == 2 ~ 1
                      ))

### Subset dataset to select outcome and features included in model ###
matrix <- df %>%
          filter(., (dep_dic == 0 | dep_dic == 1)) %>%
          dplyr::select(., dep_dic, a03sexo, a05idade, abepdicotomica, cordapele, escolaridade,
                    vive_companheiro, b01famil1, b04interna1, b03med1, b06tentsu1, b08famil2,
                    b10med2, b13tentsu2, nemtrabnemestuda, tpanicoatual, fobiasocialatual,
                    fobiaespatual, a16tratpsic, a30interp, moracomalgunsdospais,
                    tagatual, teptatual, tocatual, agorafobiaatual, clusterA, clusterB, clusterC, alcoolabudep, maconhaabudep,
                    abudepoutrasdrogas, abudepoutrasdrogasshipnoticos,
                    cigarroabudep, suiciderisk_MINI, CTQ)

### Correct wrong codification ###
matrix$b01famil1[matrix$b01famil1 == 3 | matrix$b01famil1 == 4] <- 1
matrix$b04interna1[matrix$b04interna1 == 3 | matrix$b04interna1 == 4] <- 1
matrix$b03med1[matrix$b03med1 == 3 | matrix$b03med1 == 4] <- 1
matrix$b06tentsu1[matrix$b06tentsu1 == 3] <- 1
matrix$b08famil2[matrix$b08famil2 == 3 | matrix$b08famil2 == 4] <- 1
matrix$b10med2[matrix$b10med2 == 3 | matrix$b10med2 == 4] <- 1
matrix$b13tentsu2[matrix$b13tentsu2 == 3] <- 1

# Legend
# miniA11 - Age on first depressive episode
# miniA12 - How many distinct depressive episodes?
# b01famil1 - Maternal psychiatric illness
# b04interna1 - Maternal psychiatric hospitalization
# b03med1 - Maternal psychiatric medication
# b06tentsu1 - Maternal suicide attempt
# b08famil2 - Paternal psychiatric illness
# b10med2 - Paternal psychiatric medication
# b13tentsu2 - Paternal suicide attempt
# uso_crackandcocaina - Use of crack or cocaine
# nemtrabnemestuda - Current occupation
# a16tratpsic - Lifetime psychological treatment
# a30interp - Lifetime hospitalization
# moracomalgunsdospais - Live with parents

### Integer variables to factor ###
matrix$a03sexo <- as.factor(matrix$a03sexo)
matrix$abepdicotomica <- as.factor(matrix$abepdicotomica)
matrix$cordapele <- as.factor(matrix$cordapele)
matrix$vive_companheiro <- as.factor(matrix$vive_companheiro)
matrix$tagatual <- as.factor(matrix$tagatual)
matrix$teptatual <-  as.factor(matrix$teptatual)
matrix$tocatual <- as.factor(matrix$tocatual)
matrix$agorafobiaatual <- as.factor(matrix$agorafobiaatual)
matrix$alcoolabudep <- as.factor(matrix$alcoolabudep)
matrix$maconhaabudep <- as.factor(matrix$maconhaabudep)
matrix$abudepoutrasdrogas <- as.factor(matrix$abudepoutrasdrogas)
matrix$abudepoutrasdrogasshipnoticos <- as.factor(matrix$abudepoutrasdrogasshipnoticos)
matrix$cigarroabudep <- as.factor(matrix$cigarroabudep)
matrix$suiciderisk_MINI <- as.factor(matrix$suiciderisk_MINI)
matrix$b01famil1 <- as.factor(matrix$b01famil1)
matrix$b04interna1 <- as.factor(matrix$b04interna1)
matrix$b03med1 <- as.factor(matrix$b03med1)
matrix$b06tentsu1 <- as.factor(matrix$b06tentsu1)
matrix$b08famil2 <- as.factor(matrix$b08famil2)
matrix$b10med2 <- as.factor(matrix$b10med2)
matrix$b13tentsu2 <- as.factor(matrix$b13tentsu2)
matrix$nemtrabnemestuda <- as.factor(matrix$nemtrabnemestuda)
matrix$a16tratpsic <- as.factor(matrix$a16tratpsic)
matrix$a30interp <- as.factor(matrix$a30interp)
matrix$moracomalgunsdospais <- as.factor(matrix$moracomalgunsdospais)
matrix$fobiasocialatual <- as.factor(matrix$fobiasocialatual)
matrix$fobiaespatual <- as.factor(matrix$fobiaespatual)
matrix$tpanicoatual <- as.factor(matrix$tpanicoatual)
#matrix$vive_companheiro <- as.factor(matrix$vive_companheiro)

### Label outcome variable ###
matrix$dep_dic <- factor(matrix$dep_dic, labels=c("No", "Yes"))

### Remove additional variables ###
#for_removal <- list("phq_1", "phq_2", "phq_total", "cotas", "suicidal_attempt",
#                    "alcohol", "alcohol_dose", "alcohol_binge", "Risk_Stratification_Alcohol",
#                    "workload", "bullying_school", "psyc_diag", "Transexual")
#matrix <- matrix[,-which(names(matrix) %in% for_removal)]

### Proportions ###
prop.table(table(matrix$dep_dic))

##### Creating Train/Test holdout partitions #####
set.seed(200)
partitions <- createDataPartition(matrix$dep_dic, p=0.75, list=FALSE)
train_matrix <- matrix[partitions,]
test_matrix <- matrix[-partitions,]

### Get mode function ###
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

##### Imputate missing values #####
for (i in names(train_matrix)){
  if (is.factor(train_matrix[,i])) {
    print(i)
    mode_value = getmode(train_matrix[,i])
    #recorded_values[1,i] = mode_value
    train_matrix[is.na(train_matrix[,i]),i] = mode_value
    test_matrix[is.na(test_matrix[,i]),i] = mode_value
  } else {
    print(i)
    mean_value = mean(train_matrix[,i], na.rm=TRUE)
    #recorded_values[1,i] = mean_value
    train_matrix[is.na(train_matrix[,i]),i] = mean_value
    test_matrix[is.na(test_matrix[,i]),i] = mean_value
  }
  
}

### Remove observations with NA values ###
#matrix <- matrix[complete.cases(matrix), ]

##### Outcome Levels #####
# Just checking if the outcome levels of training and test dataset keep the same proportion
prop.table(table(train_matrix$dep_dic))
prop.table(table(test_matrix$dep_dic))


##### Feature Selection #####
# Remove by correlation
#library(polycor)
#correlate <- train_matrix[,-which(names(train_matrix) == "dep_dic")]
#correlationMatrix <- hetcor(correlate)
#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
#print(highlyCorrelated)

# Remove by rfe
#x = train_matrix[,33]
#y = train_matrix[,-33]

##### Training control #####
train_control <- trainControl(method="repeatedcv", number=10, repeats=10, savePredictions=TRUE,
                              classProbs=TRUE, summaryFunction=twoClassSummary)

### Calculating weights ###
f_no = table(train_matrix$dep_dic)[1]
f_yes = table(train_matrix$dep_dic)[2]
w_no = (f_yes)/(f_no+f_yes)
w_yes = (f_no)/(f_no+f_yes)
weights <- ifelse(train_matrix$dep_dic == "No", w_no, w_yes)

### Train model ###
model <- train(dep_dic ~ .,
               data=train_matrix,
               trControl=train_control,
               weights=weights,
               method="glmnet")

### Predictions ###
predictions <- predict(model, test_matrix)
predictions_prob <- predict(model, test_matrix, type="prob")

### Confusion matrix ###
confusionMatrix(predictions, test_matrix$dep_dic, positive="Yes")

### ROC ###
library(pROC)
roc_curve = roc(test_matrix$dep_dic, predictions_prob[, 2], levels=c("Yes","No"))
plot(roc_curve)

### Predictions ###
prepare_risk = predictions_prob
prepare_risk["outcome"] = test_matrix$dep_dic

### Sensitivities and specificities ###
sensitivities = data.frame(roc_curve$sensitivities)
specificities = data.frame(roc_curve$specificities)
write.csv(sensitivities, file="sensitivities.csv")
write.csv(specificities, file="specificities.csv")
write.csv(prepare_risk, file="predictions.csv")

### Most important features arranged ###
varImp(model)$importance %>% as.data.frame() %>% arrange(desc(Overall))# %>% head(5)

### Importance ###
importance = varImp(model)
importance = importance$importance
write.csv(importance, file="importance.csv")
