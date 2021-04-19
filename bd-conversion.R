library(caret)
library(dplyr)
library(purrr)

# read dataframe
df <- read.csv('/home/pepper/dox/repos/amanda-masters/data/banco-conversao-16-10-20.csv')
df_josi <- read.csv('/home/pepper/dox/repos/amanda-masters/data/banco-conversao-josi.csv')

# check dataframe structure
#str(df) 

# filter for 468 from 2nd wave
df <- df %>% filter(., !is.na(mora_t2))

# rename rec variable from 2nd dataset
colnames(df_josi)[colnames(df_josi) == "a02rec"] <- "rec"

# add variables from another dataset
df <- left_join(df, df_josi %>%
   select(rec, starts_with("CTQ"), miniA11, miniA12, miniA03ATa, miniA03ATb, miniA03ATc1,
          miniA03ATc2, miniA03ATd, miniA03ATe1, miniA03ATe2, miniA03ATf, miniA03ATg), by = "rec")

# create outcome variable
df <- df %>% 
      mutate(bipolar = case_when(
                    # bipolar
                    TB_erros == 1 | TB_erros == 2 ~ 0,
                    TB_erros == 3 ~ 1
                ))

# subset dataframe and select variables included in model
matrix <- df %>%
          filter(., (bipolar == 0 | bipolar == 1)) %>%
          select(., bipolar, a03sexo, a05idade, abepdicotomica, cordapele, escolaridade,
                    a36relaciona, b01famil1, b04interna1, b03med1, b06tentsu1, b08famil2,
                    b10med2, b13tentsu2, uso_crackandcocaina, nemtrabnemestuda,
                    a16tratpsic, a30interp, moracomalgunsdospais,
                    tagatual, teptatual, tocatual, agorafobiaatual, esquizoide, ansiedade,
                    esquizotipico, paranoide, histrionico, narcisista, borderline, anti_social,
                    evitativo, dependente, compulsivo, alcoolabudep, maconhaabudep,
                    alucinogenosabudep, abudepoutrasdrogas, abudepoutrasdrogasshipnoticos,
                    cigarroabudep, suiciderisk_MINI, CTQ)

# correct wrong codification
matrix$alucinogenosabudep[is.na(matrix$alucinogenosabudep)] <- 2
matrix$b01famil1[matrix$b01famil1 == 3 | matrix$b01famil1 == 4] <- 1
matrix$b04interna1[matrix$b04interna1 == 3 | matrix$b04interna1 == 4] <- 1
matrix$b03med1[matrix$b03med1 == 3 | matrix$b03med1 == 4] <- 1
matrix$b06tentsu1[matrix$b06tentsu1 == 3] <- 1
matrix$b08famil2[matrix$b08famil2 == 3 | matrix$b08famil2 == 4] <- 1
matrix$b10med2[matrix$b10med2 == 3 | matrix$b10med2 == 4] <- 1
matrix$b13tentsu2[matrix$b13tentsu2 == 3] <- 1
#matrix$miniA11[matrix$miniA11 == 99] <- ? 
#matrix$miniA12[matrix$miniA12 == 99 | matrix$miniA12 == 168] <- ? 
#matrix$miniA03ATa[matrix$miniA03ATa == 8] <- 0 
#matrix$miniA03ATb[matrix$miniA03ATb == 8] <- 0 

# counting NA values
#sum(is.na(matrix$esquizoide)) # 60
#sum(is.na(matrix$esquizotipico)) # 60
#sum(is.na(matrix$paranoide)) # 60
#sum(is.na(matrix$histrionico)) # 60
#sum(is.na(matrix$narcisista)) # 60
#sum(is.na(matrix$borderline)) # 60
#sum(is.na(matrix$anti_social)) # 60
#sum(is.na(matrix$evitativo)) # 60
#sum(is.na(matrix$dependente)) # 60
#sum(is.na(matrix$compulsivo)) # 60
#sum(is.na(matrix$tagatual)) # 1
#sum(is.na(matrix$teptatual)) # 1
#sum(is.na(matrix$agorafobiaatual)) # 1
#sum(is.na(matrix$alcoolabudep)) # 27
#sum(is.na(matrix$maconhaabudep)) # 27
#sum(is.na(matrix$alucinogenosabudep)) # 0
#sum(is.na(matrix$cigarroabudep)) # 27
#sum(is.na(matrix$abudepoutrasdrogas)) # 27
#sum(is.na(matrix$abudepoutrasdrogasshipnoticos)) # 27
#sum(is.na(matrix$suiciderisk_MINI)) # 2

# remove variables with 10% or more missings
#matrix <- matrix %>% 
#  discard(~sum(is.na(.x))/length(.x)* 100 >= 10)

# int to factor


# miniA11 - idade primeiro ep. depressivo
# miniA12 - quantos períodos distintos de depressao?
# b01famil1 - mãe doença psi
# b04interna1 - internação materna
# b03med1 - mãe medicamento psi
# b06tentsu1 - tentativa suicídio mãe
# b08famil2 - pai doença psi
# b10med2 - pai medicamento psi
# b13tentsu2 - tentativa suicídio pai
# uso_crackandcocaina - uso crack/cocaina
# nemtrabnemestuda - ocupacao atual
# a16tratpsic - tratamento psicologico ao longo da vida
# a30interp - internação ao longo da vida
# moracomalgunsdospais
matrix$a03sexo <- as.factor(matrix$a03sexo)
matrix$abepdicotomica <- as.factor(matrix$abepdicotomica)
matrix$cordapele <- as.factor(matrix$cordapele)
matrix$a36relaciona <- as.factor(matrix$a36relaciona)
matrix$tagatual <- as.factor(matrix$tagatual)
matrix$teptatual <-  as.factor(matrix$teptatual)
matrix$tocatual <- as.factor(matrix$tocatual)
matrix$agorafobiaatual <- as.factor(matrix$agorafobiaatual)
matrix$alcoolabudep <- as.factor(matrix$alcoolabudep)
matrix$maconhaabudep <- as.factor(matrix$maconhaabudep)
matrix$alucinogenosabudep <- as.factor(matrix$alucinogenosabudep)
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
matrix$uso_crackandcocaina <- as.factor(matrix$uso_crackandcocaina)
matrix$nemtrabnemestuda <- as.factor(matrix$nemtrabnemestuda)
matrix$a16tratpsic <- as.factor(matrix$a16tratpsic)
matrix$a30interp <- as.factor(matrix$a30interp)
matrix$moracomalgunsdospais <- as.factor(matrix$moracomalgunsdospais)

# label outcome variable
matrix$bipolar <- factor(matrix$bipolar, labels=c("No", "Yes"))

#matrix <- matrix %>%
#          select(., -alucinogenosabudep)

### Remove additional variables ###
#column_list <- list("phq_1", "phq_2", "phq_total", "cotas", "suicidal_attempt",
#                    "alcohol", "alcohol_dose", "alcohol_binge", "Risk_Stratification_Alcohol",
#                    "workload", "bullying_school", "psyc_diag", "Transexual")
#matrix <- matrix[,-which(names(matrix) %in% column_list)]

#str(matrix)

# proportions
prop.table(table(matrix$bipolar))

##### Creating Train/Test holdout partitions #####
set.seed(200)
partitions <- createDataPartition(matrix$bipolar, p=0.75, list=FALSE)
train_matrix <- matrix[partitions,]
test_matrix <- matrix[-partitions,]

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

#str(matrix)

# remove observations with NA values
#matrix <- matrix[complete.cases(matrix), ]

##### Outcome Levels #####
# Just checking if the outcome levels of training and test dataset keep the same proportion
prop.table(table(train_matrix$bipolar))
prop.table(table(test_matrix$bipolar))


##### Feature Selection #####
# Remove by correlation (removed alcohol related (> 0.9))
#library(polycor)
#correlate <- train_matrix[,-which(names(train_matrix) == "bipolar")]
#correlationMatrix <- hetcor(correlate)
#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
#print(highlyCorrelated)

# Remove by rfe
#x = train_matrix[,33]
#y = train_matrix[,-33]

##### Training #####
train_control <- trainControl(method="repeatedcv", number=10, repeats=10, savePredictions=TRUE,
                              classProbs=TRUE, summaryFunction=twoClassSummary)

f_no = table(train_matrix$bipolar)[1]
f_yes = table(train_matrix$bipolar)[2]
w_no = (f_yes)/(f_no+f_yes)
w_yes = (f_no)/(f_no+f_yes)
weights <- ifelse(train_matrix$bipolar == "No", w_no, w_yes)

model <- train(bipolar ~ ., data=train_matrix,
                     trControl=train_control, weights=weights, method="glmnet")
predictions <- predict(model, test_matrix)
predictions_prob <- predict(model, test_matrix, type="prob")
confusionMatrix(predictions, test_matrix$bipolar, positive="Yes")
library(pROC)
roc_curve = roc(test_matrix$bipolar, predictions_prob[, 2], levels=c("Yes","No"))
prepare_risk = predictions_prob
prepare_risk["outcome"] = test_matrix$bipolar

sensitivities = data.frame(roc_curve$sensitivities)
specificities = data.frame(roc_curve$specificities)
write.csv(sensitivities, file="/home/pepper/dox/repos/amanda-masters/data/bd-conversion/sensitivities.csv")
write.csv(specificities, file="/home/pepper/dox/repos/amanda-masters/data/bd-conversion/specificities.csv")
write.csv(prepare_risk, file="/home/pepper/dox/repos/amanda-masters/data/bd-conversion/predictions.csv")

varImp(model)$importance %>% as.data.frame() %>% arrange(desc(Overall)) %>% head(5)
importance = varImp(model)
importance = importance$importance
write.csv(importance, file="/home/pepper/dox/repos/amanda-masters/data/bd-conversion/importance.csv")
