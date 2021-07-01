# TRAIN WITHOUT MODERATE AND TEST WITH MODERATE AND TEST ON MODERATE AND 30% OF MILD/SEVERE
library(caret)
library(dplyr)
library(purrr)

# read dataframe
setwd("/home/pepper/dox/repos/amanda-masters/naodep-severo/")
df <- read.csv('/home/pepper/dox/repos/amanda-masters/data/banco-conversao-16-10-20.csv')
df_josi <- read.csv('/home/pepper/dox/repos/amanda-masters/data/banco-conversao-josi.csv')
df_lu <- read.csv('/home/pepper/dox/repos/amanda-masters/data/banco-apesm-3-anos.csv')

# check dataframe structure
#str(df) 

# filter for 468 from 2nd wave
df <- df %>% filter(., !is.na(mora_t2))

# rename rec variable from 2nd dataset
colnames(df_josi)[colnames(df_josi) == "a02rec"] <- "rec"
colnames(df_lu)[colnames(df_lu) == "a02rec"] <- "rec"

# add variables from another dataset
df <- left_join(df, df_josi %>%
   dplyr::select(rec, starts_with(c("CTQ", "Abuso", "Negligencia", "cluster")), miniA11, miniA12,
          miniA03ATa, miniA03ATb, miniA03ATc1, miniA03ATc2, miniA03ATd, miniA03ATe1,
          miniA03ATe2, miniA03ATf, miniA03ATg), by = "rec")

df <- left_join(df, df_lu %>%
   dplyr::select(rec, miniC04, miniC05), by = "rec")

# create variable of total BDI score
df <- df %>% mutate(bdi_total = rowSums(dplyr::select(., starts_with("BDI"))))

# remove observations with suicide risk and psychotic disorder
df <- df %>% filter(., !(miniC04 == 10 | miniC05 == 10 | tpsicoticoatual == 2))

df <- df %>% mutate(., vive_companheiro = case_when(
                    a36relaciona == 1 | a36relaciona == 5 | a36relaciona == 6 ~ 0,
                    a36relaciona == 2 | a36relaciona == 3 | a36relaciona == 4 ~ 1
))

# create outcome variable
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
# subset dataframe and select variables included in model
matrix <- df %>%
          filter(., (dep_severa == 0 | dep_severa == 3)) %>%
          dplyr::select(., dep_severa, a03sexo, a05idade, abepdicotomica, cordapele, escolaridade,
                    vive_companheiro, b01famil1, b04interna1, b03med1, b06tentsu1, b08famil2,
                    b10med2, b13tentsu2, nemtrabnemestuda,
                    a16tratpsic, a30interp, moracomalgunsdospais, tpanicoatual, fobiaespatual,
                    fobiasocialatual, tagatual, teptatual, tocatual, agorafobiaatual,
                    clusterA, clusterB, clusterC,
                    alcoolabudep, maconhaabudep,
                    abudepoutrasdrogas, abudepoutrasdrogasshipnoticos,
                    cigarroabudep, suiciderisk_MINI, CTQ)
#matrix <- df %>%
#          filter(., (dep_severa == 0 | dep_severa == 2)) %>%
#          select(., dep_severa, a03sexo, a05idade, abepdicotomica, cordapele, escolaridade,
#                    a36relaciona, b01famil1, b04interna1, b03med1, b06tentsu1, b08famil2,
#                    b10med2, b13tentsu2, nemtrabnemestuda,
#                    a16tratpsic, a30interp, moracomalgunsdospais,
#                    tagatual, teptatual, tocatual, agorafobiaatual, clusterA, clusterB, clusterC,
#                    alcoolabudep, maconhaabudep,
#                    alucinogenosabudep, abudepoutrasdrogas, abudepoutrasdrogasshipnoticos,
#                    cigarroabudep, suiciderisk_MINI, AbusoSexual, AbusoFisico, AbusoEmocional,
#                    NegligenciaFisica, NegligenciaEmocional)

# correct wrong codification
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

# label outcome variable
matrix$dep_severa <- factor(matrix$dep_severa, labels=c("No", "Yes"))

#matrix <- matrix %>%
#          select(., -alucinogenosabudep)

### Remove additional variables ###
#column_list <- list("phq_1", "phq_2", "phq_total", "cotas", "suicidal_attempt",
#                    "alcohol", "alcohol_dose", "alcohol_binge", "Risk_Stratification_Alcohol",
#                    "workload", "bullying_school", "psyc_diag", "Transexual")
#matrix <- matrix[,-which(names(matrix) %in% column_list)]

#str(matrix)

# proportions
prop.table(table(matrix$dep_severa))

##### Creating Train/Test holdout partitions #####
set.seed(200)
partitions <- createDataPartition(matrix$dep_severa, p=0.75, list=FALSE)
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
prop.table(table(train_matrix$dep_severa))
prop.table(table(test_matrix$dep_severa))


##### Feature Selection #####
# Remove by correlation (removed alcohol related (> 0.9))
#library(polycor)
#correlate <- train_matrix[,-which(names(train_matrix) == "dep_severa")]
#correlationMatrix <- hetcor(correlate)
#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
#print(highlyCorrelated)

# Remove by rfe
#x = train_matrix[,33]
#y = train_matrix[,-33]

##### Training #####
train_control <- trainControl(method="LOOCV",
                              savePredictions=TRUE,
                              classProbs=TRUE,
                              summaryFunction=twoClassSummary)

f_no = table(train_matrix$dep_severa)[1]
f_yes = table(train_matrix$dep_severa)[2]
w_no = (f_yes)/(f_no+f_yes)
w_yes = (f_no)/(f_no+f_yes)
weights <- ifelse(train_matrix$dep_severa == "No", w_no, w_yes)

model <- train(dep_severa ~ .,
               data=train_matrix,
               trControl=train_control,
               weights=weights,
               method="glmnet",
               tuneGrid = expand.grid(alpha = 0.5,
                                      lambda = 0.1))

moderados <- df %>% filter(., dep_severa == 1 | dep_severa == 2) %>% select(names(matrix))

moderados$b01famil1[moderados$b01famil1 == 3 | moderados$b01famil1 == 4] <- 1
moderados$b04interna1[moderados$b04interna1 == 3 | moderados$b04interna1 == 4] <- 1
moderados$b03med1[moderados$b03med1 == 3 | moderados$b03med1 == 4] <- 1
moderados$b06tentsu1[moderados$b06tentsu1 == 3] <- 1
moderados$b08famil2[moderados$b08famil2 == 3 | moderados$b08famil2 == 4] <- 1
moderados$b10med2[moderados$b10med2 == 3 | moderados$b10med2 == 4] <- 1
moderados$b13tentsu2[moderados$b13tentsu2 == 3] <- 1

moderados$a03sexo <- as.factor(moderados$a03sexo)
moderados$abepdicotomica <- as.factor(moderados$abepdicotomica)
moderados$cordapele <- as.factor(moderados$cordapele)
moderados$vive_companheiro <- as.factor(moderados$vive_companheiro)
moderados$tagatual <- as.factor(moderados$tagatual)
moderados$teptatual <-  as.factor(moderados$teptatual)
moderados$tocatual <- as.factor(moderados$tocatual)
moderados$agorafobiaatual <- as.factor(moderados$agorafobiaatual)
moderados$alcoolabudep <- as.factor(moderados$alcoolabudep)
moderados$maconhaabudep <- as.factor(moderados$maconhaabudep)
moderados$abudepoutrasdrogas <- as.factor(moderados$abudepoutrasdrogas)
moderados$abudepoutrasdrogasshipnoticos <- as.factor(moderados$abudepoutrasdrogasshipnoticos)
moderados$cigarroabudep <- as.factor(moderados$cigarroabudep)
moderados$suiciderisk_MINI <- as.factor(moderados$suiciderisk_MINI)
moderados$b01famil1 <- as.factor(moderados$b01famil1)
moderados$b04interna1 <- as.factor(moderados$b04interna1)
moderados$b03med1 <- as.factor(moderados$b03med1)
moderados$b06tentsu1 <- as.factor(moderados$b06tentsu1)
moderados$b08famil2 <- as.factor(moderados$b08famil2)
moderados$b10med2 <- as.factor(moderados$b10med2)
moderados$b13tentsu2 <- as.factor(moderados$b13tentsu2)
moderados$nemtrabnemestuda <- as.factor(moderados$nemtrabnemestuda)
moderados$a16tratpsic <- as.factor(moderados$a16tratpsic)
moderados$a30interp <- as.factor(moderados$a30interp)
moderados$moracomalgunsdospais <- as.factor(moderados$moracomalgunsdospais)
moderados$fobiasocialatual <- as.factor(moderados$fobiasocialatual)
moderados$fobiaespatual <- as.factor(moderados$fobiaespatual)
moderados$tpanicoatual <- as.factor(moderados$tpanicoatual)

for (i in names(moderados)){
  if (is.factor(moderados[,i])) {
    print(i)
    mode_value = getmode(moderados[,i])
    #recorded_values[1,i] = mode_value
    moderados[is.na(moderados[,i]),i] = mode_value
  } else {
    print(i)
    mean_value = mean(moderados[,i], na.rm=TRUE)
    #recorded_values[1,i] = mean_value
    moderados[is.na(moderados[,i]),i] = mean_value
  }
  
}

moderados_teste <- rbind(moderados, test_matrix)

str(moderados_teste[complete.cases(moderados_teste),])

predictions <- predict(model, test_matrix)
predictions_prob <- predict(model, test_matrix, type="prob")
confusionMatrix(predictions, test_matrix$dep_severa, positive="Yes")

predictions_mod <- predict(model, moderados)
predictions_prob_mod <- predict(model, moderados, type="prob")

### PEGAR A COLUNA DO DIAGNÓSTICO ###

pred_junto <- rbind(predictions_prob_mod, predictions_prob)

diag <- moderados_teste %>% select(dep_severa)
ds_plot <- cbind(pred_junto, diag)

ds_plot <- ds_plot %>% mutate(outcome = case_when(
                                        dep_severa == "No" ~ "Sem depressão",
                                        dep_severa == "Yes" ~ "Severa",
                                        dep_severa == "1" ~ "Leve",
                                        dep_severa == "2" ~ "Moderada"
))

ds_plot <- ds_plot %>% select(No, Yes, outcome)
ds_plot$outcome <- factor(ds_plot$outcome,
                          levels = c("Sem depressão", "Leve", "Moderada", "Severa"),
                          labels = c("Sem depressão", "Leve", "Moderada", "Severa"))

ds_plot <- ds_plot %>% arrange(outcome)

write.csv(ds_plot, "../data/ds_plot_prediction.csv", row.names = FALSE)

#set.seed(666)
#ds_plot %>%
#  mutate(outcome = forcats::fct_relevel(outcome, "Severa", "Moderada", "Leve", "Sem depressão")) %>%
#  ggplot(aes(x = Yes, y = outcome, color = outcome)) +
#  geom_boxplot() +
#  geom_jitter(alpha = 0.7, size = 3, width = 0, height = 0.1) +
#  labs(x = "Predições", y = "Severidade de depressão", color = "Severidade") +
#  theme(legend.position = "none", text = element_text(size = 18))# + coord_flip()

library(pROC)
roc_curve = roc(test_matrix$dep_severa, predictions_prob[, 2], levels=c("Yes","No"))
prepare_risk = predictions_prob
prepare_risk["outcome"] = test_matrix$dep_severa
plot(roc_curve)

sensitivities = data.frame(roc_curve$sensitivities)
specificities = data.frame(roc_curve$specificities)
write.csv(sensitivities, file="sensitivities.csv")
write.csv(specificities, file="specificities.csv")
write.csv(prepare_risk, file="predictions.csv")

varImp(model)$importance %>% as.data.frame() %>% arrange(desc(Overall))
importance = varImp(model)
importance = importance$importance
write.csv(importance, file="importance.csv")

### Check coefficients with direction
as.data.frame(as.matrix(coef(model$finalModel, model$bestTune$lambda)))