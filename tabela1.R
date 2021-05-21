library(dplyr)
library(purrr)
library(tableone)

setwd("/home/pepper/dox/repos/amanda-masters/recorrencia/")
df <- read.csv('../data/banco-conversao-16-10-20.csv')
df_josi <- read.csv('../data/banco-conversao-josi.csv')
df_lu <- read.csv('../data/banco-apesm-3-anos.csv')

df <- df %>% filter(., !is.na(mora_t2))

colnames(df_josi)[colnames(df_josi) == "a02rec"] <- "rec"
colnames(df_lu)[colnames(df_lu) == "a02rec"] <- "rec"

df <- left_join(df, df_josi %>%
   dplyr::select(rec, starts_with(c("CTQ", "Abuso", "Negligencia", "cluster")), miniA11, miniA12,
          miniA03ATa, miniA03ATb, miniA03ATc1, miniA03ATc2, miniA03ATd, miniA03ATe1,
          miniA03ATe2, miniA03ATf, miniA03ATg), by = "rec")

df <- left_join(df, df_lu %>%
   dplyr::select(rec, miniC04, miniC05), by = "rec")

df <- df %>% mutate(bdi_total = rowSums(dplyr::select(., starts_with("BDI"))))

df <- df %>% dplyr::filter(., !(miniC04 == 10 | miniC05 == 10 | tpsicoticoatual == 2))

df <- df %>% mutate(., vive_companheiro = case_when(
                    a36relaciona == 1 | a36relaciona == 5 | a36relaciona == 6 ~ 0,
                    a36relaciona == 2 | a36relaciona == 3 | a36relaciona == 4 ~ 1
))

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

amostra <- df %>% filter(dep_severa == 0 | dep_severa == 1 | dep_severa == 2 | dep_severa == 3)

amostra$a03sexo <- factor(amostra$a03sexo,
levels = c(1,2),
labels = c("Male", "Female"))

amostra$cordapele <- factor(amostra$cordapele,
levels = c(1,2),
labels = c("White", "Non-white"))

amostra$abepdicotomica <- factor(amostra$abepdicotomica,
levels = c(1,2),
labels = c("Lower", "Upper"))

amostra$vive_companheiro <- factor(amostra$vive_companheiro,
levels = c(0,1),
labels = c("No", "Yes"))

amostra$nemtrabnemestuda <- factor(amostra$nemtrabnemestuda,
levels = c(1,2),
labels = c("Do not work/study", "Works/study"))

amostra$suiciderisk_MINI <- factor(amostra$suiciderisk_MINI,
levels = c(0,1),
labels = c("No", "Yes"))

amostra$alcoolabudep <- factor(amostra$alcoolabudep,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$maconhaabudep <- factor(amostra$maconhaabudep,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$alucinogenosabudep <- factor(amostra$alucinogenosabudep,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$abudepoutrasdrogas <- factor(amostra$abudepoutrasdrogas,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$abudepoutrasdrogasshipnoticos <- factor(amostra$abudepoutrasdrogasshipnoticos,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$cigarroabudep <- factor(amostra$cigarroabudep,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$tpanicoatual <- factor(amostra$tpanicoatual,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$fobiasocialatual <- factor(amostra$fobiasocialatual,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$fobiaespatual <- factor(amostra$fobiaespatual,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$tagatual <- factor(amostra$tagatual,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$teptatual <- factor(amostra$teptatual,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$tocatual <- factor(amostra$tocatual,
levels = c(1,2),
labels = c("No", "Yes"))

amostra$dep_severa <- factor(amostra$dep_severa,
levels = c(0,1,2,3),
labels = c("Euthymic depression", "Mild depression", "Moderate depression", "Severe depression"))

list_vars <- c("a03sexo", "a05idade", "cordapele", "escolaridade", "abepdicotomica",
               "vive_companheiro", "nemtrabnemestuda", "suiciderisk_MINI", "alcoolabudep",
               "maconhaabudep", "alucinogenosabudep", "abudepoutrasdrogas",
               "abudepoutrasdrogasshipnoticos", "cigarroabudep", "tpanicoatual",
               "fobiasocialatual", "fobiaespatual", "tagatual", "teptatual", "tocatual")

cat_vars <- c("a03sexo", "cordapele", "abepdicotomica", "vive_companheiro", "nemtrabnemestuda",
              "suiciderisk_MINI", "alcoolabudep", "maconhaabudep", "alucinogenosabudep",
              "abudepoutrasdrogas", "abudepoutrasdrogasshipnoticos", "cigarroabudep",
              "tpanicoatual", "fobiasocialatual", "fobiaespatual", "tagatual", "teptatual",
              "tocatual")

table1 <- CreateTableOne(vars = list_vars,
                         data = amostra,
                         factorVars = cat_vars,
                         strata = "dep_severa")

table1_print <- print(table1, showAllLevels = TRUE)

setwd("/home/pepper/dox/repos/amanda-masters/")
write.csv(table1_print, file = "table1.csv")

library(table1)

label(amostra$a03sexo) <- "Sex"
label(amostra$a05idade) <- "Age"
units(amostra$a05idade) <- "years"
label(amostra$cordapele) <- "Skin color"
label(amostra$escolaridade) <- "Years of education"
label(amostra$abepdicotomica) <- "Socioeconomic status"
label(amostra$vive_companheiro) <- "Lives with partner"
label(amostra$nemtrabnemestuda) <- "Current occupation"
label(amostra$suiciderisk_MINI) <- "Suicide risk"
label(amostra$alcoolabudep) <- "Alcohol abuse/dependence"
label(amostra$maconhaabudep) <- "Marijuana abuse/dependence"
label(amostra$alucinogenosabudep) <- "Hallucinogenics abuse/dependence"
label(amostra$abudepoutrasdrogas) <- "Other drugs abuse/dependence"
label(amostra$abudepoutrasdrogasshipnoticos) <- "Hypnotics abuse/dependence"
label(amostra$cigarroabudep) <- "Tobacco abuse/dependence"
label(amostra$tpanicoatual) <- "Panic disorder"
label(amostra$fobiasocialatual) <- "Social phobia"
label(amostra$fobiaespatual) <- "Specific phobia"
label(amostra$tagatual) <- "Generalized anxiety disorder"
label(amostra$teptatual) <- "Posttraumatic stress disorder"
label(amostra$tocatual) <- "Obsessive-compulsive disorder"

pvalue <- function(x, ...) {
    # Construct vectors of data y, and groups (strata) g
    y <- unlist(x)
    g <- factor(rep(1:length(x), times=sapply(x, length)))
    if (is.numeric(y)) {
        # For numeric variables, perform a standard 2-sample t-test
        p <- summary(aov(y ~ g))[[1]][["Pr(>F)"]][[1]]
    } else {
        # For categorical variables, perform a chi-squared test of independence
        p <- chisq.test(table(y, g))$p.value
    }
    # Format the p-value, using an HTML entity for the less-than sign.
    # The initial empty string places the output on the line below the variable label.
    c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

options(browser = "brave")

table1(~ a03sexo + a05idade + cordapele + escolaridade + abepdicotomica +
         vive_companheiro + nemtrabnemestuda + suiciderisk_MINI + alcoolabudep +
         maconhaabudep + alucinogenosabudep + abudepoutrasdrogas +
         abudepoutrasdrogasshipnoticos + cigarroabudep + tpanicoatual + fobiasocialatual + 
         fobiaespatual + tagatual + teptatual + tocatual | dep_severa,
         data=amostra,
         extra.col=list(`p-value`=pvalue))

library(furniture)

table1(amostra,
       "Sex" = a03sexo, "Age" = a05idade, "Skin color" = cordapele,
       "Years of education" = escolaridade, "Socioeconomic status" = abepdicotomica,
       "Lives with partner" = vive_companheiro, "Current occupation" = nemtrabnemestuda,
       "Suicide risk" = suiciderisk_MINI, "Alcohol abuse/dependence" = alcoolabudep,
       "Marijuana abuse/dependence" = maconhaabudep,
       #"Hallucinogenics abuse/dependence" = alucinogenosabudep,
       "Other drugs abuse/dependence" = abudepoutrasdrogas,
       "Hypnotics abuse/dependence" = abudepoutrasdrogasshipnoticos,
       "Tobacco abuse/dependence" = cigarroabudep,
       "Panic disorder" = tpanicoatual,
       "Social phobia" = fobiasocialatual, "Specific phobia" = fobiaespatual,
       "Generalized anxiety disorder" = tagatual,
       "Posttraumatic stress disorder" = teptatual,
       "Obsessive-compulsive disorder" = tocatual,
       splitby = ~dep_severa,
       test = TRUE,
       param = TRUE,
       export = "secondtry")

