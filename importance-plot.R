# Packages
library(tidyverse)

# Read importance data frames
mod1 <- read.csv("recorrencia/importance.csv")
mod2 <- read.csv("naodep-severo/importance.csv")
mod3 <- read.csv("severo/importance.csv")

# Change variable names
colnames(mod1) <- c("variable", "importance")
colnames(mod2) <- c("variable", "importance")
colnames(mod3) <- c("variable", "importance")
 # Check structure str(c(mod1, mod2, mod3))

# Change format
mod1_imp <- mod1 %>%
  gather(importance, value, -variable)

mod2_imp <- mod2 %>%
  gather(importance, value, -variable)

mod3_imp <- mod3 %>%
  gather(importance, value, -variable)

# Create fill variable
mod1_imp <- mod1_imp %>% mutate(be_filled = ifelse(variable %in% c("b13tentsu22", "nemtrabnemestuda2"), TRUE, FALSE))
mod2_imp <- mod2_imp %>% mutate(be_filled = ifelse(variable %in% c("nemtrabnemestuda2"), TRUE, FALSE))
mod3_imp <- mod3_imp %>% mutate(be_filled = ifelse(variable %in% c("abepdicotomica2", "b04interna12",
                                                                   "b13tentsu22", "nemtrabnemestuda2",
                                                                   "tpanicoatual2", "a16tratpsic2",
                                                                   "a30interp2", "moracomalgunsdospais2",
                                                                   "teptatual2", "clusterB", "alcoolabudep2",
                                                                   "cigarroabudep2"), TRUE, FALSE))

# Create labels for each plot
labels_mod1 <- c("Childhood Trauma score", "Cluster B", "Cluster C", "Current occupation", "Skin color (non-white)",
                 "Cluster A", "Panic disorder", "Gender (female)", "Specific phobia", "Lives with partner",
                 "Suicide risk", "Paternal suicide attempt", "Marijuana use")

labels_mod2 <- c("Lives with partner", "Childhood Trauma score", "Cluster A", "Current occupation",
                 "Other drugs use", "Marijuana use", "General anxiety disorder", "Gender (female)",
                 "Specific phobia", "Suicide risk")

labels_mod3 <- c("Cluster C", "Cluster B", "Childhood Trauma score", "Age", "Socioeconomic status", "Cluster A",
                 "Years of education", "Social phobia", "Lives with partner", "Lifetime psychological treatment",
                 "Skin color (non-white)", "Gender (female)", "Panic disorder", "Alcohol use", "Lives with parents",
                 "Marijuana use", "Specific phobia", "Current occupation", "Tobacco use", "Suicide risk",
                 "Paternal psychiatric illness", "Other drugs use", "General anxiety disorder",
                 "Post-traumatic stress disorder", "Obsessive-compulsive disorder", "Maternal psychiatric medication",
                 "Maternal psychiatric hospitalization", "Agoraphobia", "Maternal suicide attempt",
                 "Paternal psychiatric medication", "Hypnotics use", "Lifetime hospitalization", "Paternal suicide attempt")

# Create plots for each model
mod1_plot <- ggplot(mod1_imp[(mod1_imp$value>0),], aes(x = reorder(variable, value), y = value, fill = be_filled)) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylab(" ") +
  xlab("Predictor") +
  scale_x_discrete(labels = labels_mod1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14)) +
  theme(legend.position = "none")


mod2_plot <- ggplot(mod2_imp[(mod2_imp$value>0),], aes(x = reorder(variable, value), y = value, fill = be_filled)) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylab("Importance level") +
  xlab(element_blank()) +
  scale_x_discrete(labels = labels_mod2) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14)) +
  theme(legend.position = "none")

mod3_plot <- ggplot(mod3_imp[(mod3_imp$value>0),], aes(x = reorder(variable, value), y = value, fill = be_filled)) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylab(" ") +
  xlab(element_blank()) +
  scale_x_discrete(labels = labels_mod3) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14)) +
  theme(legend.position = "none")

# Use ggarrange to make a grid with all plots
library(ggpubr)
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

preditores <- ggarrange(mod1_plot, mod2_plot, mod3_plot,
                    labels = c("Model 1", "Model 2", "Model 3"),
                    ncol = 3, nrow = 1,
                    font.label = list (size = 12, face = "bold", color = "black"))

# Check the result
preditores
