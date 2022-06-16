# Packages
library(tidyverse)

# Read importance data frames
mod1 <- read.csv("~/dox/repos/amanda-masters/recorrencia/importance.csv")
mod2 <- read.csv("~/dox/repos/amanda-masters/naodep-severo/importance.csv")
mod3 <- read.csv("~/dox/repos/amanda-masters/severo/importance.csv")

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
mod1_imp <- mod1_imp %>% mutate(be_filled = ifelse(variable %in% c(""), TRUE, FALSE))
mod2_imp <- mod2_imp %>% mutate(be_filled = ifelse(variable %in% c("nemtrabnemestuda2"), TRUE, FALSE))
mod3_imp <- mod3_imp %>% mutate(be_filled = ifelse(variable %in% c("moracomalgunsdospais2", "cigarroabudep2"), TRUE, FALSE))
#mod1_imp <- mod1_imp %>% mutate(be_filled = ifelse(variable %in% c("b13tentsu22", "nemtrabnemestuda2"), TRUE, FALSE))
#mod2_imp <- mod2_imp %>% mutate(be_filled = ifelse(variable %in% c("nemtrabnemestuda2"), TRUE, FALSE))
#mod3_imp <- mod3_imp %>% mutate(be_filled = ifelse(variable %in% c("abepdicotomica2", "b04interna12",
#                                                                   "b13tentsu22", "nemtrabnemestuda2",
#                                                                   "tpanicoatual2", "a16tratpsic2",
#                                                                   "a30interp2", "moracomalgunsdospais2",
#                                                                   "teptatual2", "clusterB", "alcoolabudep2",
#                                                                   "cigarroabudep2"), TRUE, FALSE))

# Create labels for each plot
labels_mod1 <- c("Cannabis use (Yes)", "Suicide risk (Yes)", "Cluster A", "Cluster B", "Specific phobia (Yes)")
labels_mod2 <- c("Cluster C", "Childhood Trauma score", "Cluster A", "Does not work/study",
                 "Other drugs use (Yes)", "General anxiety disorder (Yes)", "Cannabis use (Yes)",
                 "Gender (Female)", "Specific phobia (Yes)", "Suicide risk (Yes)")
labels_mod3 <- c("Specific phobia (Yes)", "Age", "Childhood Trauma score", "Tobacco use (Yes)", "Suicide risk (Yes)",
                 "Resides with parents (Yes)", "Cluster A", "Obsessive-compulsive disorder (Yes)",
                 "Other drugs use (Yes)", "Agoraphobia (Yes)", "Paternal psychiatric medication (Yes)")

# Create plots for each model
mod1_plot <- ggplot(mod1_imp[(mod1_imp$value>0),], aes(x = reorder(variable, value), y = value, fill = be_filled)) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylab(" ") +
  xlab("Predictor") +
  scale_x_discrete(labels = labels_mod1) +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11)) +
  theme(legend.position = "none")


mod2_plot <- ggplot(mod2_imp[(mod2_imp$value>0),], aes(x = reorder(variable, value), y = value, fill = be_filled)) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylab("Importance level") +
  xlab(element_blank()) +
  scale_x_discrete(labels = labels_mod2) +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11)) +
  theme(legend.position = "none")

mod3_plot <- ggplot(mod3_imp[(mod3_imp$value>0),], aes(x = reorder(variable, value), y = value, fill = be_filled)) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylab(" ") +
  xlab(element_blank()) +
  scale_x_discrete(labels = labels_mod3) +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11)) +
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
                    font.label = list (size = 13,
                      face = "bold",
                      color = "black"))

# Check the result
preditores

ggsave(
       plot = preditores,
       filename = "~/tmp/figura_arrumada.png",
       dpi = 300
)
