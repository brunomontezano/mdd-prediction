# Packages
library(tidyverse)

# Read dataset
ds <- read.csv("importance.csv")

# Subset
colnames(ds) <- c("variable", "importance")

str(ds)

ds %>% ggplot(aes(x = variable, y = importance)) + geom_bar()

plot(ds$importance)

imp <- ds %>%
  gather(importance, value, -variable)

ggplot(imp[(imp$value>0),], aes(x = reorder(variable, value), y = value, fill = importance)) +
  geom_col(position = "dodge") +
  coord_flip() +
  ylab("Nível de importância") +
  xlab("Variável preditora") +
  theme(legend.position = "none")
