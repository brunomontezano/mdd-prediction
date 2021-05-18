# Packages
library(dplyr)
library(ggplot2)
library(scales)

# Read dataset
ds <- read.csv("predictions.csv")

# Subset
ds <- ds[, 2:4]

str(ds)

# Quintile
quantile(ds$Yes, probs = seq(0, 1, 1/5))

# Create quintile variable
ds <- ds %>%
  mutate(quintiles = factor(ntile(Yes, 5)))

str(ds)

# Factorize outcome
ds$outcome <- as.factor(ds$outcome)

# Bar plot with quintiles
ds_severo <- ds[ds$outcome == "Yes", ]

str(ds_severo)

ds_severo %>%
    ggplot(aes(x = quintiles, fill = quintiles)) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    scale_y_continuous(labels=percent_format()) +
    ylab("All Severe Depression Cases (%)") +
    xlab("Quintile of Predicted Risk") +
    theme(legend.position = "none")
