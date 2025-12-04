# Load necessary libraries
library(tidyverse)
library(reshape2) # for melting data
library(ggplot2)

# Read CSV
scores <- read.csv("C:/Users/cliff/Documents/ANAPRI/Charts/Table 10/Table 10 Data.csv", stringsAsFactors = FALSE)

# Melt the data for ggplot
scores_long <- melt(scores, id.vars = "Indicator", variable.name = "Commodity", value.name = "Score")

ggplot(scores_long, aes(x = Commodity, y = Indicator, fill = Score)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Commodity Scoring Heatmap", x = "Commodity", y = "Indicator", fill = "Score")
