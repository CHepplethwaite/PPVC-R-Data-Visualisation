# Load libraries
library(tidyverse)
library(reshape2)
library(ggplot2)

# Manually create the data
data <- data.frame(
  Value_Chain = c("Rice","Fisheries","Tomato","Maize","Cocoa","Poultry","Pineapple",
                  "Oil palm","Cassava","Cashew","Rubber","Yam","Sorghum","Plantain","Soyabean","Goat"),
  Business = c(3,4,7,2,5,1,11,12,6,8,10,9,15,14,13,16),
  Development = c(2,1,3,6,5,8,4,7,12,11,9,10,13,15,16,14),
  Final_Ranking = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
)

# Melt for ggplot
data_long <- melt(data, id.vars = "Value_Chain", variable.name = "Category", value.name = "Rank")

# Heatmap
ggplot(data_long, aes(x = Category, y = reorder(Value_Chain, Rank), fill = Rank)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradientn(
    colors = c("#d9f0a3", "#5da32e", "#052e56"),  # light green → green → dark blue
    guide = guide_colorbar(barwidth = 15, barheight = 1)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 7),  # slightly smaller commodity names
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) +
  labs(
    title = "Value Chain Rankings Heatmap",
    x = "Category",
    y = "Value Chain",
    fill = "Rank"
  )
