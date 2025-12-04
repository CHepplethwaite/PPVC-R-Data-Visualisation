# ============================================
# SIDE-BY-SIDE BAR CHART: Clear comparison of two metrics
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)

# Embed the data
data <- data.frame(
  Product = c("Goat", "Oil palm", "Poultry", "Maize", "Soyabean", "Fisheries", 
              "Cassava", "Plantain", "Yam", "Sorghum", "Rice", "Cocoa", 
              "Rubber", "Cashew", "Pineapple", "Tomato"),
  Poverty = c(0.10, 0.22, 0.25, 0.39, 0.41, 0.41, 0.47, 0.49, 0.50, 0.52, 
              0.52, 0.66, 0.67, 0.73, 0.91, 1.10),
  Diets = c(0.16, 0.20, 0.15, 0.13, 0.11, 0.18, 0.11, 0.11, 0.12, 0.13, 
            0.12, 0.12, 0.11, 0.10, 1.10, 0.71)
)

# Convert to long format
data_long <- data %>%
  pivot_longer(
    cols = c(Poverty, Diets),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = factor(Metric, levels = c("Poverty", "Diets"))
  )

# Create side-by-side bar chart
ggplot(data_long, aes(x = reorder(Product, Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.7, alpha = 0.9) +
  labs(
    title = "Poverty Reduction vs. Dietary Impact",
    subtitle = "Comparative analysis of agricultural products",
    x = "Product",
    y = "Score (0-1.1 scale)",
    fill = "Metric"
  ) +
  scale_fill_manual(
    values = c("Poverty" = "#E41A1C",  # Red for poverty reduction
               "Diets" = "#377EB8"),   # Blue for dietary impact
    labels = c("Poverty Reduction", "Dietary Impact")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60", alpha = 0.7) +
  annotate("rect", xmin = 0, xmax = 16.5, ymin = 0, ymax = 0.25,
           alpha = 0.1, fill = "#E41A1C") +
  annotate("text", x = 8.5, y = 0.125, 
           label = "Low Score Zone", size = 3.5, color = "#E41A1C")