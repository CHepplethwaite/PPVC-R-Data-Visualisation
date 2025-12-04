# ============================================
# GROUPED BAR CHART: Best for 3-metric comparison
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)

# Embed the data
data <- data.frame(
  Product = c("Cashew", "Soyabean", "Cocoa", "Rubber", "Plantain", "Cassava", 
              "Yam", "Poultry", "Tomato", "Pineapple", "Goat", "Sorghum", 
              "Fisheries", "Oil palm", "Maize", "Rice"),
  Growth = c(0.10, 0.23, 0.29, 0.31, 0.34, 0.35, 0.39, 0.41, 0.42, 0.44, 
             0.49, 0.52, 0.67, 0.70, 0.76, 1.10),
  Jobs = c(0.16, 0.13, 0.47, 0.23, 0.31, 0.37, 0.37, 0.33, 0.32, 0.31, 
           0.45, 0.10, 1.10, 0.30, 0.35, 0.53),
  Policy_support = c(0.75, 0.48, 1.10, 0.57, 0.21, 0.50, 0.48, 0.89, 0.55, 
                     0.10, 0.41, 0.48, 0.81, 0.76, 0.98, 0.90)
)

# Convert to long format
data_long <- data %>%
  pivot_longer(
    cols = -Product,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = factor(Metric, 
                    levels = c("Growth", "Jobs", "Policy_support"),
                    labels = c("Growth", "Jobs", "Policy Support"))
  )

# Create grouped bar chart with dodged bars
ggplot(data_long, aes(x = reorder(Product, Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.7, alpha = 0.9) +
  labs(
    title = "Agricultural Sector Metrics Comparison",
    subtitle = "Growth Potential, Job Creation, and Policy Support Scores",
    x = "Product",
    y = "Score (0-1.1 scale)",
    fill = "Metric"
  ) +
  scale_fill_manual(
    values = c("Growth" = "#2E86AB", 
               "Jobs" = "#A23B72", 
               "Policy Support" = "#F18F01")
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
  coord_cartesian(ylim = c(0, 1.2)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60", alpha = 0.7)