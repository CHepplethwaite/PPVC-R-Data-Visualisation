# ============================================
# CLEAR VISUALIZATION: Faceted Bar Chart
# ============================================

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Embed the data directly
data <- data.frame(
  Product = c("Yam", "Plantain", "Tomato", "Sorghum", "Goat", "Rubber", 
              "Oil palm", "Cashew", "Soyabean", "Pineapple", "Fisheries", 
              "Cassava", "Rice", "Poultry", "Cocoa", "Maize"),
  Scalability = c(0.10, 0.26, 0.32, 0.32, 0.38, 0.45, 0.55, 0.59, 0.66, 
                  0.74, 0.80, 0.92, 0.96, 0.99, 1.01, 1.10),
  Natural_resource = c(0.74, 0.71, 0.80, 0.10, 0.80, 0.28, 0.40, 0.79, 0.36,
                       0.44, 0.86, 1.02, 0.86, 1.10, 0.63, 1.01),
  Production_volatility = c(1.10, 0.39, 0.50, 1.10, 0.44, 1.10, 0.10, 0.70, 
                            0.25, 0.14, 0.63, 0.39, 0.26, 1.10, 0.21, 0.31)
)

# Reshape data for faceting
data_long <- data %>%
  pivot_longer(
    cols = -Product,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = factor(Metric,
                    levels = c("Scalability", "Natural_resource", "Production_volatility"),
                    labels = c("Scalability", "Natural Resource\nAvailability", "Production\nVolatility"))
  )

# Create faceted bar chart with clear formatting
ggplot(data_long, aes(x = reorder(Product, Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  facet_wrap(~ Metric, ncol = 3) +
  labs(
    title = "Agricultural Metrics Across Products",
    subtitle = "Comparison of Scalability, Natural Resource Availability, and Production Volatility",
    x = "",
    y = "Score (0-1.1 scale)",
    fill = ""
  ) +
  scale_fill_manual(
    values = c("Scalability" = "#1f77b4",
               "Natural Resource\nAvailability" = "#2ca02c",
               "Production\nVolatility" = "#d62728")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1.5, "lines")
  ) +
  coord_flip() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60", alpha = 0.7)