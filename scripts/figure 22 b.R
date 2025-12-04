library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)  # for unit()

# Embed the data
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

# Reshape for heatmap
heatmap_data <- data %>%
  pivot_longer(
    cols = -Product,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = factor(Metric,
                    levels = c("Scalability", "Natural_resource", "Production_volatility"),
                    labels = c("Scalability", "Natural Resource", "Production Volatility")),
    Value_cat = cut(Value, 
                    breaks = c(0, 0.3, 0.6, 0.9, 1.2),
                    labels = c("Low (0-0.3)", "Medium (0.3-0.6)", 
                               "High (0.6-0.9)", "Very High (0.9-1.2)"))
  )

# Order products by **average score** or a specific metric
avg_scores <- data %>%
  mutate(Avg = rowMeans(select(., Scalability, Natural_resource, Production_volatility))) %>%
  arrange(Avg)

heatmap_data$Product <- factor(heatmap_data$Product, levels = avg_scores$Product)

# Create heatmap
ggplot(heatmap_data, aes(x = Metric, y = Product)) +
  geom_tile(aes(fill = Value), color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", Value)), 
            color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "#f7f7f7",
    high = "#b2182b",
    midpoint = 0.55,
    name = "Score"
  ) +
  labs(
    title = "Agricultural Metrics Heatmap",
    subtitle = "Color intensity indicates score magnitude (darker = higher)",
    x = "",
    y = "Product"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    panel.grid = element_blank()
  )
