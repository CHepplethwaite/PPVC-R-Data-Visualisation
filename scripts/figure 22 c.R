library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)

# Data
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

# Normalize data for parallel coordinates
data_long <- data %>%
  pivot_longer(cols = -Product, names_to = "Metric", values_to = "Value") %>%
  group_by(Metric) %>%
  mutate(Value_scaled = (Value - min(Value)) / (max(Value) - min(Value))) %>%
  ungroup()

# Keep metric order
data_long$Metric <- factor(data_long$Metric, levels = c("Scalability", "Natural_resource", "Production_volatility"),
                           labels = c("Scalability", "Natural Resource", "Production Volatility"))

# Plot
ggplot(data_long, aes(x = Metric, y = Value_scaled, group = Product, color = Product)) +
  geom_line(alpha = 0.8) +
  geom_point(size = 2) +
  geom_text(
    data = data_long %>% filter(Metric == "Production Volatility"),
    aes(x = 3.05, y = Value_scaled, label = Product),
    hjust = 0, size = 3.5, check_overlap = TRUE, inherit.aes = FALSE
  ) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Parallel Coordinates Plot of Agricultural Metrics",
    x = "Metric",
    y = "Normalized Score"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90")
  )
