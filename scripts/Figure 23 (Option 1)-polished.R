# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Create the data
data <- data.frame(
  Product = c("Cashew", "Sorghum", "Maize", "Soyabean", "Oil palm", "Tomato", 
              "Yam", "Cassava", "Poultry", "Rice", "Fisheries", "Cocoa", 
              "Rubber", "Plantain", "Pineapple"),
  Cost_Use_Ratio = c(0.74, 0.90, 0.94, 0.99, 0.99, 1.01, 1.02, 1.04, 1.04, 
                     1.04, 1.07, 1.07, 1.09, 1.10, 1.10),
  RTA = c(0.39, 0.33, 0.74, 0.53, 1.01, 0.91, 0.28, 0.39, 0.23, 1.10, 
          0.43, 0.17, 0.77, 0.39, 0.71),
  Investment_Support = c(0.67, 0.42, 1.00, 0.50, 0.81, NA, 0.48, 0.55, 0.85, 
                         0.90, 0.76, 1.10, 0.74, 0.10, 0.51)
)

# Convert data to long format for heatmap
data_heatmap <- data %>%
  pivot_longer(cols = -Product, names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = factor(Metric, 
                    levels = c("Cost_Use_Ratio", "RTA", "Investment_Support"),
                    labels = c("Cost/Use\nRatio", "RTA", "Investment\nSupport"))
  )

# Create the heatmap
ggplot(data_heatmap, aes(x = Metric, y = reorder(Product, Value), fill = Value)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = sprintf("%.2f", Value)), 
            color = "white", fontface = "bold", size = 2.5) +  # smaller numbers inside tiles
  scale_fill_gradient2(low = "#2E8B57", mid = "#FFD700", high = "#B22222", 
                       midpoint = 0.75, na.value = "grey90") +
  labs(
    title = "Agricultural Metrics Heatmap",
    x = "",
    y = "",
    fill = "Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(face = "bold", size = 9, hjust = 0.5),  # smaller x-axis labels
    axis.text.y = element_text(face = "bold", size = 9),  # smaller y-axis labels
    legend.position = "right",
    panel.grid = element_blank()
  )
