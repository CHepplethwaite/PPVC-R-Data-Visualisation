# ============================================
# HEATMAP: Best for 9-dimensional data overview
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Embed the data
data <- data.frame(
  Product = c("Goat", "Sorghum", "Plantain", "Soyabean", "Oil palm", 
              "Pineapple", "Rubber", "Yam", "Cashew", "Tomato", 
              "Cassava", "Cocoa", "Fisheries", "Rice", "Maize", "Poultry"),
  Potential_intensification = c(0.10, 1.08, 1.07, 1.08, 0.95, 1.08, 1.09, 1.08, 1.07, 1.08, 1.04, 1.09, 0.84, 1.08, 1.08, 1.10),
  Domestic_consumption = c(0.18, 0.40, 0.52, 0.28, 0.27, 0.51, 0.10, 0.78, 0.93, 1.02, 0.53, 0.84, 0.82, 0.45, 0.67, 1.10),
  Regional_export = c(1.10, 0.15, 0.36, 0.37, 0.43, 0.33, 0.26, 0.35, 0.17, 0.46, 0.33, 0.30, 0.22, 0.28, 0.10, 0.36),
  Cost_use_ratio = c(0.10, 0.90, 1.10, 0.99, 0.99, 1.10, 1.09, 1.02, 0.74, 1.01, 1.04, 1.07, 1.07, 1.04, 0.94, 1.04),
  RTA = c(0.10, 0.33, 0.39, 0.53, 1.01, 0.71, 0.77, 0.28, 0.39, 0.91, 0.39, 0.17, 0.43, 1.10, 0.74, 0.23),
  Scalability = c(0.38, 0.32, 0.26, 0.66, 0.55, 0.74, 0.45, 0.10, 0.59, 0.32, 0.92, 1.01, 0.80, 0.96, 1.10, 0.99),
  Investment_Support = c(0.14, 0.42, 0.10, 0.50, 0.81, 0.51, 0.74, 0.48, 0.67, 0.10, 0.55, 1.10, 0.76, 0.90, 1.00, 0.85),
  Natural_resource = c(0.80, 0.10, 0.71, 0.36, 0.40, 0.44, 0.28, 0.74, 0.79, 0.80, 1.02, 0.63, 0.86, 0.86, 1.01, 1.10),
  Production_volatility = c(0.44, 1.10, 0.39, 0.25, 0.10, 0.14, 1.10, 1.10, 0.70, 0.50, 0.39, 0.21, 0.63, 0.26, 0.31, 1.10)
)

# Prepare data for heatmap
heatmap_data <- data %>%
  pivot_longer(
    cols = -Product,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = factor(Metric,
                    levels = c("Potential_intensification", "Domestic_consumption", "Regional_export",
                               "Cost_use_ratio", "RTA", "Scalability", 
                               "Investment_Support", "Natural_resource", "Production_volatility"),
                    labels = c("Potential for\nIntensification", "Domestic\nConsumption Growth", 
                               "Regional Export\nPotential", "Input Cost/\nUse Ratio", "RTA", 
                               "Scalability", "Investment\nSupport", "Natural Resource\nAvailability",
                               "Production\nVolatility"))
  )

# Create clustered heatmap with dendrogram
library(pheatmap)

# Prepare matrix for clustering
data_matrix <- as.matrix(data[, -1])
rownames(data_matrix) <- data$Product

# Create color palette
color_palette <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(100)

# Create heatmap with clustering
pheatmap(data_matrix,
         color = color_palette,
         scale = "column",  # Standardize by column
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "complete",
         border_color = "white",
         cellwidth = 25,
         cellheight = 15,
         fontsize = 10,
         main = "Comprehensive Agricultural Metrics Heatmap\n(Clustered by Similarity)",
         angle_col = 45,
         display_numbers = FALSE,
         legend = TRUE)