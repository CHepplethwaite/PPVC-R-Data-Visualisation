# ============================================
# HEATMAP WITH CLUSTERING: Best for 5-metric multivariate data
# ============================================

library(pheatmap)
library(dplyr)
library(tidyr)

# Embed the data
data <- data.frame(
  Value_Chain = c("Soyabean", "Plantain", "Goat", "Sorghum", "Cassava", "Cashew",
                  "Yam", "Rubber", "Poultry", "Oil palm", "Maize", "Cocoa",
                  "Pineapple", "Tomato", "Rice", "Fisheries"),
  Poverty = c(0.41, 0.49, 0.10, 0.52, 0.47, 0.73, 0.50, 0.67, 0.25, 0.22, 0.39, 0.66, 0.91, 1.10, 0.52, 0.41),
  Diets = c(0.11, 0.11, 0.16, 0.13, 0.11, 0.10, 0.12, 0.11, 0.15, 0.20, 0.13, 0.12, 1.10, 0.71, 0.12, 0.18),
  Growth = c(0.23, 0.34, 0.49, 0.52, 0.35, 0.10, 0.39, 0.31, 0.41, 0.70, 0.76, 0.29, 0.44, 0.42, 1.10, 0.67),
  Jobs = c(0.13, 0.31, 0.45, 0.10, 0.37, 0.16, 0.37, 0.23, 0.33, 0.30, 0.35, 0.47, 0.31, 0.32, 0.53, 1.10),
  Policy_support = c(0.48, 0.21, 0.41, 0.48, 0.50, 0.75, 0.48, 0.57, 0.89, 0.76, 0.98, 1.10, 0.10, 0.55, 0.90, 0.81)
)

# Prepare matrix for heatmap
data_matrix <- as.matrix(data[, -1])
rownames(data_matrix) <- data$Value_Chain

# Define column names for display
col_names <- c("Poverty Reduction", "Dietary Impact", "Growth Potential", 
               "Job Creation", "Policy Support")
colnames(data_matrix) <- col_names

# Create custom color palette
color_palette <- colorRampPalette(c("#053061", "#2166ac", "#4393c3", "#92c5de",
                                    "#d1e5f0", "#f7f7f7", "#fddbc7", "#f4a582",
                                    "#d6604d", "#b2182b", "#67001f"))(100)

# Create heatmap with clustering
pheatmap(data_matrix,
         color = color_palette,
         scale = "column",  # Standardize by column for fair comparison
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "ward.D2",
         border_color = "white",
         cellwidth = 35,
         cellheight = 20,
         fontsize_row = 11,
         fontsize_col = 11,
         fontsize = 10,
         main = "Value Chain Performance Across 5 Key Metrics\n(Clustered by Similarity)",
         angle_col = 45,
         display_numbers = TRUE,
         number_format = "%.2f",
         number_color = "black",
         fontsize_number = 8,
         legend = TRUE,
         treeheight_row = 50,
         treeheight_col = 30,
         cutree_rows = 3,  # Cut tree to show 3 clusters
         cutree_cols = 2)  # Cut tree to show 2 metric groups