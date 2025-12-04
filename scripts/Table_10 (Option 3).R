# ============================================
# HEATMAP WITH RANK ANNOTATION: Best for showing all data
# ============================================

library(pheatmap)
library(dplyr)
library(tidyr)

# Embed the data
data <- data.frame(
  Indicator = c(
    "Potential for intensification", "Domestic cons growth", "Export potential",
    "Input costs/output", "RTA 3-year average", "Investment support",
    "Scalability", "Agro-ecology support", "Production volatility",
    "Poverty alleviation", "Job creation", "Diet quality",
    "Afs growth", "Policy support", "Composite score", "Final Rank"
  ),
  Rice = c(1.08, 0.45, 0.28, 1.04, 1.10, 0.96, 0.90, 0.86, 0.26, 0.52, 0.53, 0.12, 1.10, 0.90, 1.00, 1),
  Fisheries = c(0.84, 0.82, 0.22, 1.07, 0.43, 0.80, 0.76, 0.86, 0.63, 0.41, 1.10, 0.18, 0.67, 0.81, 0.94, 2),
  Tomato = c(1.08, 1.02, 0.46, 1.01, 0.91, 0.32, 0.10, 0.80, 0.50, 1.10, 0.32, 0.71, 0.42, 0.55, 0.90, 3),
  Maize = c(1.08, 0.67, 0.10, 0.94, 0.74, 1.10, 1.00, 1.01, 0.31, 0.39, 0.35, 0.13, 0.76, 0.98, 0.85, 4),
  Cocoa = c(1.09, 0.84, 0.30, 1.07, 0.17, 1.01, 1.10, 0.63, 0.21, 0.66, 0.47, 0.12, 0.29, 1.10, 0.79, 5),
  Poultry = c(1.10, 1.10, 0.36, 1.04, 0.23, 0.99, 0.85, 1.10, 1.10, 0.25, 0.33, 0.15, 0.41, 0.89, 0.79, 6),
  Pineapple = c(1.08, 0.51, 0.33, 1.10, 0.71, 0.74, 0.51, 0.44, 0.14, 0.91, 0.31, 1.10, 0.44, 0.10, 0.76, 7),
  Oil_palm = c(0.95, 0.27, 0.43, 0.99, 1.01, 0.55, 0.81, 0.40, 0.10, 0.22, 0.30, 0.20, 0.70, 0.76, 0.57, 8),
  Cassava = c(1.04, 0.53, 0.33, 1.04, 0.39, 0.92, 0.55, 1.02, 0.39, 0.47, 0.37, 0.11, 0.35, 0.50, 0.54, 9),
  Cashew = c(1.07, 0.93, 0.17, 0.74, 0.39, 0.59, 0.67, 0.79, 0.70, 0.73, 0.16, 0.10, 0.10, 0.75, 0.53, 10),
  Rubber = c(1.09, 0.10, 0.26, 1.09, 0.77, 0.45, 0.74, 0.28, 1.10, 0.67, 0.23, 0.11, 0.31, 0.57, 0.53, 11),
  Yam = c(1.08, 0.78, 0.35, 1.02, 0.28, 0.10, 0.48, 0.74, 1.10, 0.50, 0.37, 0.12, 0.39, 0.48, 0.52, 12),
  Sorghum = c(1.08, 0.40, 0.15, 0.90, 0.33, 0.32, 0.42, 0.10, 1.10, 0.52, 0.10, 0.13, 0.52, 0.48, 0.37, 13),
  Plantain = c(1.07, 0.52, 0.36, 1.10, 0.39, 0.26, 0.10, 0.71, 0.39, 0.49, 0.31, 0.11, 0.34, 0.21, 0.30, 14),
  Soyabean = c(1.08, 0.28, 0.37, 0.99, 0.53, 0.66, 0.50, 0.36, 0.25, 0.41, 0.13, 0.11, 0.23, 0.48, 0.28, 15),
  Goat = c(0.10, 0.18, 1.10, 0.10, 0.10, 0.38, 0.14, 0.80, 0.44, 0.10, 0.45, 0.16, 0.49, 0.41, 0.17, 16)
)

# Prepare matrix for heatmap (excluding Final Rank for separate visualization)
main_data <- data[1:15, ]  # First 15 rows (excluding Final Rank)
main_matrix <- as.matrix(main_data[, -1])
rownames(main_matrix) <- main_data$Indicator

# Get ranking data separately
rank_data <- data[16, -1]
rank_matrix <- as.matrix(rank_data)
rownames(rank_matrix) <- "Final Rank"

# Create color palette
color_palette <- colorRampPalette(c("#053061", "#2166ac", "#4393c3", "#92c5de",
                                    "#d1e5f0", "#f7f7f7", "#fddbc7", "#f4a582",
                                    "#d6604d", "#b2182b", "#67001f"))(100)

# Create main heatmap
main_heatmap <- pheatmap(main_matrix,
                         color = color_palette,
                         scale = "column",
                         cluster_rows = FALSE,
                         cluster_cols = FALSE,
                         border_color = "white",
                         cellwidth = 20,
                         cellheight = 15,
                         fontsize_row = 9,
                         fontsize_col = 10,
                         display_numbers = FALSE,
                         main = "Value Chain Performance Matrix\n(Products ordered by Final Rank)",
                         angle_col = 45,
                         legend = TRUE)

# Create ranking heatmap
rank_heatmap <- pheatmap(rank_matrix,
                         color = colorRampPalette(c("#2E8B57", "#FFD700", "#DC143C"))(16),
                         cluster_rows = FALSE,
                         cluster_cols = FALSE,
                         border_color = "white",
                         cellwidth = 20,
                         cellheight = 15,
                         fontsize = 10,
                         display_numbers = TRUE,
                         number_format = "%d",
                         main = "Final Ranking (1 = Best, 16 = Worst)",
                         legend = FALSE)