# ============================================
# IMPROVED RADAR CHART WITH FACETS
# ============================================

library(fmsb)
library(dplyr)
library(patchwork)

# Sample dataset
data <- read.table(text = "
Value_Chain Poverty Diets Growth Jobs Policy_support
Soyabean 0.41 0.11 0.23 0.13 0.48
Plantain 0.49 0.11 0.34 0.31 0.21
Goat 0.10 0.16 0.49 0.45 0.41
Sorghum 0.52 0.13 0.52 0.10 0.48
Cassava 0.47 0.11 0.35 0.37 0.50
Cashew 0.73 0.10 0.10 0.16 0.75
Yam 0.50 0.12 0.39 0.37 0.48
Rubber 0.67 0.11 0.31 0.23 0.57
Poultry 0.25 0.15 0.41 0.33 0.89
Oil_palm 0.22 0.20 0.70 0.30 0.76
Maize 0.39 0.13 0.76 0.35 0.98
Cocoa 0.66 0.12 0.29 0.47 1.10
Pineapple 0.91 1.10 0.44 0.31 0.10
Tomato 1.10 0.71 0.42 0.32 0.55
Rice 0.52 0.12 1.10 0.53 0.90
Fisheries 0.41 0.18 0.67 1.10 0.81
", header = TRUE)

# ============================================
# Select products to compare
# ============================================
selected_products <- c("Goat", "Oil_palm", "Cocoa", "Fisheries")
selected_data <- data %>% filter(Value_Chain %in% selected_products)

# Prepare data for radar chart
radar_data <- selected_data %>% select(-Value_Chain)
rownames(radar_data) <- selected_data$Value_Chain

# Calculate dynamic max and min for scaling
max_val <- apply(radar_data, 2, max) * 1.1  # 10% buffer above max
min_val <- rep(0, ncol(radar_data))

max_min <- rbind(max_val, min_val)
rownames(max_min) <- c("Max", "Min")

# Combine with product data
radar_data <- rbind(max_min, radar_data)

# Abbreviate column names
colnames(radar_data) <- c("Poverty", "Diets", "Growth", "Jobs", "Policy")

# Define colors
colors_border <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4")
colors_fill <- sapply(colors_border, function(x) adjustcolor(x, alpha.f = 0.25))

# ============================================
# Plot radar charts in facets
# ============================================
par(mfrow = c(2, 2), mar = c(1, 1, 2, 1), oma = c(0, 0, 4, 0))  # 2x2 grid with outer margin for title

for (i in 1:4) {
  product_name <- selected_products[i]
  product_data <- radar_data[c("Max", "Min", product_name), ]
  
  radarchart(product_data,
             axistype = 1,
             pcol = colors_border[i],
             pfcol = colors_fill[i],
             plwd = 2,
             plty = 1,
             cglcol = "grey",
             cglty = 1,
             axislabcol = "grey40",
             caxislabels = seq(0, round(max_val[i], 1), length.out = 5),
             cglwd = 0.8,
             vlcex = 0.8,
             title = gsub("_", " ", product_name),
             cex.main = 1.2)
}

# Reset layout
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# Add overall title
mtext("Value Chain Performance Comparison", 
      side = 3, line = 1, outer = TRUE, cex = 1.5, font = 2)
