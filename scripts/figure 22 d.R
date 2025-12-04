# ============================================
# CLEAR VISUALIZATION: Radar Chart for Selected Products
# ============================================

library(ggplot2)
library(dplyr)
library(fmsb)

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

# Select 4 products for comparison
selected_products <- c("Oil palm", "Cocoa", "Rice", "Sorghum")
selected_data <- data[data$Product %in% selected_products, ]

# Prepare data for radar chart
radar_data <- selected_data[, -1]
rownames(radar_data) <- selected_data$Product

# Add max and min rows
max_min <- data.frame(
  Scalability = c(1.2, 0),
  Natural_resource = c(1.2, 0),
  Production_volatility = c(1.2, 0)
)
rownames(max_min) <- c("Max", "Min")

radar_data <- rbind(max_min, radar_data)

# Create radar chart
op <- par(mar = c(1, 2, 2, 1))
radarchart(radar_data,
           axistype = 1,
           pcol = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4"),
           pfcol = c("#FF6B6B60", "#4ECDC460", "#45B7D160", "#96CEB460"),
           plwd = 2,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 1.2, 0.3),
           cglwd = 0.8,
           vlcex = 0.9,
           title = "Radar Chart: Selected Products Comparison")
legend(x = 1.2, y = 1.2, 
       legend = selected_products,
       bty = "n", pch = 20, 
       col = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4"),
       text.col = "black", cex = 0.9, pt.cex = 2)
par(op)