library(fmsb)
library(dplyr)

# Select products
selected_products <- c("Goat", "Cocoa", "Oil palm", "Poultry")
selected_data <- data[data$Product %in% selected_products, ]

# Metrics we have
metrics <- c("Scalability", "Natural_resource", "Production_volatility")

# Prepare radar data
radar_data <- selected_data[, metrics]
rownames(radar_data) <- selected_data$Product

# Add max and min rows (for scaling)
max_min <- data.frame(
  Scalability = c(1.2, 0),
  Natural_resource = c(1.2, 0),
  Production_volatility = c(1.2, 0)
)
rownames(max_min) <- c("Max", "Min")

radar_data <- rbind(max_min, radar_data)

# Colors
colors_border <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4")
colors_fill <- c("#FF6B6B80", "#4ECDC480", "#45B7D180", "#96CEB480")

# Create radar chart
par(mar = c(0, 0, 3, 0))
radarchart(radar_data,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_fill,
           plwd = 3,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey40",
           caxislabels = seq(0, 1.2, 0.3),
           cglwd = 0.8,
           vlcex = 0.9,
           title = "Selected Products Comparison")

# Add legend
legend(x = 1.3, y = 1.2, 
       legend = selected_products,
       bty = "n", pch = 20, 
       col = colors_border,
       text.col = "black", 
       cex = 1.1, 
       pt.cex = 3,
       title = "Products")
