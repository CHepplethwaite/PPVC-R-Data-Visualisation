# Load required libraries
# If you don't have these installed, run: install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

# ====================================================================
# 🔥 STEP 0: EMBEDDED DATA 🔥
# Data transcribed and structured in long format (Commodity, Value, Metric)
# ====================================================================
df <- tibble(
  Commodity = c("Goat", "Fisheries", "Oil palm", "Cassava", "Cashew", "Plantain", "Pineapple", "Tomato", "Rice", "Yam", "Soyabean", "Maize", "Sorghum", "Cocoa", "Rubber", "Poultry", "Rubber", "Goat", "Oil palm", "Soyabean", "Sorghum", "Rice", "Pineapple", "Plantain", "Cassava", "Maize", "Yam", "Fisheries", "Cocoa", "Cashew", "Tomato", "Poultry", "Maize", "Sorghum", "Cashew", "Fisheries", "Rubber", "Rice", "Cocoa", "Pineapple", "Cassava", "Yam", "Plantain", "Poultry", "Soyabean", "Oil palm", "Tomato", "Goat"),
  Value = c(0.10, 0.84, 0.95, 1.04, 1.07, 1.07, 1.08, 1.08, 1.08, 1.08, 1.08, 1.08, 1.08, 1.09, 1.09, 1.10, 0.10, 0.18, 0.27, 0.28, 0.40, 0.45, 0.51, 0.52, 0.53, 0.67, 0.78, 0.82, 0.84, 0.93, 1.02, 1.10, 0.10, 0.15, 0.17, 0.22, 0.26, 0.28, 0.30, 0.33, 0.33, 0.35, 0.36, 0.36, 0.37, 0.43, 0.46, 0.46),
  Metric = c(rep("Potential for intensification", 16), rep("Domestic consumption growth", 16), rep("Regional export potential", 16))
)

# 1. Prepare data for ordering: Order commodities by 'Potential for intensification'
# This ensures a consistent y-axis (Commodity) order across all panels.
order_df <- df %>%
  filter(Metric == "Potential for intensification") %>%
  arrange(Value)

# Apply the new ordering to the Commodity column as a factor
df$Commodity <- factor(df$Commodity, levels = order_df$Commodity)

# 2. Create the Faceted Dot Plot
faceted_dot_plot <- ggplot(df, aes(x = Value, y = Commodity, color = Metric)) +
  
  # Set a light gray background and horizontal lines for the plot panels
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.grid.major.x = element_line(color = "white")
  ) +
  
  # Plot the points
  geom_point(size = 2) +
  
  # Add value labels for precision
  geom_text(aes(label = sprintf("%.2f", Value)),
            hjust = -0.2, 
            size = 3,
            show.legend = FALSE) +
  
  # Separate metrics into facets (panels)
  facet_wrap(~ Metric, scales = "free_x", ncol = 3) +
  
  # Apply labels and colors
  labs(
    title = "Commodity Performance Across Three Key Metrics",
    x = "Score Value",
    y = NULL, 
    color = "Metric"
  ) +
  scale_color_brewer(palette = "Dark2") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.spacing = unit(2, "lines"),
    legend.position = "none"
  )

# 3. Display the plot
print(faceted_dot_plot)