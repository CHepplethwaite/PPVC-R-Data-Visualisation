# Load required libraries
library(tidyverse)
library(ggplot2)

# ====================================================================
# 🔥 STEP 0: EMBEDDED DATA 🔥
# ====================================================================
df <- tibble(
  Commodity = c("Goat", "Fisheries", "Oil palm", "Cassava", "Cashew", "Plantain", "Pineapple", "Tomato", "Rice", "Yam", "Soyabean", "Maize", "Sorghum", "Cocoa", "Rubber", "Poultry",
                "Rubber", "Goat", "Oil palm", "Soyabean", "Sorghum", "Rice", "Pineapple", "Plantain", "Cassava", "Maize", "Yam", "Fisheries", "Cocoa", "Cashew", "Tomato", "Poultry",
                "Maize", "Sorghum", "Cashew", "Fisheries", "Rubber", "Rice", "Cocoa", "Pineapple", "Cassava", "Yam", "Plantain", "Poultry", "Soyabean", "Oil palm", "Tomato", "Goat"),
  Value = c(0.10, 0.84, 0.95, 1.04, 1.07, 1.07, 1.08, 1.08, 1.08, 1.08, 1.08, 1.08, 1.08, 1.09, 1.09, 1.10,
            0.10, 0.18, 0.27, 0.28, 0.40, 0.45, 0.51, 0.52, 0.53, 0.67, 0.78, 0.82, 0.84, 0.93, 1.02, 1.10,
            0.10, 0.15, 0.17, 0.22, 0.26, 0.28, 0.30, 0.33, 0.33, 0.35, 0.36, 0.36, 0.37, 0.43, 0.46, 0.46),
  Metric = c(rep("Potential for intensification", 16),
             rep("Domestic consumption growth", 16),
             rep("Regional export potential", 16))
)

# ====================================================================
# 1. Order commodities by the first metric
# ====================================================================
order_df <- df %>%
  filter(Metric == "Potential for intensification") %>%
  arrange(Value)

df$Commodity <- factor(df$Commodity, levels = order_df$Commodity)

# ====================================================================
# 2. Create safe label map for shorter labels (DO NOT MODIFY df$Metric)
# ====================================================================
metric_labels <- c(
  "Potential for intensification" = "Potential for Intensification",
  "Domestic consumption growth" = "Domestic Consumption Growth",
  "Regional export potential" = "Regional Export Potential"
)

# ====================================================================
# 3. Plot
# ====================================================================
faceted_dot_plot <- ggplot(df, aes(x = Value, y = Commodity, color = Metric)) +
  
  # Light gray panel background
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.grid.major.x = element_line(color = "white")
  ) +
  
  # Points: larger and more visible
  geom_point(size = 2, stroke = 0.8) +
  
  # Label values: larger font
  geom_text(
    aes(label = sprintf("%.2f", Value)),
    hjust = -0.15,
    size = 3,   
    show.legend = FALSE
  ) +
  
  # Facets with relabeling
  facet_wrap(
    ~ Metric,
    scales = "free_x",
    ncol = 3,
    labeller = as_labeller(metric_labels)
  ) +
  
  # Expand x-axis so labels don't get clipped
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.25))) +
  
  # Allow text beyond plot boundaries
  coord_cartesian(clip = "off") +
  
  # Titles
  labs(
    title = "Commodity Performance Across Three Key Metrics",
    x = "Score Value",
    y = NULL
  ) +
  
  # Colors
  scale_color_brewer(palette = "Dark2") +
  
  # Minimal theme foundation
  theme_minimal(base_size = 12) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    panel.spacing = unit(2, "lines"),
    strip.text = element_text(size = 10, face = "bold"), 
    axis.text.y = element_text(size = 9) 
  )

# 4. Display
print(faceted_dot_plot)
