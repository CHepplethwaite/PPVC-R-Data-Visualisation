# Load required libraries
library(tidyverse)

# -------------------------------
# Example data (replace with your actual dataset)
# -------------------------------
my_df <- data.frame(
  Commodity = rep(c("Rice", "Maize", "Tomato", "Fisheries", "Cocoa"), each = 3),
  Value = c(1.08, 1.05, 1.02, 0.90, 0.95, 0.88, 1.10, 1.12, 1.05, 0.84, 0.80, 0.85, 1.09, 1.07, 1.03),
  Metric = rep(c("Potential for intensification", "Business case", "Final ranking"), 5)
)

# -------------------------------
# 1. Order commodities by 'Potential for intensification'
# -------------------------------
order_df <- my_df %>%
  filter(Metric == "Potential for intensification") %>%
  arrange(Value)

# Set factor levels to control y-axis order
my_df$Commodity <- factor(my_df$Commodity, levels = order_df$Commodity)

# -------------------------------
# 2. Create the Faceted Dot Plot
# -------------------------------
faceted_dot_plot <- ggplot(my_df, aes(x = Value, y = Commodity, color = Metric)) +
  
  # Points
  geom_point(size = 3) +
  
  # Value labels nudged slightly to the right
  geom_text(aes(label = sprintf("%.2f", Value)),
            hjust = -0.2,
            size = 3.5,
            show.legend = FALSE) +
  
  # Facet by Metric
  facet_wrap(~ Metric, scales = "free_x", ncol = 3) +
  
  # Labels and theme
  labs(
    title = "Commodity Performance Across Three Key Metrics",
    x = "Score Value",
    y = NULL, # Remove y-axis label
    color = "Metric"
  ) +
  
  # Color palette
  scale_color_brewer(palette = "Dark2") +
  
  # Minimal theme with custom tweaks
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.spacing = unit(2, "lines"),
    legend.position = "none", # Legend not needed; facets label metrics
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.grid.major.x = element_line(color = "white")
  )

# -------------------------------
# 3. Display the plot
# -------------------------------
print(faceted_dot_plot)

# Optionally, save the plot to file
# ggsave("faceted_dot_plot.png", plot = faceted_dot_plot, width = 12, height = 7)
