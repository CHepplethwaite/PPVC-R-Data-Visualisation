library(tidyverse)

# Example data (replace with your dataset)
my_df <- data.frame(
  Commodity = rep(c("Rice", "Maize", "Tomato", "Fisheries", "Cocoa"), each = 3),
  Value = c(1.08, 1.05, 1.02, 0.90, 0.95, 0.88, 1.10, 1.12, 1.05, 0.84, 0.80, 0.85, 1.09, 1.07, 1.03),
  Metric = rep(c("Potential for intensification", "Business case", "Final ranking"), 5)
)

# Order commodities by 'Potential for intensification'
order_df <- my_df %>%
  filter(Metric == "Potential for intensification") %>%
  arrange(Value)
my_df$Commodity <- factor(my_df$Commodity, levels = order_df$Commodity)

# Faceted dot plot
faceted_dot_plot <- ggplot(my_df, aes(x = Value, y = Commodity, color = Metric)) +
  geom_point(size = 2) +  # point size
  geom_text(aes(label = sprintf("%.2f", Value)),
            hjust = -0.2,      # nudge right
            size = 2.5,        # smaller text
            show.legend = FALSE) +
  facet_wrap(~ Metric, scales = "free_x", ncol = 3) +
  labs(
    title = "Commodity Performance Across Three Key Metrics",
    x = "Score Value",
    y = NULL,
    color = "Metric"
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +  # extra space for labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.grid.major.x = element_line(color = "white"),
    strip.text = element_text(size = 11)
  )

# Display the plot
print(faceted_dot_plot)
