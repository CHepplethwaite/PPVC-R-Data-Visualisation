# ============================================
# DUAL-PLOT: Stacked area + heatmap
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Prepare data for stacked area chart
stacked_data <- data_long %>%
  group_by(Product) %>%
  mutate(
    Total_Score = sum(Value),
    Percentage = Value / Total_Score * 100
  )

# Plot 1: Stacked area chart
p1 <- ggplot(stacked_data, aes(x = reorder(Product, Total_Score), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Total Score Composition",
    x = "",
    y = "Total Score",
    fill = "Metric"
  ) +
  scale_fill_manual(
    values = c("Growth" = "#2E86AB", 
               "Jobs" = "#A23B72", 
               "Policy Support" = "#F18F01")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  coord_flip()

# Plot 2: Heatmap
p2 <- ggplot(data_long, aes(x = Metric, y = reorder(Product, -Value))) +
  geom_tile(aes(fill = Value), color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", Value)), 
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_gradient(
    low = "#2166ac",
    high = "#b2182b",
    name = "Score"
  ) +
  labs(
    title = "Metric Scores by Product",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm")
  )

# Combine plots
combined_plot <- p1 + p2 + 
  plot_layout(ncol = 2, widths = c(1, 1)) +
  plot_annotation(
    title = "Agricultural Sector Analysis: Composition vs. Individual Scores",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  )

print(combined_plot)