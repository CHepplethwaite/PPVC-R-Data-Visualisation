# ============================================
# LINE PLOT: Shows trends across sorted products
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)

# Sort data by average score
sorted_data <- data %>%
  mutate(Average = (Growth + Jobs + Policy_support) / 3) %>%
  arrange(Average)

# Convert to long format for line plot
line_data <- sorted_data %>%
  pivot_longer(
    cols = c(Growth, Jobs, Policy_support),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = factor(Metric, 
                    levels = c("Growth", "Jobs", "Policy_support"),
                    labels = c("Growth", "Jobs", "Policy Support")),
    Product = factor(Product, levels = sorted_data$Product)
  )

# Create line plot with points
ggplot(line_data, aes(x = Product, y = Value, group = Metric, color = Metric)) +
  geom_line(linewidth = 1.2, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.9) +
  labs(
    title = "Metric Trends Across Products",
    subtitle = "Products sorted by average score across all metrics",
    x = "Product (sorted by average score)",
    y = "Score",
    color = "Metric"
  ) +
  scale_color_manual(
    values = c("Growth" = "#2E86AB", 
               "Jobs" = "#A23B72", 
               "Policy Support" = "#F18F01")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "top",
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60", alpha = 0.7) +
  annotate("rect", xmin = 0, xmax = 16.5, ymin = 0.9, ymax = 1.2,
           alpha = 0.1, fill = "#4ECDC4") +
  annotate("text", x = 8, y = 1.15, 
           label = "High Performance Zone", size = 4, fontface = "bold", color = "#2c3e50")