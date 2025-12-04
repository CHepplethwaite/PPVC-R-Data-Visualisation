# ============================================
# SCATTER PLOT: Poverty vs. Diets with quadrant analysis
# ============================================

library(ggplot2)
library(ggrepel)

# Calculate quadrant boundaries
poverty_median <- median(data$Poverty)
diets_median <- median(data$Diets)

# Create scatter plot with quadrants
ggplot(data, aes(x = Poverty, y = Diets)) +
  geom_point(aes(size = abs(Poverty - Diets), color = Poverty * Diets), 
             alpha = 0.8, stroke = 1.5) +
  geom_text_repel(aes(label = Product), 
                  size = 4,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.color = 'grey50',
                  max.overlaps = 20) +
  scale_size_continuous(
    range = c(4, 12),
    name = "Score Difference\n(Poverty - Diets)",
    breaks = c(0.1, 0.3, 0.6)
  ) +
  scale_color_gradient(
    low = "#FF6B6B",
    high = "#4ECDC4",
    name = "Composite Score\n(Poverty × Diets)"
  ) +
  # Add quadrant lines
  geom_vline(xintercept = poverty_median, 
             linetype = "dashed", color = "gray60", alpha = 0.7) +
  geom_hline(yintercept = diets_median, 
             linetype = "dashed", color = "gray60", alpha = 0.7) +
  # Annotate quadrants
  annotate("text", x = 0.15, y = 0.85, 
           label = "Low Poverty\nHigh Diets", 
           size = 4, fontface = "bold", color = "#2E8B57") +
  annotate("text", x = 0.85, y = 0.85, 
           label = "High Poverty\nHigh Diets", 
           size = 4, fontface = "bold", color = "#FFD700") +
  annotate("text", x = 0.15, y = 0.15, 
           label = "Low Poverty\nLow Diets", 
           size = 4, fontface = "bold", color = "#6495ED") +
  annotate("text", x = 0.85, y = 0.15, 
           label = "High Poverty\nLow Diets", 
           size = 4, fontface = "bold", color = "#DC143C") +
  labs(
    title = "Poverty Reduction vs. Dietary Impact: Quadrant Analysis",
    subtitle = "Products positioned by their dual impact potential",
    x = "Poverty Reduction Score",
    y = "Dietary Impact Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  coord_cartesian(xlim = c(0, 1.2), ylim = c(0, 1.2))