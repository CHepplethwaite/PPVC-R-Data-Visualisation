# ============================================
# LADDER PLOT: Showing the gap between Poverty and Diets scores
# ============================================

library(ggplot2)
library(dplyr)

# Calculate difference and prepare data
data_diff <- data %>%
  mutate(
    Difference = Poverty - Diets,
    Direction = ifelse(Difference > 0, "Poverty > Diets", 
                       ifelse(Difference < 0, "Diets > Poverty", "Equal")),
    Product = factor(Product, levels = data$Product[order(data$Poverty)])
  ) %>%
  arrange(Poverty)

# Create ladder plot
ggplot(data_diff) +
  # Segments connecting Poverty and Diets
  geom_segment(aes(x = Poverty, xend = Diets, 
                   y = reorder(Product, Poverty), yend = reorder(Product, Poverty),
                   color = Direction),
               linewidth = 2, alpha = 0.7) +
  # Poverty points
  geom_point(aes(x = Poverty, y = reorder(Product, Poverty)), 
             color = "#E41A1C", size = 4, alpha = 0.9) +
  # Diets points
  geom_point(aes(x = Diets, y = reorder(Product, Poverty)), 
             color = "#377EB8", size = 4, alpha = 0.9) +
  # Labels for points
  geom_text(aes(x = Poverty, y = reorder(Product, Poverty), 
                label = sprintf("%.2f", Poverty)),
            hjust = -0.3, size = 3.5, fontface = "bold", color = "#E41A1C") +
  geom_text(aes(x = Diets, y = reorder(Product, Poverty), 
                label = sprintf("%.2f", Diets)),
            hjust = 1.3, size = 3.5, fontface = "bold", color = "#377EB8") +
  scale_color_manual(
    values = c("Poverty > Diets" = "#E41A1C", 
               "Diets > Poverty" = "#377EB8", 
               "Equal" = "gray50")
  ) +
  labs(
    title = "Poverty vs. Diets: Score Comparison",
    subtitle = "Red = Poverty Reduction, Blue = Dietary Impact | Line shows difference",
    x = "Score",
    y = "Product (sorted by Poverty score)",
    color = "Which is higher?"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  scale_x_continuous(limits = c(-0.1, 1.2)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray60", alpha = 0.7)