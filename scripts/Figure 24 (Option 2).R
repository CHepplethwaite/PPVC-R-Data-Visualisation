# ============================================
# BUBBLE CHART: Shows relationships between all 3 metrics
# ============================================

library(ggplot2)
library(ggrepel)

# Create bubble chart
ggplot(data, aes(x = Growth, y = Jobs, size = Policy_support, color = Policy_support)) +
  geom_point(alpha = 0.8, stroke = 1.5) +
  geom_text_repel(aes(label = Product), 
                  size = 4,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  segment.color = 'grey50',
                  max.overlaps = 20) +
  scale_size_continuous(
    range = c(4, 15),
    name = "Policy Support\n(size)",
    breaks = c(0.1, 0.5, 1.0),
    labels = c("Low", "Medium", "High")
  ) +
  scale_color_gradient(
    low = "#FF6B6B",
    high = "#4ECDC4",
    name = "Policy Support\n(color)"
  ) +
  labs(
    title = "Agricultural Sector: 3-Metric Relationship",
    subtitle = "X = Growth, Y = Jobs, Size/Color = Policy Support",
    x = "Growth Score",
    y = "Jobs Score"
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
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_vline(xintercept = mean(data$Growth), 
             linetype = "dashed", color = "gray60", alpha = 0.7) +
  geom_hline(yintercept = mean(data$Jobs), 
             linetype = "dashed", color = "gray60", alpha = 0.7) +
  annotate("text", x = 0.2, y = 1.05, 
           label = "High Jobs, Low Growth", size = 3.5, color = "gray40") +
  annotate("text", x = 1.0, y = 0.15, 
           label = "High Growth, Low Jobs", size = 3.5, color = "gray40")