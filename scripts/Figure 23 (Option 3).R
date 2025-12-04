# ============================================
# STACKED BAR FACETS: Clear metric comparison
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)

# Prepare data for faceted bar plot
facet_data <- heatmap_data %>%
  group_by(Product) %>%
  mutate(
    Overall_Score = mean(Value, na.rm = TRUE),
    Metric_Group = case_when(
      Metric %in% c("Potential for\nIntensification", "Scalability", "Investment\nSupport") ~ "Growth Potential",
      Metric %in% c("Domestic\nConsumption Growth", "Regional Export\nPotential", "RTA") ~ "Market Factors",
      Metric %in% c("Input Cost/\nUse Ratio", "Natural Resource\nAvailability", "Production\nVolatility") ~ "Resource & Risk"
    )
  )

# Create faceted bar plot
ggplot(facet_data, aes(x = reorder(Product, -Overall_Score), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Metric_Group ~ ., scales = "free_y") +
  scale_fill_viridis_d(option = "D") +
  labs(
    title = "Agricultural Metrics by Product Grouped by Category",
    subtitle = "Products sorted by average score across all metrics",
    x = "Product",
    y = "Score",
    fill = "Metric"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.text = element_text(size = 9),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  guides(fill = guide_legend(ncol = 1))