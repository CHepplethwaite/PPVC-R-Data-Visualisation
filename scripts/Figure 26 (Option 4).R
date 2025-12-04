# ============================================
# FACETED BAR CHART: Detailed metric-by-metric view
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)

# Calculate composite score and prepare data
data_with_composite <- data %>%
  mutate(
    Composite = (Poverty + Diets + Growth + Jobs + Policy_support) / 5
  ) %>%
  arrange(Composite)

# Convert to long format for faceting
long_data <- data_with_composite %>%
  pivot_longer(
    cols = c(Poverty, Diets, Growth, Jobs, Policy_support),
    names_to = "Metric",
    values_to = "Score"
  ) %>%
  mutate(
    Metric = factor(Metric,
                    levels = c("Poverty", "Diets", "Growth", "Jobs", "Policy_support"),
                    labels = c("Poverty Reduction", "Dietary Impact", "Growth Potential", 
                               "Job Creation", "Policy Support")),
    Value_Chain = factor(Value_Chain, levels = data_with_composite$Value_Chain)
  )

# Create a per-value-chain label row (only once per chain, not every metric)
composite_labels <- long_data %>%
  distinct(Value_Chain, Composite)

ggplot(long_data, aes(x = Value_Chain, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", alpha = 0.85, width = 0.7) +
  facet_wrap(~ Metric, ncol = 3, scales = "free_x") +
  labs(
    title = "Value Chain Performance Across 5 Metrics",
    subtitle = "Products sorted by composite score (average of all metrics)",
    x = "",
    y = "Score",
    fill = "Metric"
  ) +
  scale_fill_manual(
    values = c("Poverty Reduction" = "#E41A1C",
               "Dietary Impact" = "#377EB8",
               "Growth Potential" = "#4DAF4A",
               "Job Creation" = "#984EA3",
               "Policy Support" = "#FF7F00")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60", alpha = 0.5) +
  
  # Apply composite labels once per Value_Chain (bottom)
  geom_text(
    data = composite_labels,
    aes(x = Value_Chain, y = -0.05,
        label = sprintf("Avg: %.2f", Composite)),
    inherit.aes = FALSE,
    angle = 90,
    size = 3,
    hjust = 0,
    color = "gray40"
  ) +
  
  coord_cartesian(ylim = c(-0.2, 1.1))  # ensures labels are visible
