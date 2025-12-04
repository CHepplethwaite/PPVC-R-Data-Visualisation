# ============================================
# RADIAL BAR CHART: Shows composite score with ranking
# ============================================

library(ggplot2)
library(dplyr)
library(scales)

# Extract composite scores and ranking
composite_data <- data.frame(
  Product = c("Rice", "Fisheries", "Tomato", "Maize", "Cocoa", "Poultry", 
              "Pineapple", "Oil palm", "Cassava", "Cashew", "Rubber", 
              "Yam", "Sorghum", "Plantain", "Soyabean", "Goat"),
  Composite_Score = c(1.00, 0.94, 0.90, 0.85, 0.79, 0.79, 0.76, 0.57, 0.54, 
                      0.53, 0.53, 0.52, 0.37, 0.30, 0.28, 0.17),
  Rank = 1:16
)

# Create color gradient based on rank groups
composite_data <- composite_data %>%
  mutate(
    Color_Group = cut(
      Rank,
      breaks = c(0, 4, 8, 16),
      labels = c("Top 4", "Middle 4", "Bottom 8"),
      include.lowest = TRUE
    )
  )

# Radial bar chart
ggplot(composite_data, aes(
  x = reorder(Product, -Rank),
  y = Composite_Score,
  fill = Color_Group
)) +
  geom_bar(stat = "identity", width = 0.85, alpha = 0.95) +
  coord_polar(start = -0.1) +
  scale_fill_manual(
    values = c(
      "Top 4" = "#2E8B57",     # green
      "Middle 4" = "#FFD700",  # gold
      "Bottom 8" = "#DC143C"   # crimson
    )
  ) +
  geom_text(
    aes(label = Rank),
    position = position_stack(vjust = 0.5),
    color = "white", fontface = "bold", size = 4
  ) +
  labs(
    title = "Value Chain Composite Score",
    subtitle = "Outer ring = Rank 1 (Best), inner ring = Rank 16 (Worst)",
    fill = "Rank Group"
  ) +
  ylim(-0.35, 1.1) +  # increased bottom margin so labels don't get cut
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 9, face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "right",
    plot.margin = margin(t = 20, r = 20, b = 30, l = 20)  # add extra space
  )
