library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(grid) # for unit()

# Sample data frame (replace with your actual data)
data <- read.table(header=TRUE, text="
Value_Chain Business Development Final_Ranking
Rice 3 2 1
Fisheries 4 1 2
Tomato 7 3 3
Maize 2 6 4
Cocoa 5 5 5
Poultry 1 8 6
Pineapple 11 4 7
Oil_palm 12 7 8
Cassava 6 12 9
Cashew 8 11 10
Rubber 10 9 11
Yam 9 10 12
Sorghum 15 13 13
Plantain 14 15 14
Soyabean 13 16 15
Goat 16 14 16
")

# Ensure Value_Chain is a factor sorted by Final_Ranking
data <- data %>%
  mutate(Value_Chain = factor(Value_Chain, levels = Value_Chain[order(Final_Ranking)]))

# Plot 1: Heatmap of Business vs Development rank
p1 <- data %>%
  pivot_longer(cols = c(Business, Development), names_to = "Metric", values_to = "Rank") %>%
  ggplot(aes(x = Metric, y = Value_Chain, fill = Rank)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Rank), color = "black", fontface = "bold", size = 3.5) +
  scale_fill_gradient(low = "#b2182b", high = "#2166ac", name = "Rank") +
  labs(title = "Business vs Development Rank Heatmap", x = "", y = "") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm")
  )

# Plot 2: Final Ranking bar plot
p2 <- data %>%
  ggplot(aes(x = Final_Ranking, y = Value_Chain)) +
  geom_bar(stat = "identity", fill = "#4DAF4A", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = Final_Ranking), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(title = "Final Ranking", x = "Rank", y = "") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_x_continuous(limits = c(0, 17))

# Combine plots
combined_plot <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(2, 1)) +
  plot_annotation(
    title = "Agricultural Products: Business vs Development Rank",
    subtitle = "Left: Individual ranks | Right: Final combined ranking",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40")
    )
  )

print(combined_plot)
