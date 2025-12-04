library(GGally)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Example data
# data <- data.frame(
#   Value_Chain = c("Rice","Fisheries","Tomato","Maize","Cocoa"),
#   Poverty = runif(5),
#   Diets = runif(5),
#   Growth = runif(5),
#   Jobs = runif(5),
#   Policy_support = runif(5)
# )

# Convert to long format for plotting with labels
long_data <- data %>%
  pivot_longer(
    cols = Poverty:Policy_support,
    names_to = "Metric",
    values_to = "Score"
  ) %>%
  mutate(Metric_num = as.numeric(factor(Metric, levels = c("Poverty","Diets","Growth","Jobs","Policy_support"))))

# Base parallel coordinates using ggplot (not ggparcoord)
ggplot(long_data, aes(x = Metric_num, y = Score, group = Value_Chain, color = Value_Chain)) +
  geom_line(alpha = 0.7) +
  geom_point() +
  # Add labels at last metric
  geom_text(data = long_data %>% filter(Metric == "Policy Support"),
            aes(x = Metric_num + 0.2, y = Score, label = Value_Chain),
            hjust = 0, size = 3) +
  scale_x_continuous(breaks = 1:5, labels = c("Poverty","Diets","Growth","Jobs","Policy Support")) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Value Chain Performance",
       x = "Metrics", y = "Normalized Score (0-1)", color = "Value Chain") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    legend.title = element_blank()
  ) +
  coord_cartesian(clip = "off")   # allows labels outside the plot
