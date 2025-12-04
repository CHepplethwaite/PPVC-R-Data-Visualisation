
# Load required libraries
library(tidyverse)
library(ggplot2) # Already included in tidyverse, but explicitly good practice

# ====================================================================
# 🔥 STEP 0: EMBEDDED DATA (The Fix for the "group_by" Error) 🔥
# This creates the data frame 'df' required by the subsequent steps.
# ====================================================================
df <- tibble(
  Commodity = c("Goat", "Fisheries", "Oil palm", "Cassava", "Cashew", "Plantain", "Pineapple", "Tomato", "Rice", "Yam", "Soyabean", "Maize", "Sorghum", "Cocoa", "Rubber", "Poultry", "Rubber", "Goat", "Oil palm", "Soyabean", "Sorghum", "Rice", "Pineapple", "Plantain", "Cassava", "Maize", "Yam", "Fisheries", "Cocoa", "Cashew", "Tomato", "Poultry", "Maize", "Sorghum", "Cashew", "Fisheries", "Rubber", "Rice", "Cocoa", "Pineapple", "Cassava", "Yam", "Plantain", "Poultry", "Soyabean", "Oil palm", "Tomato", "Goat"),
  Value = c(0.10, 0.84, 0.95, 1.04, 1.07, 1.07, 1.08, 1.08, 1.08, 1.08, 1.08, 1.08, 1.08, 1.09, 1.09, 1.10, 0.10, 0.18, 0.27, 0.28, 0.40, 0.45, 0.51, 0.52, 0.53, 0.67, 0.78, 0.82, 0.84, 0.93, 1.02, 1.10, 0.10, 0.15, 0.17, 0.22, 0.26, 0.28, 0.30, 0.33, 0.33, 0.35, 0.36, 0.36, 0.37, 0.43, 0.46, 0.46),
  Metric = c(rep("Potential for intensification", 16), rep("Domestic consumption growth", 16), rep("Regional export potential", 16))
)


# 1. Calculate the Total Score for each commodity
total_score_df <- df %>%
  group_by(Commodity) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Total_Value) 

# 2. Set the factor level order based on the Total Score (for correct ordering on the Y-axis)
df$Commodity <- factor(df$Commodity, levels = total_score_df$Commodity)

# 3. Create the Stacked Bar Chart
stacked_bar_chart <- ggplot(df, aes(x = Value, y = Commodity, fill = Metric)) +
  
  # Plot the stacked bars
  geom_bar(stat = "identity", position = "stack") +
  
  # Add total score labels at the end of each stacked bar
  geom_text(data = total_score_df,
            aes(x = Total_Value, y = Commodity, label = sprintf("%.2f", Total_Value)),
            inherit.aes = FALSE,
            hjust = -0.1, # Nudge the text outside the bar
            size = 3.5,
            fontface = "bold") +
  
  # Apply theme and labels
  labs(
    title = "Total Commodity Potential: Score Breakdown by Metric",
    x = "Score Value (Sum of All Metrics)",
    y = NULL, # Remove y-axis label
    fill = "Metric"
  ) +
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# 4. Display the plot
print(stacked_bar_chart)
```