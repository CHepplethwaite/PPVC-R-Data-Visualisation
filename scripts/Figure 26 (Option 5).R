# ============================================
# BUBBLE MATRIX: Compare all pairwise relationships
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)

# Create custom function for bubble matrix
bubble_matrix_fn <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(aes(size = abs(data[, as.character(mapping$x)[2]] - 
                                data[, as.character(mapping$y)[2]])),
               color = "#2E86AB", alpha = 0.6, ...) +
    geom_smooth(method = "lm", color = "#A23B72", se = FALSE, ...) +
    scale_size_continuous(range = c(1, 6))
  p
}

# Create custom function for diagonal
diag_fn <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) + 
    geom_density(fill = "#F18F01", alpha = 0.7, ...) +
    geom_vline(xintercept = mean(eval_data_col(data, mapping$x)), 
               color = "gray40", linetype = "dashed")
  p
}

# Create custom function for correlation values
cor_fn <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  corr <- round(cor(x, y), 2)
  
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = paste0("r = ", corr), 
             size = 5, fontface = "bold", 
             color = ifelse(abs(corr) > 0.5, "#B22222", 
                            ifelse(abs(corr) > 0.3, "#FF8C00", "black"))) +
    theme_void()
}

# Create bubble matrix plot
ggpairs(data,
        columns = 2:6,
        upper = list(continuous = wrap(bubble_matrix_fn)),
        diag = list(continuous = wrap(diag_fn)),
        lower = list(continuous = wrap(cor_fn)),
        title = "Pairwise Metric Relationships\n(Bubble size = absolute difference)",
        axisLabels = "show") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(face = "bold")
  )