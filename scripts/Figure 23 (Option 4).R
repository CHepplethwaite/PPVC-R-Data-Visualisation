library(GGally)
library(ggplot2)

# Custom function for upper triangle
upper_fn <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) + 
    geom_point(color = "#2E86AB", alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", color = "#A23B72", se = FALSE, ...)
}

# Custom function for diagonal
diag_fn <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) + 
    geom_density(fill = "#F18F01", alpha = 0.7, ...)
}

# Custom function for lower triangle (correlation)
lower_fn <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  corr <- round(cor(x, y, use = "complete.obs"), 2)
  
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = corr, 
             size = 6, fontface = "bold", color = ifelse(abs(corr) > 0.5, "#B22222", "black")) +
    theme_void()
}

# Select only the numeric metrics you have
selected_metrics <- data[, c("Product", "Scalability", "Natural_resource", "Production_volatility")]

# Create scatter plot matrix
ggpairs(selected_metrics,
        columns = 2:4,
        mapping = aes(color = Product),
        upper = list(continuous = upper_fn),
        diag = list(continuous = diag_fn),
        lower = list(continuous = lower_fn),
        title = "Metric Relationships") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
