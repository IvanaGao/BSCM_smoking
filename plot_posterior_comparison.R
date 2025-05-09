library(ggplot2)
library(dplyr)

# ==== 1. Load data ====
horseshoe_diff <- readRDS("posterior_difference_horseshoe.rds")
ds_horseshoe_diff <- readRDS("posterior_difference_dshorseshoe.rds")
normal_horseshoe_diff <- readRDS("posterior_difference_ridge.rds")

# ==== 2. Add prior labels for each dataset ====
horseshoe_diff$prior <- "horseshoe"
ds_horseshoe_diff$prior <- "distance-horseshoe"
normal_horseshoe_diff$prior <- "ridge"

# ==== 3. Combine datasets ====
# all_diff <- bind_rows(horseshoe_diff, ds_horseshoe_diff, normal_horseshoe_diff)
all_diff <- bind_rows(horseshoe_diff, normal_horseshoe_diff)

# ==== 4. Select the year to plot ====
target_year <- 1990

plot_data <- all_diff %>%
  filter(year == target_year)

# ==== 5. Plot posterior difference distributions ====
p <- ggplot(plot_data, aes(x = diff, fill = prior)) +
  geom_density(alpha = 0.5) +   # semi-transparent
  labs(
    title = paste("Posterior Distribution of effect estimation in", target_year),
    x = "Prediction - Real",
    y = "Density"
  ) +
  scale_fill_manual(
    name = "Prior",
    values = c(
      "horseshoe" = "green",
      "distance-horseshoe" = "red",
      "ridge" = "brown"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# ==== 6. Save plot ====
ggsave(
  filename = paste0("posterior_difference_", target_year, ".png"),
  plot = p,
  width = 8,
  height = 5
)

summary_stats <- plot_data %>%
  group_by(prior) %>%
  summarise(
    mean_diff = mean(diff),
    lower_95 = quantile(diff, 0.025),
    upper_95 = quantile(diff, 0.975),
    .groups = "drop"
  )
print(target_year)
print(summary_stats)
