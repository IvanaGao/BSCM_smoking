# Clear environment
rm(list=ls())
library(ggplot2)
library(dplyr)

# Load data
synth_data <- readRDS("synth_result.rds")
bayes_data_hs <- readRDS("bayes_result_hs.rds")
bayes_data_dshs <- readRDS("bayes_result_dshs.rds")
bayes_data_norm <- readRDS("bayes_result_ridge.rds")

# Prepare data
synth_data_plot <- synth_data %>%
  select(time_unit, real_y, synth_y) %>%
  rename(year = time_unit, observed = real_y, synth = synth_y)

bayes_data_plot_hs <- bayes_data_hs %>% select(year, observed, proposed, lower, upper)
bayes_data_plot_dshs <- bayes_data_dshs %>% select(year, observed, proposed, lower, upper)
bayes_data_plot_norm <- bayes_data_norm %>% select(year, observed, proposed, lower, upper)

# ---- Draw the total overview plot ----
p_total <- ggplot() +
  geom_line(data = synth_data_plot, aes(x = year, y = observed, color = "Observed"), linetype = "dotted", size = 1) +
  geom_line(data = synth_data_plot, aes(x = year, y = synth, color = "SCM"), size = 1) +
  geom_line(data = bayes_data_plot_hs, aes(x = year, y = proposed, color = "BSCM-horseshoe"), size = 1) +
  #geom_ribbon(data = bayes_data_plot_dshs, aes(x = year, ymin = lower, ymax = upper), fill = "gray80", alpha = 0.5) +
  #geom_line(data = bayes_data_plot_dshs, aes(x = year, y = proposed, color = "BSCM-distance-horseshoe"), size = 1) +
  geom_line(data = bayes_data_plot_norm, aes(x = year, y = proposed, color = "BSCM-ridge"), size = 1) +
  geom_vline(xintercept = 1988, linetype = "dashed", color = "black") +
  scale_color_manual(
    name = "Legend",
    values = c(
      "Observed" = "black",
      "SCM" = "blue",
      "BSCM-horseshoe" = "green",
      # "BSCM-distance-horseshoe" = "red",
      "BSCM-ridge" = "brown"
    )
  ) +
  labs(
    title = "Comparison of Synthetic Control Methods",
    x = "Year",
    y = "Per-capita Cigarette Sales (in packs)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Save total overview
ggsave("total_comparison_plot.png", p_total, width = 10, height = 6)

# ---- Draw individual plots ----

# Synth SCM only
p_synth <- ggplot() +
  geom_line(data = synth_data_plot, aes(x = year, y = observed, color = "Observed"), size = 1) +
  geom_line(data = synth_data_plot, aes(x = year, y = synth, color = "SCM"), size = 1) +
  geom_vline(xintercept = 1988, linetype = "dashed", color = "black") +
  scale_color_manual(
    name = "",
    values = c("Observed" = "black", "SCM" = "blue")
  ) +
  labs(
    title = "Observed vs SCM (Synth)",
    x = "Year",
    y = "Per-capita Cigarette Sales (in packs)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("synth_only_plot.png", p_synth, width = 8, height = 5)

# BSCM-horseshoe
p_hs <- ggplot() +
  geom_ribbon(data = bayes_data_plot_hs, aes(x = year, ymin = lower, ymax = upper, fill = "95% Credible Interval"), alpha = 0.4) +
  geom_line(data = bayes_data_plot_hs, aes(x = year, y = observed, color = "Observed"), size = 1) +
  geom_line(data = bayes_data_plot_hs, aes(x = year, y = proposed, color = "BSCM-horseshoe"), size = 1) +
  geom_vline(xintercept = 1988, linetype = "dashed", color = "black") +
  scale_color_manual(
    name = "",
    values = c("Observed" = "black", "BSCM-horseshoe" = "green")
  ) +
  scale_fill_manual(
    name = "",
    values = c("95% Credible Interval" = "gray80")
  ) +
  labs(
    title = "Observed vs BSCM-horseshoe",
    x = "Year",
    y = "Per-capita Cigarette Sales (in packs)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("bscm_horseshoe_plot.png", p_hs, width = 8, height = 5)

# BSCM-distance-horseshoe
p_dshs <- ggplot() +
  geom_ribbon(data = bayes_data_plot_dshs, aes(x = year, ymin = lower, ymax = upper, fill = "95% Credible Interval"), alpha = 0.4) +
  geom_line(data = bayes_data_plot_dshs, aes(x = year, y = observed, color = "Observed"), size = 1) +
  geom_line(data = bayes_data_plot_dshs, aes(x = year, y = proposed, color = "BSCM-distance-horseshoe"), size = 1) +
  geom_vline(xintercept = 1988, linetype = "dashed", color = "black") +
  scale_color_manual(
    name = "",
    values = c("Observed" = "black", "BSCM-distance-horseshoe" = "red")
  ) +
  scale_fill_manual(
    name = "",
    values = c("95% Credible Interval" = "gray80")
  ) +
  labs(
    title = "Observed vs BSCM-distance-horseshoe",
    x = "Year",
    y = "Per-capita Cigarette Sales (in packs)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("bscm_distance_horseshoe_plot.png", p_dshs, width = 8, height = 5)

# BSCM-ridge
p_norm <- ggplot() +
  geom_ribbon(data = bayes_data_plot_norm, aes(x = year, ymin = lower, ymax = upper, fill = "95% Credible Interval"), alpha = 0.4) +
  geom_line(data = bayes_data_plot_norm, aes(x = year, y = observed, color = "Observed"), size = 1) +
  geom_line(data = bayes_data_plot_norm, aes(x = year, y = proposed, color = "BSCM-ridge"), size = 1) +
  geom_vline(xintercept = 1988, linetype = "dashed", color = "black") +
  scale_color_manual(
    name = "",
    values = c("Observed" = "black", "BSCM-ridge" = "brown")
  ) +
  scale_fill_manual(
    name = "",
    values = c("95% Credible Interval" = "gray80")
  ) +
  labs(
    title = "Observed vs BSCM-ridge",
    x = "Year",
    y = "Per-capita Cigarette Sales (in packs)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("bscm_ridge_plot.png", p_norm, width = 8, height = 5)

