############################################################
# Bayesian Synthetic Control Method for Smoking Data (California)
# Using Horseshoe Prior
############################################################
# rm(list=ls())

# ==== Build Stan Data ====
standat <- list(
  N_train = length(y_train),
  N_test = length(y_test),
  p = ncol(X_train),
  y_train = y_train,
  X_train = X_train,
  X_test = X_test,
  ds = ds         ## ds needs to be computed separately
)

# ==== Compile and Fit Horseshoe Model ====
fit_horseshoe <- stan_model(file = ".\\distance_horseshoe\\distance_Horseshoe.stan")

p = ncol(X_train)
fit_horseshoe <- sampling(object = fit_horseshoe, data=standat, 
                          seed=2019, chains = 4, iter = 25000, warmup = 10000)

# ==== Extract Posterior Predictions ====
posterior <- rstan::extract(fit_horseshoe)

# Extract predictions
y_fit_post <- posterior$y_fit  # Pre-treatment fit
y_test_post <- posterior$y_test  # Post-treatment prediction

# Compute posterior mean and 95% CI
y_fit_mean <- apply(y_fit_post, 2, mean)
y_fit_ci <- apply(y_fit_post, 2, quantile, probs = c(0.025, 0.975))

y_test_mean <- apply(y_test_post, 2, mean)
y_test_ci <- apply(y_test_post, 2, quantile, probs = c(0.025, 0.975))

# ==== Combine data across all periods ====
years <- c(pre_years, post_years)
mean_pred <- c(y_fit_mean, y_test_mean)
lower_ci <- c(y_fit_ci[1,], y_test_ci[1,])
upper_ci <- c(y_fit_ci[2,], y_test_ci[2,])
observed <- c(y_train, y_test)

# ==== Construct plot dataframe ====
plot_data <- data.frame(
  year = years,
  observed = observed,
  proposed = mean_pred,
  lower = lower_ci,
  upper = upper_ci
)

# ==== Plot ====
ggplot(plot_data, aes(x = year)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
  geom_line(aes(y = proposed, color = "Proposed"), size = 1) +
  geom_line(aes(y = observed, color = "Observed"), linetype = "dotted", size = 1) +
  geom_vline(xintercept = treatment_year, linetype = "solid", size = 0.6) +
  annotate("text", x = treatment_year + 0.5, y = max(observed, na.rm = TRUE), 
           label = "Proposition 99", hjust = 0, size = 3.5) +
  scale_color_manual(
    name = "", 
    values = c("Proposed" = "red", "Observed" = "black")
  ) +
  labs(
    title = "(a) Observed Outcomes and Synthetic Control Outcomes",
    x = "Year",
    y = "Per-capita Cigarette Sales (in packs)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Save file
saveRDS(plot_data, file = "bayes_result_dshs.rds")

# Extract posterior samples
y_test_post <- posterior$y_test  # (iterations, N_test)

# True observed values
y_test_real <- y_test  # (N_test)

# Expand to same dimensional matrix
y_test_real_mat <- matrix(rep(y_test_real, each = nrow(y_test_post)), 
                          nrow = nrow(y_test_post))

# Compute prediction differences
diff_post <- y_test_post - y_test_real_mat  # (iterations, N_test)

# ==== 2. Convert to long format with year info ====

# Vector of years (test years)
test_years <- post_years  # Defined earlier as post_years

# Convert to data.frame
df_diff <- as.data.frame(diff_post)
colnames(df_diff) <- test_years  # Column names are the years

# Convert to long format
df_diff_long <- pivot_longer(df_diff, 
                             cols = everything(), 
                             names_to = "year", 
                             values_to = "diff")

# Ensure year is numeric
df_diff_long$year <- as.numeric(df_diff_long$year)

# Add a column to indicate the prior used
df_diff_long$prior <- "Horseshoe"

# ==== 3. Save posterior difference data ====

saveRDS(df_diff_long, file = "posterior_difference_dshorseshoe.rds")

# Plot beta traceplot

# ==== Extract beta posterior draws ====
beta_samples <- rstan::extract(fit_horseshoe, pars = "beta", permuted = FALSE)

# ==== Convert to mcmc.list object ====
library(coda)
chains_list <- lapply(1:dim(beta_samples)[2], function(chain_index) {
  as.mcmc(beta_samples[, chain_index, ])
})
mcmc_obj <- as.mcmc.list(chains_list)

# === Save Traceplot as PNG image ===
png("traceplot_beta1to4_with_rhat_dshorseshoe.png", width = 1200, height = 800, res = 150)

# ==== Extract Rhat values ====
summary_stats <- summary(fit_horseshoe)$summary
beta_rhat <- summary_stats[grep("^beta\\[", rownames(summary_stats)), "Rhat"]
names(beta_rhat) <- rownames(summary_stats)[grep("^beta\\[", rownames(summary_stats))]

# === Plotting code (reused) ===
par(mfrow = c(2, 2), oma = c(0, 0, 2, 8))
for (j in 1:4) {
  rhat_val <- round(beta_rhat[j], 5)
  traceplot(mcmc_obj[, j], 
            main = paste0("beta[", j, "]  (Rhat=", rhat_val, ")"), 
            col = c("orange", "blue", "purple", "black"), 
            ylab = "Value", xlab = "Iterations")
}
par(xpd = NA)
legend("topright", inset = c(-0.25, 0), 
       legend = c("1", "2", "3", "4"),
       title = "chain",
       col = c("orange", "blue", "purple", "black"), 
       lty = 1, lwd = 1, cex = 0.8)

# === Close PNG output ===
dev.off()
