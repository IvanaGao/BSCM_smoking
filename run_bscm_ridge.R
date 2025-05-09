############################################################
# Bayesian Synthetic Control Method for Smoking Data (California)
# Using Horseshoe Prior
############################################################
rm(list=ls())
# ==== Load Libraries ====
library(rstan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(loo)

# ==== Stan Options ====
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# ==== Read and Preprocess Data ====
data <- read.csv("smoking_california_info.csv")

# Ensure year is numeric
data$year <- as.numeric(data$year)

# ==== Define Treatment Year ====
treatment_year <- 1988

# ==== Split into Treated and Control Units using 'california' Column ====
treated <- data %>% filter(california == "True")
control <- data %>% filter(california == "False")

# ==== Convert Control Data to Wide Format ====
control_wide <- control %>%
  select(year, state, cigsale) %>%
  pivot_wider(names_from = state, values_from = cigsale) %>%
  arrange(year)

# ==== Match Years ====
treated_years <- treated$year
control_wide <- control_wide %>% filter(year %in% treated_years)

# ==== Define Pre/Post Periods ====
pre_years <- treated %>% filter(year < treatment_year) %>% pull(year)
post_years <- treated %>% filter(year >= treatment_year) %>% pull(year)

# ==== Outcome for Treated Unit ====
y_train <- treated %>% filter(year < treatment_year) %>% pull(cigsale)
y_test  <- treated %>% filter(year >= treatment_year) %>% pull(cigsale)

# ==== Covariates from Control Units ====
X_train <- control_wide %>% filter(year < treatment_year) %>% select(-year) %>% as.matrix()
X_test  <- control_wide %>% filter(year >= treatment_year) %>% select(-year) %>% as.matrix()

# ==== Diagnostic Print ====
cat("X_train dim: ", dim(X_train), "\n")
cat("y_train length: ", length(y_train), "\n")

cat("X_test dim: ", dim(X_test), "\n")
cat("y_test length: ", length(y_test), "\n")

# ==== Build Stan Data ====
standat <- list(
  N_train = length(y_train),
  N_test = length(y_test),
  p = ncol(X_train),
  y_train = y_train,
  X_train = X_train,
  X_test = X_test
)

# ==== Compile and Fit Horseshoe Model ====
#fit_horseshoe <- stan_model(file = "Horseshoe_Publish.stan")

# ==== Compile and Fit Horseshoe Model ====
fit_horseshoe <- stan_model(file = "Normal_ridge.stan")

fit_horseshoe <- sampling(object = fit_horseshoe, data=standat, 
                          seed=2019, chains = 4, iter = 25000, warmup = 10000)


# ==== Extract Posterior Predictions ====
posterior <- rstan::extract(fit_horseshoe)

y_fit_post <- posterior$y_fit  # pre-treatment fit
y_test_post <- posterior$y_test  # post-treatment predict

y_fit_mean <- apply(y_fit_post, 2, mean)
y_fit_ci <- apply(y_fit_post, 2, quantile, probs = c(0.025, 0.975))

y_test_mean <- apply(y_test_post, 2, mean)
y_test_ci <- apply(y_test_post, 2, quantile, probs = c(0.025, 0.975))

years <- c(pre_years, post_years)
mean_pred <- c(y_fit_mean, y_test_mean)
lower_ci <- c(y_fit_ci[1,], y_test_ci[1,])
upper_ci <- c(y_fit_ci[2,], y_test_ci[2,])
observed <- c(y_train, y_test)

plot_data <- data.frame(
  year = years,
  observed = observed,
  proposed = mean_pred,
  lower = lower_ci,
  upper = upper_ci
)

# ==== plot ====
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

saveRDS(plot_data, file = "bayes_result_ridge.rds")


# ==== 1. Bayesian SCM posterior difference extraction ====

y_test_post <- posterior$y_test  # (iterations, N_test)

y_test_real <- y_test  # (N_test)

y_test_real_mat <- matrix(rep(y_test_real, each = nrow(y_test_post)), 
                          nrow = nrow(y_test_post))

diff_post <- y_test_post - y_test_real_mat  # (iterations, N_test)

test_years <- post_years

df_diff <- as.data.frame(diff_post)
colnames(df_diff) <- test_years

df_diff_long <- pivot_longer(df_diff, 
                             cols = everything(), 
                             names_to = "year", 
                             values_to = "diff")

df_diff_long$year <- as.numeric(df_diff_long$year)

df_diff_long$prior <- "Horseshoe"

saveRDS(df_diff_long, file = "posterior_difference_ridge.rds")


# plot beta traceplot

beta_samples <- rstan::extract(fit_horseshoe, pars = "beta", permuted = FALSE)

library(coda)
chains_list <- lapply(1:dim(beta_samples)[2], function(chain_index) {
  as.mcmc(beta_samples[, chain_index, ])
})
mcmc_obj <- as.mcmc.list(chains_list)

png("traceplot_beta1to4_with_rhat_ridge.png", width = 1200, height = 800, res = 150)

summary_stats <- summary(fit_horseshoe)$summary
beta_rhat <- summary_stats[grep("^beta\\[", rownames(summary_stats)), "Rhat"]
names(beta_rhat) <- rownames(summary_stats)[grep("^beta\\[", rownames(summary_stats))]

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

dev.off()