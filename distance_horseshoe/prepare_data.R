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
# data <- read.csv(".\\distance_horseshoe\\smoking_california_info.csv")
data <- read.csv(".\\distance_horseshoe\\smoking.csv")
data <- data %>% arrange(state)
# Ensure year is numeric
data$year <- as.numeric(data$year)

# ==== Define Treatment Year ====
treatment_year <- 1988

# ==== Split into Treated and Control Units using 'california' Column ====
treated <- data %>% filter(california == "TRUE")
control <- data %>% filter(california == "FALSE")

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

