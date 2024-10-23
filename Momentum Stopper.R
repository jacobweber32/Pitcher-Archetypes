library(dplyr)
library(readxl)
library(rstan)
library(baseballr)

fangraphs_data <- fg_pitcher_leaders(startseason = "2024", endseason = "2024")
fangraphs_data <- fangraphs_data %>%
  mutate(`FIP-` = as.numeric(`FIP-`)) %>%
  rename(FIP_minus = `FIP-`)

fangraphs_data <- fangraphs_data %>%
  mutate(GR = G - GS) %>%
  mutate(FIP_plus = (-(FIP_minus - 100)) + 100)

fangraphs_data_2024 <- fangraphs_data %>%
  filter(Season == "2024") %>%
  filter(GR >= 35)

fangraphs_data_2024$Momentum_Stopper <- NA

fangraphs_data_2024$Soft_pct <- as.numeric(fangraphs_data_2024$Soft_pct)
fangraphs_data_2024$K_pct <- as.numeric(fangraphs_data_2024$K_pct)
fangraphs_data_2024$LOB_pct <- as.numeric(fangraphs_data_2024$LOB_pct)
fangraphs_data_2024$FIP_plus <- as.numeric(fangraphs_data_2024$FIP_plus)

# Define the Bayesian model using Stan language
stan_code <- "
data {
  int<lower=0> N;            // Number of observations
  vector[N] K_pct;           // Strikeout Percentage
  vector[N] Soft_pct;        // Soft hit percentage
  vector[N] LOB_pct;         // Left on base percentage
  vector[N] FIP_plus;        // Fielder Independent Pitching Above Average
 
}
parameters {
  real alpha;                // Intercept
  real beta_K;               // Coefficient for K_pct
  real beta_Soft;            // Coefficient for Soft_pct
  real beta_LOB;             // Coefficient for LOB_pct
  real beta_FIP_plus;        // Coefficient for FIP_plus
  
}
model {
  // Priors
  alpha ~ normal(0, 10);
  beta_K ~ normal(0, 1);
  beta_Soft ~ normal(0, 1);
  beta_LOB ~ normal(0, 1);
  beta_FIP_plus ~ normal(50, 200);   // Corrected identifier
  
  // Likelihood
  for (i in 1:N) {
    // Likelihood function
    K_pct[i] ~ normal(alpha + beta_K * K_pct[i] + beta_Soft * Soft_pct[i] + beta_LOB * LOB_pct[i] + beta_FIP_plus * FIP_plus[i], 1);
  }
}
"

# Compile the Stan model
stan_model <- stan_model(model_code = stan_code)

# Prepare the data
stan_data <- list(
  N = nrow(fangraphs_data_2024),
  K_pct = fangraphs_data_2024$K_pct,
  Soft_pct = fangraphs_data_2024$Soft_pct,
  LOB_pct = fangraphs_data_2024$LOB_pct,
  FIP_plus = fangraphs_data_2024$FIP_plus
)

# Perform Bayesian inference (sampling)
bayesian_fit <- sampling(stan_model, data = stan_data)

# Print summary of the Bayesian fit
print(bayesian_fit)


# Initialize predictions vector
predictions <- numeric(nrow(fangraphs_data_2024))

# Extract posterior samples
posterior_samples <- extract(bayesian_fit)

# Calculate mean posterior samples
alpha_mean <- mean(posterior_samples$alpha)
beta_K_mean <- mean(posterior_samples$beta_K)
beta_Soft_mean <- mean(posterior_samples$beta_Soft)
beta_LOB_mean <- mean(posterior_samples$beta_LOB)
beta_FIP_plus_mean <- mean(posterior_samples$beta_FIP_plus)

# Iterate over each row of fangraphs_data_2024
for (i in 1:nrow(fangraphs_data_2024)) {
  # Calculate prediction for the i-th player using the mean of posterior samples
  predictions[i] <- rnorm(1, 
                          mean = alpha_mean + 
                            beta_K_mean * fangraphs_data_2024$K_pct[i] + 
                            beta_Soft_mean * fangraphs_data_2024$Soft_pct[i] +
                            beta_LOB_mean * fangraphs_data_2024$LOB_pct[i] +
                            beta_FIP_plus_mean * fangraphs_data_2024$FIP_plus[i],
                          sd = 1)  # Assuming standard deviation of 1
}

# Add predictions to the dataframe
fangraphs_data_2024$Momentum_Stopper_prediction <- predictions

# View the dataframe with predictions
head(fangraphs_data_2024)

# Perform Bayesian inference (sampling) again
bayesian_fit <- sampling(stan_model, data = stan_data)

# Print summary of the Bayesian fit again
print(bayesian_fit)

# Select the desired columns
predictions_subset_momentum <- fangraphs_data_2024 %>%
  select(PlayerName, Momentum_Stopper_prediction)

# View the resulting dataframe
head(predictions_subset_momentum)
