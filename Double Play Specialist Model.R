# Packages
library(dplyr)
library(readxl)
library(rstan)
library(baseballr)
library(purrr)

# Data Scraping
fg_2021_data <- fg_pitch_leaders(startseason = "2021", endseason = "2021", qual = 0)
fg_2022_data <- fg_pitch_leaders(startseason = "2022", endseason = "2022", qual = 0)
fg_2023_data <- fg_pitch_leaders(startseason = "2023", endseason = "2023", qual = 0)

# Create the new variable GR for each dataset
fg_2021_data <- fg_2021_data %>%
  mutate(GR = G - GS, Season = 2021)

fg_2022_data <- fg_2022_data %>%
  mutate(GR = G - GS, Season = 2022)

fg_2023_data <- fg_2023_data %>%
  mutate(GR = G - GS, Season = 2023)

# List of all dataframes
fangraphs_data <- list(fg_2021_data, fg_2022_data, fg_2023_data)

# Get common column names across all dataframes
common_cols <- reduce(map(fangraphs_data, colnames), intersect)

# Function to select common columns and add a season column
select_common_cols <- function(df, season) {
  df %>%
    select(all_of(common_cols)) %>%
    mutate(Season = season)
}

# Apply the function to each dataframe and combine all data
fangraphs_data <- bind_rows(
  select_common_cols(fg_2021_data, 2021),
  select_common_cols(fg_2022_data, 2022),
  select_common_cols(fg_2023_data, 2023)
)

# Filter the data for the 2023 season and GR >= 35
fangraphs_data_2023 <- fangraphs_data %>%
  filter(Season == 2023 & GR >= 35)

# Add DP_Specialist column initialized to NA
fangraphs_data_2023 <- fangraphs_data_2023 %>%
  mutate(DP_Specialist = NA)

# Convert Soft_pct to numeric
fangraphs_data_2023 <- fangraphs_data_2023 %>%
  mutate(Soft_pct = as.numeric(Soft_pct))

# Define the Bayesian model using Stan language
# Define the Bayesian model using Stan language
stan_code <- "
data {
  int<lower=0> N;        // Number of observations
  vector[N] GB_pct;      // Ground ball percentage
  vector[N] Soft_pct;    // Soft hit percentage
  vector[N] Swing_pct;   // Swing percentage
}
parameters {
  real alpha;             // Intercept
  real beta_GB;           // Coefficient for GB_pct
  real beta_Soft;         // Coefficient for Soft_pct
  real beta_Swing;        // Coefficient for Swing_pct
}
model {
  // Priors
  alpha ~ normal(0, 10);
  beta_GB ~ normal(0, 1);
  beta_Soft ~ normal(0, 1);
  beta_Swing ~ normal(0, 1);
  
  // Likelihood
  for (i in 1:N) {
    // Quantile regression likelihood
    GB_pct[i] ~ normal(alpha + beta_GB * GB_pct[i] + beta_Soft * Soft_pct[i] + beta_Swing * Swing_pct[i], 1);
  }
}
"

# Compile the Stan model
stan_model <- stan_model(model_code = stan_code)

# Prepare the data
stan_data <- list(
  N = nrow(fangraphs_data_2023),           # Assuming 'data' is your dataframe
  GB_pct = fangraphs_data_2023$GB_pct,
  Soft_pct = fangraphs_data_2023$Soft_pct,
  Swing_pct = fangraphs_data_2023$Swing_pct
)

# Perform Bayesian inference (sampling)
bayesian_fit <- sampling(stan_model, data = stan_data)

# Print summary of the Bayesian fit
print(bayesian_fit)


# Initialize predictions vector
predictions <- numeric(nrow(fangraphs_data_2023))

# Extract posterior samples
posterior_samples <- extract(bayesian_fit)

# Calculate mean posterior samples
alpha_mean <- mean(posterior_samples$alpha)
beta_GB_mean <- mean(posterior_samples$beta_GB)
beta_Soft_mean <- mean(posterior_samples$beta_Soft)
beta_Swing_mean <- mean(posterior_samples$beta_Swing)

# Initialize predictions vector
predictions <- numeric(nrow(fangraphs_data_2023))

# Iterate over each row of fangraphs_data_2023
for (i in 1:nrow(fangraphs_data_2023)) {
  # Calculate prediction for the i-th player using the mean of posterior samples
  predictions[i] <- rnorm(1, 
                          mean = alpha_mean + 
                            beta_GB_mean * fangraphs_data_2023$GB_pct[i] + 
                            beta_Soft_mean * fangraphs_data_2023$Soft_pct[i] +
                            beta_Swing_mean * fangraphs_data_2023$Swing_pct[i],
                          sd = 1)  # Assuming standard deviation of 1
}

# Add predictions to the dataframe
fangraphs_data_2023$DP_Specialist_prediction <- predictions

# View the dataframe with predictions
head(fangraphs_data_2023)

# Add predictions to the dataframe
fangraphs_data_2023$DP_Specialist_prediction <- predictions

# View the dataframe with predictions
head(fangraphs_data_2023)

# Perform Bayesian inference (sampling)
bayesian_fit <- sampling(stan_model, data = stan_data)

# Print summary of the Bayesian fit
print(bayesian_fit)

# Select the desired columns
predictions_subset_DP <- fangraphs_data_2023 %>%
  select(PlayerName, DP_Specialist_prediction)

# View the resulting dataframe
head(predictions_subset_DP)
