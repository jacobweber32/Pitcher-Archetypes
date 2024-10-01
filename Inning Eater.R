library(dplyr)
library(readxl)
library(rstan)

# Dataset and Filter
fangraphs_data <- read_excel(("C:/Users/weber/OneDrive/Desktop/Baseball/Weber Final Project/Fangraphs Data Weber.xlsx"))
fangraphs_data <- fangraphs_data %>%
  mutate(GR = G - GS) %>%
  mutate(FIP_plus = (-(FIP_minus - 100)) + 100) %>%
  mutate(Not_Walked_pct = 1 - BB_pct)

fangraphs_data_2023 <- fangraphs_data %>%
  filter(Season == "2023") %>%
  filter(GR >= 35)

fangraphs_data_2023$Inning_Eater <- NA

fangraphs_data_2023$Not_Walked_pct <- as.numeric(fangraphs_data_2023$Not_Walked_pct)
fangraphs_data_2023$Contact_pct <- as.numeric(fangraphs_data_2023$Contact_pct)
fangraphs_data_2023$Stuff_plus <- as.numeric(fangraphs_data_2023$Stuff_plus)


# Define the Bayesian model using Stan language
stan_code <- "
data {
  int<lower=0> N;            // Number of observations
  vector[N] Not_Walked_pct;  // Percentage of Batters not walked
  vector[N] Contact_pct;        // Percentage of Contact Made on Swings
  vector[N] Swing_pct;         // Left on base percentage

}
parameters {
  real alpha;                // Intercept
  real beta_BB;               // Coefficient for Not Walked_pct
  real beta_Contact;            // Coefficient for Contact_pct
  real beta_Swing;             // Coefficient for Swing_pct

}
model {
  // Priors
  alpha ~ normal(0, 10);
  beta_BB ~ normal(0, 1);
  beta_Contact ~ normal(0, 1);
  beta_Swing ~ normal(0, 1);

  // Likelihood
  for (i in 1:N) {
    // Likelihood function
    Not_Walked_pct[i] ~ normal(alpha + beta_BB * Not_Walked_pct[i] + beta_Contact * Contact_pct[i] + beta_Swing * Swing_pct[i], 1);
  }
}
"

# Compile the Stan model
stan_model <- stan_model(model_code = stan_code)

# Prepare the data
stan_data <- list(
  N = nrow(fangraphs_data_2023),
  Not_Walked_pct = fangraphs_data_2023$Not_Walked_pct,
  Contact_pct = fangraphs_data_2023$Contact_pct,
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
beta_BB_mean <- mean(posterior_samples$beta_BB)
beta_Contact_mean <- mean(posterior_samples$beta_Contact)
beta_Swing_mean <- mean(posterior_samples$beta_Swing)

# Iterate over each row of fangraphs_data_2023
for (i in 1:nrow(fangraphs_data_2023)) {
  # Calculate prediction for the i-th player using the mean of posterior samples
  predictions[i] <- rnorm(1, 
                          mean = alpha_mean + 
                            beta_BB_mean * fangraphs_data_2023$Not_Walked_pct[i] + 
                            beta_Contact_mean * fangraphs_data_2023$Contact_pct[i] +
                            beta_Swing_mean * fangraphs_data_2023$Swing_pct[i],
                          sd = 1)  # Assuming standard deviation of 1
}

# Add predictions to the dataframe
fangraphs_data_2023$Inning_Eater <- predictions

# View the dataframe with predictions
head(fangraphs_data_2023)

# Perform Bayesian inference (sampling) again
bayesian_fit <- sampling(stan_model, data = stan_data)

# Print summary of the Bayesian fit again
print(bayesian_fit)

# Select the desired columns
predictions_eater <- fangraphs_data_2023 %>%
  select(NameASCII, Inning_Eater)

# View the resulting dataframe
head(predictions_eater)

