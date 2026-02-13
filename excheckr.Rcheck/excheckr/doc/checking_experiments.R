## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(excheckr)
library(tidyverse)
library(estimatr)

set.seed(2024)

## ----simulate-data------------------------------------------------------------
n <- 500

# Treatment assignment (randomized)
dat <- data.frame(
  Z = rbinom(n, 1, 0.5),
  cluster_id = sample(1:25, n, replace = TRUE)
)

# Covariates with realistic missingness patterns
dat <- dat |>
  mutate(
    # Demographics
    X_age = rnorm(n, 45, 15),
    X_gender = sample(c("Male", "Female", "Other"), n, replace = TRUE, 
                      prob = c(0.48, 0.48, 0.04)),
    X_income = exp(rnorm(n, 11, 0.8)) * 1000,
    
    # Political variables
    X_party = factor(sample(c("Democrat", "Republican", "Independent"), n, 
                           replace = TRUE, prob = c(0.35, 0.30, 0.35))),
    X_ideology = sample(1:7, n, replace = TRUE),
    
    # Education
    X_education = factor(sample(c("HS", "Some College", "BA", "Graduate"), n,
                               replace = TRUE, prob = c(0.25, 0.30, 0.30, 0.15)))
  )

# Introduce MCAR missingness in covariates
dat$X_age[sample(1:n, 25)] <- NA
dat$X_income[sample(1:n, 40)] <- NA
dat$X_party[sample(1:n, 30)] <- NA
dat$X_ideology[sample(1:n, 35)] <- NA

# Outcomes
# Y_attitude: treatment increases missingness slightly (differential attrition)
# Y_behavior: no differential attrition
dat <- dat |>
  mutate(
    Y_attitude_latent = 3 + 0.5 * Z + 0.01 * X_age + rnorm(n),
    Y_behavior_latent = 0.3 + 0.2 * Z + rnorm(n, 0, 0.5)
  )

# Create missingness in outcomes
# Y_attitude: 25% missing in control, 35% missing in treatment (differential!)
miss_attitude <- rbinom(n, 1, ifelse(dat$Z == 1, 0.35, 0.25))
dat$Y_attitude <- ifelse(miss_attitude == 1, NA, dat$Y_attitude_latent)

# Y_behavior: 20% missing in both groups (no differential attrition)
miss_behavior <- rbinom(n, 1, 0.20)
dat$Y_behavior <- ifelse(miss_behavior == 1, NA, dat$Y_behavior_latent)

# Remove latent variables
dat <- dat |> select(-Y_attitude_latent, -Y_behavior_latent)

# View first few rows
head(dat)

## ----check-missingness--------------------------------------------------------
# Check missingness patterns
check_covariate_missingness(dat)

## ----generate-imputation-code-------------------------------------------------
# Generate imputation code for all X_ variables
write_covariate_imputation_code(dat)

## ----apply-imputation---------------------------------------------------------
dat <-
  dat |>
  mutate(
    X_age_nona = replace_na(X_age, median(X_age, na.rm = TRUE)),
    X_age_missing = if_else(is.na(X_age), 1, 0),
    X_gender_nona = replace_na(X_gender, stat_mode(X_gender)),
    X_gender_missing = if_else(is.na(X_gender), 1, 0),
    X_income_nona = replace_na(X_income, median(X_income, na.rm = TRUE)),
    X_income_missing = if_else(is.na(X_income), 1, 0),
    X_party_nona = replace_na(X_party, stat_mode(X_party)),
    X_party_missing = if_else(is.na(X_party), 1, 0),
    X_ideology_nona = replace_na(X_ideology, median(X_ideology, na.rm = TRUE)),
    X_ideology_missing = if_else(is.na(X_ideology), 1, 0),
    X_education_nona = replace_na(X_education, stat_mode(X_education)),
    X_education_missing = if_else(is.na(X_education), 1, 0)
  )

# Verify no missingness in imputed variables
dat |>
  summarize(
    across(ends_with("_nona"), ~sum(is.na(.)))
  )

## ----generate-outcome-missing-------------------------------------------------
# Generate code for outcome missingness indicators
write_outcome_missingness_dummies_code(dat)

## ----apply-outcome-missing----------------------------------------------------
dat <- 
  dat |>
  mutate(
    Y_attitude_missing = if_else(is.na(Y_attitude), 1, 0),
    Y_behavior_missing = if_else(is.na(Y_behavior), 1, 0)
  )

# Check missingness rates by treatment
dat |>
  group_by(Z) |>
  summarize(
    n = n(),
    attitude_missing_rate = mean(Y_attitude_missing),
    behavior_missing_rate = mean(Y_behavior_missing)
  )

## ----check-balance------------------------------------------------------------
# Check balance on imputed covariates
balance_results <- check_balance(
  dat, 
  treatment = Z,
  covariates = c("X_age_nona", "X_income_nona", "X_ideology_nona", 
                 "X_gender", "X_party_nona", "X_education"),
  clusters = cluster_id,
  se_type = "CR2"
)

## ----generate-balance-code----------------------------------------------------
write_balance_check_code(
  dat,
  treatment = Z,
  covariates = c("X_age_nona", "X_income_nona"),
  clusters = cluster_id,
  se_type = "CR2"
)

## ----check-attrition----------------------------------------------------------
# Check if treatment predicts outcome missingness
attrition_results <- check_attrition(
  dat,
  treatment = Z,
  clusters = cluster_id,
  se_type = "CR2"
)

## ----generate-attrition-code--------------------------------------------------
write_attrition_check_code(
  dat,
  treatment = Z,
  clusters = cluster_id,
  se_type = "CR2"
)

## ----check-missing-balance----------------------------------------------------
# Check if treatment predicts covariate missingness
missing_balance <- check_balance(
  dat,
  treatment = Z,
  covariates = c("X_age_missing", "X_income_missing", 
                 "X_party_missing", "X_ideology_missing"),
  clusters = cluster_id,
  se_type = "CR2"
)

