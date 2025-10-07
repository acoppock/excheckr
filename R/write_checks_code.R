

write_attrition_check_code <- function(data, missingness_dummies = list(), .method = lm_robust, ...){

  # step 1: select the variables in missingness dummies. could this be a tidy-select specification with starts with "Y_" and ends with "_missing" as a default

  # step 2: apply the regression model in .method with the args passed to ... on each of the missignness dummies

  # step 3: tidy the output and return as a data frame

}

write_balance_check_code <- function(data, covariates = list(), .method = lm_robust, ...){

  # step 1: select the variables in covariates. could this be a tidy-select specification with starts with "X_" as a default?

  # step 2: apply the regression model in .method with the args passed to ... on each of the covariates.
  # one complication I anticipate is categorical covariates. Could the function pre-process such covariates to run on dummy variables that equal 1 for each level and 0 otherwise (much like how a regression handles it)

  # step 3: tidy the output and return as a data frame

  # step

}
