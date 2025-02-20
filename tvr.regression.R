library(tidyverse)
library(lmtest)
library(rlang)
library(sandwich)

#Transitivity Violation Rate Delta
tvr <- read.csv('tvr_regression_table.csv')

regress_delta <- function(df, ...){
  
  # Capture all predictor variables
  predictors <- ensyms(...)
  #Check whether to add the Season*Year interaction
  if ("Season" %in% predictors && "Year" %in% predictors){
    predictors <- append(predictors, "Season*Year")
  }
  # Convert to character and collapse into a formula string
  predictor_vars <- sapply(predictors, as_string)

  predictor_str <- paste(predictor_vars, collapse = " + ")
  print(predictor_str)

  formula_str <- paste(as_string("TVR.delta"), "~", predictor_str)
  print(formula_str)
  ##Run linear regression models and summarize
  ##NBA as reference
  model <- lm(formula = as.formula(formula_str), data = df)
  coefs <- coeftest(model, vcov = vcovHC(model, "HC1")) 
  
  print(summary(model))

  print(coefs)
}

model1_season <- regress_delta(tvr, Season, ABA, BAA, WNBA)
model2_year <- regress_delta(tvr, Year, ABA, BAA, WNBA)
model3_seasonyear <- regress_delta(tvr, Season, Year, ABA, BAA, WNBA)
model4_era <- regress_delta(tvr, Era)
model5_lastdecade <- regress_delta(tvr, LastDecade)

#T Statistic based on the robustness-checked coefs
t <- model1_season[2]/model1_season[7]

## WNBA Model
#Only WNBA seasons
wnba_only <- tvr |>
  filter(WNBA == 1)

model6_wnba <- regress_delta(wnba_only, Season)
