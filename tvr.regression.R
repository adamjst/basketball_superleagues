install.packages("tidyverse")
install.packages("lmtest")
install.packages("rlang")
install.packages("sandwich")

library(tidyverse)
library(lmtest)
library(rlang)
library(sandwich)

#Transitivity Violation Rate Delta
tvr <- read.csv('tvr_regression_table.csv')

tvr <- tvr |>
  mutate(Era = case_when(Season <= 23 ~ 1,   # Three eras, 23 years, 22 years, 22 years
                         Season >= 24 & Season <= 45 ~ 2,
                         Season >= 46 ~ 3)) |>
  mutate(Era = ifelse(NBA == 0, NA, Era)) |> #Only NBA
  mutate(LastDecade = case_when(Season >= 60 ~ 1,
                                    Season <= 10 ~ 0)) |> # Divide by first and last decade
  mutate(LastDecade = ifelse(NBA == 0, NA, LastDecade)) #Only NBA


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

#T Statistic based on the robustness-checked coefs for model 1
t <- model1_season[2]/model1_season[7]

## WNBA Model
#Only WNBA seasons
wnba_only <- tvr |>
  filter(WNBA == 1)

model6_wnba <- regress_delta(wnba_only, Season)
