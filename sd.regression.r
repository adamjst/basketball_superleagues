install.packages("tidyverse")
install.packages("lmtest")
install.packages("rlang")
install.packages("sandwich")

library(lmtest)
library(sandwich)
library(tidyverse)
###REGRESSION Of Real against reference Permuted Data
# Winning pct for all 4 pro basketball leagues
all_assoc_pct <- read.csv("winning_pct_all.csv")

summary(all_assoc_pct)

all_assoc_pct <- all_assoc_pct |>
  mutate(Era = case_when(Season <= 23 ~ 1,   # Three eras, 23 years, 22 years, 22 years
                         Season >= 24 & Season <= 45 ~ 2,
                         Season >= 46 ~ 3)) |>
  mutate(Era = ifelse(NBA == 0, NA, Era)) |> #Only NBA
  mutate(LastDecade = case_when(Season >= 60 ~ 1,
                                Season <= 10 ~ 0)) |> # Divide by first and last decade
  mutate(LastDecade = ifelse(NBA == 0, NA, LastDecade)) #Only NBA



cor(all_assoc_pct)

#Is there a stastically significant difference between the real and permuted data?
regress_pct <- function(df, ...){
  ##Run linear regression models and summarize
  # Capture all predictor variables
  predictors <- ensyms(...)
  
  # Convert to character and collapse into a formula string
  predictor_vars <- sapply(predictors, as_string)
  
  predictor_str <- paste(predictor_vars, collapse = " + ")
  print(predictor_str)
  
  formula_str <- paste(as_string("sd"), "~", predictor_str)
  print(formula_str)
  ##NBA as reference
  model <- lm(formula = as.formula(formula_str), data = df)
  coefs <- coeftest(model, vcov = vcovHC(model, "HC1")) 
  
  print(summary(model))
  print(coefs)
  
}
##Apply model
model1_pctSeason <- regress_pct(all_assoc_pct, Season, ABA, BAA, WNBA)
model2_pctEra <- regress_pct(all_assoc_pct, Era)
model3_pctLastDecade <- regress_pct(all_assoc_pct, LastDecade)

#T Statistic based on the robustness-checked coefs
t_pct <- model1_pctSeason[2]/model1_pctSeason[7]

