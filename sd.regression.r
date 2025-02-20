library(lmtest)
library(sandwich)
library(tidyverse)
###REGRESSION Of Real against reference Permuted Data
# Winning pct for all 4 pro basketball leagues
all_assoc_pct <- read.csv("winning_pct_all.csv")

summary(all_assoc_pct)

all_assoc_pct <- all_assoc_pct %>%
  mutate(era = case_when(Season.num <= 23 ~ 1,   # Three eras, 23 years, 22 years, 22 years
                         Season.num >= 24 & Season.num <= 45 ~ 2,
                         Season.num >= 46 ~ 3)) %>%
  mutate(era = ifelse(NBA == 0, NA, era)) %>%
  mutate(decade = case_when(Season.num <= 10 ~ 1, # Divide by 10 years
                            Season.num >= 11 & Season.num <= 20 ~ 2,
                            Season.num >= 21 & Season.num <= 30 ~ 3,
                            Season.num >= 31 & Season.num <= 40 ~ 4,
                            Season.num >= 41 & Season.num <= 50 ~ 5,
                            Season.num >= 51 & Season.num <= 60 ~ 6,
                            Season.num >= 61 ~ 7)) %>%
  mutate(first.last.dec = case_when(Season.num >= 60 ~ 1,
                                    Season.num <= 10 ~ 0)) %>% # Divide by first and last decade
  mutate(first.last.dec = ifelse(NBA == 0, NA, first.last.dec))



cor(all_assoc_pct)

#Is there a stastically significant difference between the real and permuted data?
regress_pct <- function(league){
  ## Set up variables
  sd <- league$sd
  Year <- league$Season.Start
  Season <- league$Season.num
  Era <- league$era
  Decade <- league$decade
  NBA <- league$NBA
  ABA <- league$ABA
  BAA <- league$BAA
  WNBA <- league$WNBA
  
  ##Run linear regression models and summarize
  ##NBA as reference
  lm.std_pct <- lm(sd ~ Decade)
  coefs <- coeftest(lm.std_pct, vcov = vcovHC(lm.std_pct, "HC1"))   # HC1 gives us the White standard errors
  print(summary(lm.std_pct))
  
  print(coefs)
  
}
##Apply model
pct_sd_regression <- regress_pct(all_assoc_pct)
#T Statistic based on the robustness-checked coefs
t_pct <- pct_sd_regression[2]/pct_sd_regression[7]

