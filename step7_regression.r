library(lmtest)
library(sandwich)
library(dplyr)
library(car)
library(usdm)
library(here)
library(tidyverse)
###REGRESSION Of Real against reference Permuted Data

# divisions <- read.csv(here('Divisions.csv'))
# names(divisions)[1] <- "Season.Start"
# divisions <- divisions[-c(2, 6:14)]

#Convert associations to dummies
all_assoc_delta$NBA <- ifelse(all_assoc_delta$Assoc == 'NBA', 1, 0)
all_assoc_delta$ABA <- ifelse(all_assoc_delta$Assoc == 'ABA', 1, 0)
all_assoc_delta$BAA <- ifelse(all_assoc_delta$Assoc == 'BAA', 1, 0)
all_assoc_delta$WNBA <- ifelse(all_assoc_delta$Assoc == 'WNBA', 1, 0)
all_assoc_delta <- all_assoc_delta[-c(6)]
#Transform temporal variables Season and Year
all_assoc_delta$min_season <- all_assoc_delta$Season.Start - min(all_assoc_delta$Season.Start)
all_assoc_delta$years_since_pro <- all_assoc_delta$min_season - all_assoc_delta$Season.num + 1
# #Account for work stoppage years in NBA 1998 and 2011





#merge with divisions/games/team numbers data
#all_assoc_delta <- merge(all_assoc_delta, divisions, by='Season.Start', all.x = TRUE) 
summary(all_assoc_delta)

all_assoc_delta <- all_assoc_delta %>%
  dplyr::select(c(TVR.delta, Season.Start, Season.num, NBA, BAA, ABA, WNBA)) %>%
  distinct()
# 
all_assoc_delta_reg <- all_assoc_delta %>%
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
  mutate(first.last.dec = case_when(Season.num >= 58 ~ 1,
                                    Season.num <= 10 ~ 0)) %>% # Divide by first and last decade
  mutate(first.last.dec = ifelse(NBA == 0, NA, first.last.dec))

  
#Is there a stastically significant difference between the real and permuted data?
regress_delta <- function(league){
  ## Set up variables
  Diff <- league$TVR.delta
  Year <- league$Season.Start
  Season <- league$Season.num
  #NumGames <- league$NumGames
  #Teams <- league$Teams
  #SubLeagues <- league$Num_SubLeagues
  #Professionalization <- league$min_season
  #Years_since_league_found <- league$years_since_pro
  NBA <- league$NBA
  ABA <- league$ABA
  BAA <- league$BAA
  WNBA <- league$WNBA
  Era <- league$era
  Decade <- league$decade
  LastDecade <- league$first.last.dec
  
  ##Run linear regression models and summarize
  ##NBA as reference
  lm.TVRs <- lm(Diff ~ LastDecade)

  #bptest(lm.TVRs)
  #coefficients(lm.TVRs)
  print(coefficients(lm.TVRs, AIC(lm.TVRs)))
  summary(lm.TVRs)
  #coeftest(lm.TVRs, vcov = vcovHC(lm.TVRs, "HC1"))   # HC1 gives us the White standard errors

}

##Apply model
x <- cor(all_assoc_delta, use = "complete.obs")
regress_delta(all_assoc_delta_reg)

out_WNBA_delta <- out_WNBA_delta[-c(6)]
out_WNBA_delta$Season.Start <- seq(1,22,1)

#POST-HOC wnba analysis
lm.wnba_delta <- lm(out_WNBA_delta$TVR.delta ~ out_WNBA_delta$Season.Start)
summary(lm.wnba_delta)
coeftest(lm.wnba_delta, vcov = vcovHC(lm.wnba_delta, "HC1"))   # HC1 gives us the White standard errors


#Regression of BASELINE
regress_baseline <- function(league){
  ## Set up variables
  baseline <- league$NULL.TVR
  Season <- league$Season.num
  Year <- league$Season.Start
  NBA <- league$NBA
  ABA <- league$ABA
  BAA <- league$BAA
  WNBA <- league$WNBA
  
  ##Run linear regression models and summarize
  ##NBA as reference
  lm.TVR_base <- lm(baseline ~ Year + ABA + BAA + WNBA)
  
  ##heteroskedasticity
  bptest(lm.TVR_base)
  summary(lm.TVR_base)
}
##Apply model
regress_baseline(all_assoc_delta)

