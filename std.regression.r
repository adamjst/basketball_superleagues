library(lmtest)
library(sandwich)

###REGRESSION Of Real against reference Permuted Data

###REGRESSION Of Real against reference Permuted Data

all_assoc_pct$Season.num <- as.numeric(rownames(all_assoc_pct))
all_assoc_pct[71:79, 4] <- c(as.numeric(1:9))
all_assoc_pct[80:82, 4] <- c(as.numeric(1:3))
all_assoc_pct[83:105, 4] <- c(as.numeric(1:23))


all_assoc_pct$NBA <- ifelse(all_assoc_pct$Assoc == 'NBA', 1, 0)
all_assoc_pct$ABA <- ifelse(all_assoc_pct$Assoc == 'ABA', 1, 0)
all_assoc_pct$BAA <- ifelse(all_assoc_pct$Assoc == 'BAA', 1, 0)
all_assoc_pct$WNBA <- ifelse(all_assoc_pct$Assoc == 'WNBA', 1, 0)
all_assoc_pct <- all_assoc_pct[-c(3)]
summary(all_assoc_pct)

all_assoc_pct <- all_assoc_pct %>%
  mutate(era = case_when(Season.num <= 23 ~ 1,
                         Season.num >= 24 & Season.num <= 47 ~ 2,
                         Season.num >= 48 ~ 3)) %>%
  mutate(decade = case_when(Season.num <= 10 ~ 1,
                            Season.num >= 11 & Season.num <= 20 ~ 2,
                            Season.num >= 21 & Season.num <= 30 ~ 3,
                            Season.num >= 31 & Season.num <= 40 ~ 4,
                            Season.num >= 41 & Season.num <= 50 ~ 5,
                            Season.num >= 51 & Season.num <= 60 ~ 6,
                            Season.num >= 61 ~ 7))


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
  lm.std_pct <- lm(sd ~ Decade + WNBA)
  summary(lm.std_pct)
  #coeftest(lm.std_pct, vcov = vcovHC(lm.std_pct, "HC1"))   # HC1 gives us the White standard errors

  
}
##Apply model
regress_pct(all_assoc_pct)

