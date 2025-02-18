library(dplyr)                                            ####KEY####
library(here)                                             ###Three pound signs = a new section.###
library(ggplot2)                                          ##Two pound signs = explanatory statement of code##       
library(reshape2)                                         #One pound sign = optional print point. Take off to see what is happening under the hood.#

here()
##read data from giant csv.##
##Separates calendar year from season year. If month number is less than 10 (October, then subtract one from given year).##
total <- total %>%
  mutate(SeasonStart = if_else(total$Month < 10, -1, 0))

total$SeasonStart <- total$Year + total$SeasonStart

total$Association <- as.character(total$Association)

##Creates Season string to represent the whole season.##
total$Season <- paste(total$SeasonStart, "-", total$SeasonStart+1)


##Drop id columns and adjust column names.##
total <- total[ -c(1,2) ]
names(total)[2] <- 'Visitors'
names(total)[3] <- 'Vis.Pts'
names(total)[4] <- 'Home'
names(total)[5] <- 'Home.Pts'
names(total)[6] <- 'Box Score'
names(total)[7] <- 'URL'

##convert points to numeric.##
total$Vis.Pts <- as.numeric(total$Vis.Pts)
total$Home.Pts <- as.numeric(total$Home.Pts)

##PERMUTE
total$NULL.Vis.Pts <- sample(as.numeric(total$Vis.Pts))
total$NULL.Home.Pts <- sample(as.numeric(total$Home.Pts))

##Set counter for transitivity.##
transitivity <- 0
no_transitivity <- 0
NULL.transitivity <- 0
NULL.no_transitivity <- 0

## Create containers for catching TVRs ##
rate <- data.frame(matrix(nrow = 5000, ncol = 1))
NULL.rate <- data.frame(matrix(nrow = 5000, ncol = 1))

###TRANSITIVITY FUNCTION: EXTRACTS 3 TEAMS FOR EACH YEAR. TESTS TRANSITIVITY. ADJUSTS TRANSITIVITY RATE. BUMPS UP COUNTER.###
##Three arguments: Association (NBA, ABA, BAA), year, number of iterations.##
transitivity_test <- function(Association, year, num_iterations){
  ##Converts argument into something usable for loop.##
  num_its <- num_iterations
  rate <- data.frame(matrix())
  ##Loop based on iteration argument##
  for(i in seq(1, num_its, 1)){
    ##Counts iteration.##
    #print(i)
    
    ##Subsets the games for the particular association passed in the function's argument.##
    total <- total[ which(total$Association == Association), ]
    
    #create list of teams competing in one year.##
    season_year <- total[ which(total$SeasonStart == year), ]
    teams <- as.character(unique(season_year$Visitors))
    
    ##Extracts three teams randomly.##
    sample <- sample(teams, size = 3, replace = FALSE)
    sample1 <- sample[1]
    sample2 <- sample[2]
    sample3 <- sample[3]
    
    ###OBTAIN BOXSCORES OF SAMPLED TEAMS###
    
    ##Find the boxscore results from the first sampled team.##
    sampled_games1 <- season_year[ which(season_year$Home == sample1 | season_year$Visitors == sample1), ]
    
    ##Find the results from the first sampled team's schedule in which they faced team two.##
    game1 <- sampled_games1[ which(sampled_games1$Home == sample2 | sampled_games1$Visitors == sample2), ]
    
    ##Find the boxscore results from the second sampled team.##
    sampled_games2 <- season_year[ which(season_year$Home == sample2 | season_year$Visitors == sample2), ]
    
    ##Find the results from the second sampled team's schedule in which they faced team three.##
    game2 <- sampled_games2[ which(sampled_games2$Home == sample3 | sampled_games2$Visitors == sample3), ]
    
    ##Find the boxscore results from the first sampled team's schedule in which they faced team three.##
    game3 <- sampled_games1[ which(sampled_games1$Home == sample3 | sampled_games1$Visitors == sample3), ]
    
    ###FIND RANDOM GAME AMONG BOXSCORES AND DETERMINE WINNER###
    
    ###GAME 1###
    ##Find random game involving two teams in same year.##
    random_game1 <- game1[sample(nrow(game1), 1), ]
    
    ##Determine which points total is greater##
    won1 <- pmax(random_game1$Vis.Pts, random_game1$Home.Pts)
    NULL.won1 <- pmax(random_game1$NULL.Vis.Pts, random_game1$NULL.Home.Pts)
    
    ##Identify winner and convert to character##
    winner1 <- if_else(won1 == random_game1$Vis.Pts, random_game1$Visitors, random_game1$Home)
    NULL.winner1 <- if_else(NULL.won1 == random_game1$NULL.Vis.Pts, random_game1$Visitors, random_game1$Home)
    winner1 <- as.character(winner1) 
    NULL.winner1 <- as.character(NULL.winner1)
    
    ###GAME 2###
    ##Find random other game involving second set of two.##
    random_game2 <- game2[sample(nrow(game2), 1), ]
    
    #Determine which points total is greater.##
    won2 <- pmax(random_game2$Vis.Pts, random_game2$Home.Pts)
    NULL.won2 <- pmax(random_game2$NULL.Vis.Pts, random_game2$NULL.Home.Pts)
    
    
    #Identify winner and convert to character.##
    winner2 <- if_else(won2 == random_game2$Vis.Pts, random_game2$Visitors, random_game2$Home)
    NULL.winner2 <- if_else(NULL.won2 == random_game2$NULL.Vis.Pts, random_game2$Visitors, random_game2$Home)
    winner2 <- as.character(winner2)
    NULL.winner2 <- as.character(NULL.winner2)  
    
    
    ##Game 3##
    ##Find random other game involving first and third team.##
    random_game3 <- game3[sample(nrow(game3), 1), ]
    #print(random_game3) #print to check the year of competition
    
    
    ##Determine which points total is greater.##
    won3 <- pmax(random_game3$Vis.Pts, random_game3$Home.Pts)
    NULL.won3 <- pmax(random_game3$NULL.Vis.Pts, random_game3$NULL.Home.Pts)
    
    
    ##Identify winner and convert to character.##
    winner3 <- if_else(won3 == random_game3$Vis.Pts, random_game3$Visitors, random_game3$Home)
    NULL.winner3 <- if_else(NULL.won3 == random_game3$Vis.Pts, random_game3$Visitors, random_game3$Home)
    winner3 <- as.character(winner3)
    NULL.winner3 <- as.character(NULL.winner3)
    
    ###DETERMINE TRANSITIVITY AMONG GAME WINNERS###
    
    ##Check to ensure at least one game between three teams.##
    if (length(winner1) == 0 | length(winner2) == 0 | length(winner3) == 0 ){
      next
    }
    
    if (length(NULL.winner1) == 0 | length(NULL.winner2) == 0 | length(NULL.winner3) == 0 ){
      next
    }
    ##Print winners to show evidence of transitivity##
    #print(winner1)
    #print(winner2)
    #print(winner3)
    
    ##Bump up the counter based on transitivity. Violation if all three are deltaerent. Else, transitivity is true.##
    if (length(unique(c(winner1,winner2,winner3))) != 3){
      #print('Transitivity')
      ##Count transitivity violation.##
      transitivity <- transitivity + 1
    } else{
      #print('Transitivity violation')
      ##Count no transitivity violation.##
      no_transitivity <- no_transitivity + 1
    }
    if (length(unique(c(NULL.winner1,NULL.winner2,NULL.winner3))) != 3){
      #print('Transitivity')
      ##Count transitivity.##
      NULL.transitivity <- NULL.transitivity + 1
    } else{
      #print('Transitivity violaiton')
      ##Count transitivity violation.##
      NULL.no_transitivity <- NULL.no_transitivity + 1
    }
    ##Calculate transitivity violation rate as transitivity divided by total tests##
    rate[i] <- no_transitivity/(transitivity + no_transitivity)
    NULL.rate[i] <- NULL.no_transitivity/(NULL.transitivity+NULL.no_transitivity)
    ##Print to see the change of rate over the duration of the iteration.##
  }
  ##Manipulate shape of real and permuted Transitivity Violation Rates
  rate <- rate[-c(2:5000), ]
  NULL.rate <- NULL.rate[-c(2:5000), ]
  rate <- data.frame(t(rate))
  #print(rate)
  NULL.rate <- data.frame(t(NULL.rate))
  #print(NULL.rate)
  ##Calculate real and permuted means of Transitivity Violation Rates
  mean.tvr <- mean(rate$X1)
  #print(mean.tvr)
  NULL.mean.tvr <- mean(NULL.rate$X1)
  #print(NULL.mean.tvr)
  ##Calculate deltaerence in real and permuted TRransitivity VIolation Rates
  TVR.delta <- c(rate$X1 - NULL.rate$X1)
  #print(TVR.delta)
  ##Calculate, mean and error bars of the TVR deltaerence
  sort.tvr.delta <- sort(TVR.delta, decreasing = TRUE)
  #print(sort.tvr.delta)
  mean.delta <- mean(TVR.delta)
  max.95.delta <- sort.tvr.delta[125]
  #print(max.95.delta)
  min.95.delta <- sort.tvr.delta[4875]
  #print(min.95.delta)
  ##Save to returnable list
  rate.list <- list(mean.tvr, NULL.mean.tvr, mean.delta, max.95.delta, min.95.delta)
  #print(rate.list)
  return(rate.list)
}

### APPLYING THE FUNCTION AND EXTRACTION OF TRANSITIVITY RATES ###

##WNBA###
#Set dataframe parameters. Row number based on number of seasons. Set column names.##
out_WNBA_delta <- data.frame(matrix(nrow = 5, ncol = 22))
#Create row counter##
WNBA_count <- 1
#Set loop based on years of association existence.##
for(y in seq(1997,2018,1)){
  print(y)
  #Apply transitivity test function to dataframe, row by row (counter) year by year (y). Currently, this is set at 500 iterations for each year.##
  out_WNBA_delta[WNBA_count] <- unlist(transitivity_test('WNBA', y, 5000))
  ##Bump up the counter by one to move to the next row##
  WNBA_count <- WNBA_count + 1
}
names(out_WNBA_delta)[1:22] <- seq(1997,2018,1)
out_WNBA_delta <- data.frame(t(out_WNBA_delta))
names(out_WNBA_delta)[1] <- 'TVR'
names(out_WNBA_delta)[2] <- 'NULL.TVR'
names(out_WNBA_delta)[3] <- 'TVR.delta'
names(out_WNBA_delta)[4] <- 'Max.95'
names(out_WNBA_delta)[5] <- 'Min.95'
out_WNBA_delta$Assoc <- 'WNBA'
View(out_WNBA_delta)

###BAA###
##Set dataframe parameters. Row number based on number of seasons. Set column names.##
out_BAA_delta <- data.frame(matrix(nrow = 5, ncol = 3))
##Create row counter##
BAA_count <- 1
##Set loop based on years of association existence.##
for(y in seq(1946,1948,1)){
  print(y)
  ##Apply transitivity test function to dataframe, row by row (counter) year by year (y). Currently, this is set at 5 iterations for each year.##
  out_BAA_delta[BAA_count] <- unlist(transitivity_test('BAA', y, 5000))
  ##Bump up the counter by one to move to the next row##
  BAA_count <- BAA_count + 1
}
##Clean columns and shape of resulting BAA dataframe
names(out_BAA_delta)[1:3] <- seq(1946,1948,1)
out_BAA_delta <- data.frame(t(out_BAA_delta))
names(out_BAA_delta)[1] <- 'TVR'
names(out_BAA_delta)[2] <- 'NULL.TVR'
names(out_BAA_delta)[3] <- 'TVR.delta'
names(out_BAA_delta)[4] <- 'Max.95'
names(out_BAA_delta)[5] <- 'Min.95'
out_BAA_delta$Assoc <- 'BAA'
View(out_BAA_delta)

###NBA##
##Set dataframe parameters. Row number based on number of seasons. Set column names.##
out_NBA_delta <- data.frame(matrix(nrow = 5, ncol = 70))
##Create row counter##
NBA_count <- 1
##Set loop based on years of association existence.##
for(y in seq(1949, 2018, 1)){
  print(y)
  ##Exclude lockout years##
  if (y == 1998 | y == 2011){
    NBA_count <- NBA_count + 1
    next
  }
  ##Apply transitivity test function to dataframe, row by row (counter) year by year (y). Currently, this is set at 5 iterations for each year.##
  out_NBA_delta[NBA_count] <- unlist(transitivity_test('NBA', y, 5000))
  ##Bump up the counter by one to move to the next row##
  NBA_count <- NBA_count + 1
}
##Clean columns and shape of resulting NBA dataframe
names(out_NBA_delta)[1:70] <- seq(1949,2018,1)
out_NBA_delta <- data.frame(t(out_NBA_delta))
names(out_NBA_delta)[1] <- 'TVR'
names(out_NBA_delta)[2] <- 'NULL.TVR'
names(out_NBA_delta)[3] <- 'TVR.delta'
names(out_NBA_delta)[4] <- 'Max.95'
names(out_NBA_delta)[5] <- 'Min.95'
##Omit lockout years
out_NBA_delta <- na.omit(out_NBA_delta)
out_NBA_delta$Assoc <- 'NBA'
View(out_NBA_delta)


###ABA###
##Set dataframe parameters. Row number based on number of seasons. Set column names##
out_ABA_delta <- data.frame(matrix(nrow = 5, ncol = 8))
##Create row counter##
ABA_count <- 1
##Set loop based on years of association existence.##
for(y in seq(1967,1974,1)){
  print(y)
  ##Apply transitivity test function to dataframe, row by row (counter) year by year (y). Currently, this is set at 5 iterations for each year.##
  out_ABA_delta[ABA_count] <- unlist(transitivity_test('ABA', y, 5000))
  ##Bump up the counter by one to move to the next row##
  ABA_count <- ABA_count + 1
}
##Clean columns and shape of resulting ABA dataframe
names(out_ABA_delta)[1:8] <- seq(1967,1974,1)
##Quick summary statistics and view dataframe##
out_ABA_delta <- data.frame(t(out_ABA_delta))
names(out_ABA_delta)[1] <- 'TVR'
names(out_ABA_delta)[2] <- 'NULL.TVR'
names(out_ABA_delta)[3] <- 'TVR.delta'
names(out_ABA_delta)[4] <- 'Max.95'
names(out_ABA_delta)[5] <- 'Min.95'
out_ABA_delta$Assoc <- 'ABA'
View(out_ABA_delta)

