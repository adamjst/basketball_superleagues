library(dplyr)                                            ####KEY####
library(here)                                             ###Three pound signs = a new section.###
library(ggplot2)                                          ##Two pound signs = explanatory statement of code##       
library(reshape2)                                         #One pound sign = optional print point. Take off to see what is happening under the hood.#

here()
##read data from giant csv.##
nba_pct <- data.frame(read.csv(here('nba_pct_clean.csv')))
nba_pct$sd <- apply(nba_pct, 1, sd, na.rm=TRUE)
nba_pct$Season.Start <- c(seq(1949, 2018, 1))
nba_pct <- nba_pct[-c(1:45)]
nba_pct$Assoc <- 'NBA'

#baa#
baa_pct <- data.frame(read.csv(here('baa_pct_clean.csv')))
baa_pct$sd <- apply(baa_pct, 1, sd, na.rm=TRUE)
baa_pct$Season.Start <- c(seq(1946, 1948, 1))
baa_pct <- baa_pct[-c(1:45)]
baa_pct$Assoc <- 'BAA'


#aba
aba_pct <- data.frame(read.csv(here('aba_pct_clean.csv')))
aba_pct$sd <- apply(aba_pct, 1, sd, na.rm=TRUE)
aba_pct$Season.Start <- c(seq(1967, 1975, 1))
aba_pct <- aba_pct[-c(1:12)]
aba_pct$Assoc <- 'ABA'


#wnba_pct <- data.frame(read.csv(here('nba_pct_clean.csv')))
wnba_pct <- data.frame(read.csv(here('wnba_pct_clean.csv')))
wnba_pct$sd <- apply(wnba_pct, 1, sd, na.rm=TRUE)
wnba_pct$Season.Start <- c(seq(1997, 2019, 1))
wnba_pct <- wnba_pct[-c(1:18)]
wnba_pct$Assoc <- 'WNBA'


###Visualize

sd_plot<- function(league){
  ggplot(league, aes(Season.Start, sd))+
    geom_point()+
    geom_smooth(method = "lm")+
    ggtitle("Standard Deviation of Winning Pct")
}

sd_plot(nba_pct)
sd_plot(baa_pct)
sd_plot(aba_pct)
sd_plot(wnba_pct)



##stack dataframes
baa_wnba_pct <- rbind(baa_pct, wnba_pct)
nba_aba_pct <- rbind(nba_pct, aba_pct)
all_assoc_pct <- rbind(nba_aba_pct, baa_wnba_pct)
head(all_assoc_pct)

all_plot <- function(league){
  ggplot(league, aes(x=Season.Start, y = sd, shape = Assoc, color = Assoc))+
    geom_point()+
    scale_color_manual(values = c("BAA" = "#fec524", "ABA" = "#8b0000", "NBA" = "#0e2240", "WNBA" = '#244289'))+
    scale_shape_manual(values = c("BAA" = 15, "ABA" = 17, "NBA" = 19, 'WNBA' = 21))+
    scale_size_manual(values = c("BAA" = 8, "ABA" = 8, "NBA" = 8, "WNBA" = 8)) +
    geom_smooth(method = 'lm', se = FALSE)+
    xlab('Season') +
    ylab('Standard Deviation in Winning Pct.') +
    theme(axis.text = element_text(face="bold", size=14), 
             panel.background = element_blank(),
             plot.title = element_text(face='bold', size=16, hjust=0.5),
             legend.title = element_blank(),
             legend.text = element_text(face = 'bold',  size = 14),
             legend.position = 'bottom',
             axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
             axis.title.y = element_text(face='bold',size=14, margin=margin(0,20,0,0)))
}
all_plot(all_assoc_pct)

