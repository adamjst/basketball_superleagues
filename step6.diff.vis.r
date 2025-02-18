library(ggplot2)
library(reshape2)
library(stringr)
library(magrittr)
library(here)


##Force numeric and add association column
out_NBA_delta$Season.Start <- as.numeric(rownames(out_NBA_delta))
out_NBA_delta$Season.num <- c(as.numeric(1:67))
out_ABA_delta$Season.Start <- as.numeric(rownames(out_ABA_delta))
out_ABA_delta$Season.num <- c(as.numeric(1:8))
out_BAA_delta$Season.Start <- as.numeric(rownames(out_BAA_delta))
out_BAA_delta$Season.num <- c(as.numeric(1:3))
out_BAA_delta$Assoc <- 'BAA'
out_WNBA_delta$Assoc <- 'WNBA'
out_WNBA_delta$Season.Start <- as.numeric(rownames(out_WNBA_delta))
out_WNBA_delta$Season.num <- c(as.numeric(1:22))
out_WNBA_delta <- out_WNBA_delta[, c(1,2,3,4,5,9,6,7,8)]

##stack dataframes
BAA_WNBA_delta <- rbind(out_BAA_delta, out_WNBA_delta)
NBA_ABA_delta <- rbind(out_NBA_delta, out_ABA_delta)
all_assoc_delta <- rbind(NBA_ABA_delta, BAA_WNBA_delta)

### PLOT RESULTS ###

###Create plotting function with trendline
Assoc_plot <- function(league){
  ggplot(league, aes(x=Season.Start, y = TVR.delta))+
    geom_point()+
    geom_errorbar(aes(ymin=TVR.delta-abs(Min.95+TVR.delta), ymax=TVR.delta+abs(Max.95+TVR.delta)))+
    geom_smooth(method = 'lm', se = FALSE)+
    xlab('Season') +
    ylab('Difference in Transitivity Violation Rate') +
    theme_bw()
}

#Plot each association
Assoc_plot(out_NBA_delta)
Assoc_plot(out_ABA_delta)
Assoc_plot(out_BAA_delta)
Assoc_plot(out_WNBA)
Assoc_plot(all_assoc)

#plot all associations together
all_plot <- function(league){
  ggplot(league, aes(x=Season.Start, y = TVR.delta, shape = Assoc, color = Assoc))+
    geom_point()+
    geom_errorbar(aes(ymin=TVR.delta-abs(TVR.delta+Min.95), ymax=TVR.delta+abs(TVR.delta+Max.95)))+    scale_color_manual(values = c("BAA" = "#fec524", "ABA" = "#8b0000", "NBA" = "#0e2240", "WNBA" = '#244289'))+
    scale_shape_manual(values = c("BAA" = 15, "ABA" = 17, "NBA" = 19, 'WNBA' = 21))+
    scale_size_manual(values = c("BAA" = 8, "ABA" = 8, "NBA" = 8, "WNBA" = 8)) +
    geom_smooth(method = 'lm', se = FALSE, size=2)+
    xlab('Season') +
    ylab('Difference in Transitivity Violation Rate \n(Observed-Baseline)') +
    theme(axis.text = element_text(face="bold", size=14), 
          panel.background = element_blank(),
          plot.title = element_text(face='bold', size=16, hjust=0.5),
          legend.title = element_blank(),
          legend.text = element_text(face = 'bold',  size = 14),
          legend.position = 'bottom',
          axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(face='bold',size=14, margin=margin(0,20,0,0)))
}
all_plot(all_assoc)

#plot baseline results
all_plot_baseline <- function(league){
  ggplot(league, aes(x=Season.Start, y = NULL.TVR, shape = Assoc, color = Assoc))+
    scale_color_manual(values = c("BAA" = "#fec524", "ABA" = "#8b0000", "NBA" = "#0e2240", "WNBA"="#244289"))+
    scale_shape_manual(values = c("BAA" = 15, "ABA" = 17, "NBA" = 19, 'WNBA' = 21))+
    scale_size_manual(values = c("BAA" = 8, "ABA" = 8, "NBA" = 8, "WNBA" = 8)) +
    geom_smooth(method = 'lm', se = FALSE)+
    geom_smooth(aes(y=TVR), method = 'lm', se=FALSE)+
    annotate("text", x=1983, y=0.243, label = "Intransitivity baseline rate")+
    annotate("text", x=1983, y=0.210, label= "Intransitivity observed rate")+
    xlab('Season') +
    ylab('Transitivity Violation Rate (Baseline)') +
    theme(panel.background = element_blank(),
          plot.title = element_text(face='bold', size=16, hjust=0.5),
          legend.title = element_blank(),
          legend.text = element_text(face = 'bold',  size = 14),
          legend.position = 'bottom',
          axis.title.x = element_text(face='bold',size=14, margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(face='bold',size=14, margin=margin(0,20,0,0)))
}
all_plot_baseline(all_assoc)

