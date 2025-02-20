library(ggplot2)
library(reshape2)
library(stringr)
library(magrittr)


all_assoc_plot <- ggplot(molten_assoc, aes(x=Season.Start, y = Transitivity.Violation.Rate, 
                                           shape = Association, color = Association))

### PLOT RESULTS ###

###Create plotting function with trendline
real_perm_plot <- function(league){
  ggplot(league, aes(x=Season.Start, y = TVR, group=Assoc, col=Assoc, fill=Assoc))+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-(Mean-Min.95), ymax=Mean+(Max.95-Mean), group = Assoc), width=.2,
                  position=position_dodge(.3))+
    geom_smooth(method = 'lm', se = FALSE)+
    scale_color_manual(values = c("TVR"= "black", "NULL.TVR" = "gray77"))+
    scale_shape_manual(values = c("TVR" = 15, "NULL.TVR" = 17))+
    scale_size_manual(values = c("TVR" = 8, "NULL.TVR" = 8)) +
    xlab('Season') +
    ylab('Transitivity Violation Rate') +
  theme_bw()
}

#Plot each association
Assoc_plot(all_assoc)
Assoc_plot(all_ABA)
Assoc_plot(all_BAA)
Assoc_plot(out_NBA, control_out_NBA)
Assoc_plot(out_ABA)
Assoc_plot(out_BAA)
Assoc_plot(control_out_NBA)
Assoc_plot(control_out_ABA)
Assoc_plot(control_out_BAA)

###PLOT ALL ASSOCIATIONS TOGETHER###

##PLOT##
all_assoc_plot <- ggplot(molten_assoc, aes(x=Season.Start, y = Transitivity.Violation.Rate, 
                                        shape = Association, color = Association))+
  geom_point()+
  scale_color_manual(values = c("BAA" = "#fec524", "ABA" = "#8b0000", "NBA" = "#0e2240"))+
  scale_shape_manual(values = c("BAA" = 15, "ABA" = 17, "NBA" = 19))+
  scale_size_manual(values = c("BAA" = 8, "ABA" = 8, "NBA" = 8)) +
  geom_smooth(method = "lm")+
  xlab('Season') +
  ylab('Transitivity Violation Rate') +
  theme_bw()
all_assoc_plot



