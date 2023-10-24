library(ggplot2)


#clolors = c("#459DD6", "#844D88","#158EA7", "#8DC34A", "#F0CA42")

FONT_SIZE =  40


or_plot  = data.frame(Dataset = c("ABCD", "CHOP-ED", "MCS"),
                      or = c(2.19, 2.24, 3.4),
                      lowerci = c(1.91,2.12,2.46),
                      upperci = c(2.5,2.38,4.71)
                      )

or_plot$Dataset = factor( or_plot$Dataset , levels = c("MCS", "CHOP-ED", "ABCD"))

pa = ggplot(or_plot, aes(y=Dataset, x=or, xmin=lowerci, xmax=upperci))+ #, shape = tester
  #Add data points and color them black
  geom_point(color = 'black', size = 8)+
  #Add 'special' points for the summary estimates, by making them diamond shaped
  # geom_point(data=subset(dat, tester=='Summary'), color='black', shape=18, size=4)+
  #add the CI error bars
  geom_errorbarh(height=.15, linewidth = 2.8)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous( name='Odds Ratio', limits=c(.85,5))+
  #Give y-axis a meaningful label
  ylab('')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=1, color='black', linetype='dashed')+
  theme_minimal() + 
  theme(
    text = element_text(size = 37, face="bold"), 
    axis.text =  element_text(color='black', size = FONT_SIZE))
pa



data_plot = data.frame( Dataset = c("ABCD", "ABCD", "CHOP-ED", "CHOP-ED", "MCS", "MCS"),
                        Model = rep(c("Demographics", "Demographics + ERS"),3),
                        R2 = c(2.8, 16.9, 2.5, 18.5, 7.5, 35))


data_plot$Model = factor(data_plot$Model, levels = c( "Demographics + ERS", "Demographics"))
data_plot$Dataset = factor( data_plot$Dataset , levels = c( "MCS", "CHOP-ED", "ABCD"))

pb = ggplot(data=data_plot, aes( x=R2, y=Dataset, fill=Model)) +
  geom_col( position=position_dodge()) +
  scale_fill_discrete(breaks=c("Demographics", "Demographics + ERS")) + 
  scale_x_continuous(labels = scales::label_percent(scale = 1)) + 
  theme_minimal() +
  ylab(element_blank())+
  xlab(expression(R^"2"))+
  theme(
    legend.position = c(.65, .93),
    text = element_text(size = 37, face="bold"),
    axis.text =  element_text(color='black', size = FONT_SIZE),
    legend.text = element_text(size = FONT_SIZE-10))+
  scale_fill_manual(
    breaks = c("Demographics", "Demographics + ERS"),
    values = c( "#8DC34A","#158EA7")
  )

pb

library(ggpubr)
p = ggarrange(pa,pb, labels = "AUTO" , font.label = list(size = FONT_SIZE +2))
p

ggsave(filename = "plots/last_figure_3.tiff", width = 24.5, height = 10.5, device='tiff', dpi=700)

