library(ggplot2)

or_plot  = data.frame(Dataset = c("ABCD", "CHOP-ED", "MCS"),
                      or = c(2.19, 2, 3.4),
                      lowerci = c(1.91,1.91,2.46),
                      upperci = c(2.5,2.2,4.71)
                      )


pa = ggplot(or_plot, aes(y=Dataset, x=or, xmin=lowerci, xmax=upperci))+ #, shape = tester
  #Add data points and color them black
  geom_point(color = 'black')+
  #Add 'special' points for the summary estimates, by making them diamond shaped
  # geom_point(data=subset(dat, tester=='Summary'), color='black', shape=18, size=4)+
  #add the CI error bars
  geom_errorbarh(height=.1)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous( name='Odds Ratio', limits=c(.85,5))+ 
  #Give y-axis a meaningful label
  ylab('')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=1, color='black', linetype='dashed')+
  theme_minimal() 
pa



data_plot = data.frame( Dataset = c("ABCD", "ABCD", "CHOP-ED", "CHOP-ED", "MCS", "MCS"),
                        Model = rep(c("Demographics", "Demographics + ERS"),3),
                        R2 = c(2.8, 16.9, 3, 12, 7.5, 35))


data_plot$Model = factor(data_plot$Model, levels = c( "Demographics + ERS", "Demographics"))

pb = ggplot(data=data_plot, aes( x=R2, y=Dataset, fill=Model)) +
  geom_col( position=position_dodge()) +
  scale_fill_discrete(breaks=c("Demographics", "Demographics + ERS")) + 
  scale_x_continuous(labels = scales::label_percent(scale = 1)) + 
  theme_minimal() +
  ylab('')+
  theme(legend.position = c(.85, .95))
pb


library(ggpubr)
p = ggarrange(pa,pb,labels = "AUTO" )
p


