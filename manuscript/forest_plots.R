# https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/

library(metafor)
library(ggplot2)
library(readxl)

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        # text=element_text(family='Times'),
        legend.position='none',
        strip.text.y.right = element_text(angle = 0))


dat = read_excel("~/Desktop/individual_level_results.xlsx")
dat = read_excel("~/Desktop/structural_level_results.xlsx")
# dat = read.csv("outputs/individual_level_results.csv")
dat$feature = as.factor(dat$Description)
dat$feature = reorder(dat$Description, dat$OR)
dat$lowerci = as.numeric(dat$Lowerci_or)
dat$upperci = as.numeric(dat$Upperci_or)
# dat$Category  = factor(dat$Category, levels=c("Risk Factors","Protective Factors"  ))


dat = dat[order(dat$OR, decreasing = T),]

#### individual ####

#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(dat[1:67,], aes(y=feature, x=OR, xmin=lowerci, xmax=upperci))+ #, shape = tester
  #Add data points and color them black
  geom_point(color = 'black')+
  #Add 'special' points for the summary estimates, by making them diamond shaped
  # geom_point(data=subset(dat, tester=='Summary'), color='black', shape=18, size=4)+
  #add the CI error bars
  geom_errorbarh(height=.1)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous( name='Odds Ratio', limits=c(0,17), breaks = c(0,1,seq(5,17, 5)))+ 
  #Give y-axis a meaningful label
  ylab('')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=1, color='black', linetype='dashed')+
  #Create sub-plots (i.e., facets) based on levels of setting
  #And allow them to have their own unique axes (so authors don't redundantly repeat)
  facet_grid(Category~., scales= 'free', space='free') + 
  #Apply my APA theme
  apatheme
p

#Save plot in your working directory
ggsave(p, file='plots/individual_risk.png', width = 10, height=10, dpi=300)




p=ggplot(dat[68:83,], aes(y=feature, x=OR, xmin=lowerci, xmax=upperci))+ #, shape = tester
  #Add data points and color them black
  geom_point(color = 'black')+
  #Add 'special' points for the summary estimates, by making them diamond shaped
  # geom_point(data=subset(dat, tester=='Summary'), color='black', shape=18, size=4)+
  #add the CI error bars
  geom_errorbarh(height=.1)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous( name='Odds Ratio', limits=c(0,1.1), breaks = seq(0,1.1,.1))+ 
  #Give y-axis a meaningful label
  ylab('')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=1, color='black', linetype='dashed')+
  #Create sub-plots (i.e., facets) based on levels of setting
  #And allow them to have their own unique axes (so authors don't redundantly repeat)
  facet_grid(Category~., scales= 'free', space='free') + 
  #Apply my APA theme
  apatheme
p

#Save plot in your working directory
ggsave(p, file='plots/individual_protective.png', width = 10, height=10, dpi=300)



#### structural ####
dat$Category  = factor(dat$Category, levels=c("Social Vulnerability Index","Area Deprivation Index", 
                                              "The Child Opportunity Index", "The Opportunity Atlas"  ))

p=ggplot(dat, aes(y=feature, x=OR, xmin=lowerci, xmax=upperci))+ #, shape = tester
  #Add data points and color them black
  geom_point(color = 'black')+
  #Add 'special' points for the summary estimates, by making them diamond shaped
  # geom_point(data=subset(dat, tester=='Summary'), color='black', shape=18, size=4)+
  #add the CI error bars
  geom_errorbarh(height=.1)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous( name='Odds Ratio', limits=c(0,12.5), breaks = c(0,0.5,1,5,10))+ 
  #Give y-axis a meaningful label
  ylab('')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=1, color='black', linetype='dashed')+
  #Create sub-plots (i.e., facets) based on levels of setting
  #And allow them to have their own unique axes (so authors don't redundantly repeat)
  facet_grid(Category~., scales= 'free', space='free') + 
  #Apply my APA theme
  apatheme
p

#Save plot in your working directory
ggsave(p, file='plots/individual_risk.png', width = 10, height=10, dpi=300)



