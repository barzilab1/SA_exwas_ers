library(ggplot2)
library(readr)
library(lme4)
library(sjPlot)
library(ggpubr)

#clolors = c("#459DD6", "#844D88","#158EA7", "#8DC34A", "#F0CA42")

FONT_SIZE =  40


#####################
#### A. or plot #####
#####################
or_plot  = data.frame(Dataset = c("ABCD", "CHOP-ED", "MCS"),
                      or = c(2.24, 2.28, 2.38),
                      lowerci = c(1.95,2.16,2.13),
                      upperci = c(2.56,2.42,2.65)
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
  scale_x_continuous( name='Odds Ratio')+
  #Give y-axis a meaningful label
  ylab(element_blank())+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=1, color='black', linetype='dashed')+
  theme_minimal() + 
  theme(
    text = element_text(size = 37, face="bold"), 
    axis.text =  element_text(color='black', size = FONT_SIZE))
pa



#####################
#### B. R2 plot #####
#####################
data_plot = data.frame( Dataset = c("ABCD", "ABCD", "CHOP-ED", "CHOP-ED", "MCS", "MCS"),
                        Model = rep(c("Demographics", "Demographics + ERS"),3),
                        R2 = c(2.8, 17.6, 2.5, 19.3, 5.6, 22.6))


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
    legend.position = c(.65, .92),
    text = element_text(size = 37, face="bold"),
    axis.text =  element_text(color='black', size = FONT_SIZE, face="bold"),
    legend.text = element_text(size = FONT_SIZE-10))+
  scale_fill_manual(
    breaks = c("Demographics", "Demographics + ERS"),
    values = c( "#8DC34A","#158EA7")
  )
pb


#######################
#### C. Quintiles #####
#######################
dataset <- read.csv("data/dataset_ERS.csv") 
covs = c("interview_age", "(interview_age)^2","(interview_age)^3","sex", "race_black", "race_white" , "ethnicity_hisp" )
random_exp = "(1 | site_id_l_br/rel_family_id/src_subject_id)"

dataset$q_group = dplyr::ntile(dataset$ers_z, 5)
dataset$q_group = factor(dataset$q_group, levels = c(3,1,2,4,5))

modq1 = glmer(paste0("SA_y ~ q_group + ", paste(covs, collapse = " + ") , " + ", random_exp), family = binomial, data = dataset, nAGQ = 0)
tab_model(modq1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T)

tableone::CreateTableOne(var = "q_group", factorVars = "q_group", strata = "SA_y", data = dataset, addOverall = T)



dataset_plot_abcd = data.frame(
  Quintiles = seq(1:5)-0.15,
  OR = c(0.05,0.32, 1, 1.30, 4.33),
  ci_lower = c(0.01,0.13,NA,0.73,2.60),
  ci_upper = c(0.4,0.77,NA,2.31,7.2),
  Dataset = rep("ABCD",5)
)

dataset_plot_chop = data.frame(
  Quintiles = seq(1:5),
  OR = c(0.46,0.44, 1, 1.98, 5.77),
  ci_lower = c(0.33,0.32,NA,1.58, 4.70),
  ci_upper = c(0.62,0.61,NA,2.49,7.12),
  Dataset = rep("CHOP-ED",5)
)

dataset_plot_MCS = data.frame(
  Quintiles = seq(1:5)+0.15,
  OR = c(0.24,0.55, 1, 1.54, 3.77),
  ci_lower = c(0.13,0.34,NA,1.05,2.68),
  ci_upper = c(0.44,0.88,NA,2.26,5.3),
  Dataset = rep("MCS",5)
)

dataset_plot = plyr::rbind.fill(dataset_plot_abcd, dataset_plot_chop, dataset_plot_MCS)


pc = ggplot(dataset_plot, aes(x=Quintiles, y=OR, group = Dataset, color = Dataset)) +
  geom_point(shape=95, size = 14)+
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0, linewidth = 1.5) +
  scale_y_continuous( name = "Odds Ratio",
                      limits = c(0, NA),
                      breaks = seq(0,8.2,1),
                      labels = c(0,"",2,"",4,"",6,"",8),
                      expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() + 
  theme(
    panel.border = element_blank(), 
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    text = element_text(size = FONT_SIZE, face="bold"),
    axis.text = element_text(color='black',size = FONT_SIZE-5, face="bold"),
    legend.position = c(.65, .87))+
  scale_color_manual(
    values = c("#EB945F", "#E04A68", "#98399A")
  )
pc


p = ggarrange(pa,pb,pc, 
              labels = "AUTO" , 
              ncol = 3,
              widths = c(1,1.2,1.1),
              font.label = list(size = FONT_SIZE +2))
p

# figure 3
ggsave(filename = "plots/combined_figure_3.tiff", width = 28, height = 10.5, device='tiff', dpi=300)

