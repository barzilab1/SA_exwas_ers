library(lme4)
library(readr)
library(ggplot2)

dataset <- read.csv("data/dataset_ERS.csv") 
covs = c("interview_age", "(interview_age)^2","(interview_age)^3","sex", "race_black", "race_white" , "ethnicity_hisp" )
random_exp = "(1 | site_id_l_br/rel_family_id/src_subject_id)"

quantiles_ers_control =  quantile(dataset$ers_z[dataset$SA_y == 0], seq(0,1,0.2))
dataset$q_group = findInterval(dataset$ers_z, quantiles_ers_control, rightmost.closed = T)
dataset$q_group[dataset$q_group ==6] = 5
dataset$q_group = factor(dataset$q_group, levels = c(3,1,2,4,5))

modq1 = glmer(paste0("SA_y ~ q_group + ", paste(covs, collapse = " + ") , " + ", random_exp), family = binomial, data = dataset, nAGQ = 0)


tab_model(modq1,
          show.intercept = F, show.ngroups = T, show.aic = T, show.r2 = T)


dataset_plot = data.frame(
  Quintiles = seq(1:5),
  OR = c(0.06,0.45, 1, 1.14, 4.67),
  ci_lower = c(0.01,0.20,NA,0.63,2.79),
  ci_upper = c(0.43,1.01,NA,2.09,7.81)
)


ggplot(dataset_plot, aes(x=Quintiles, y=OR)) +
  geom_point(shape=95, size = 7)+
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.025) +
  scale_y_continuous( name = "Odds Ratio",
                      limits = c(0, NA),
                      breaks = seq(0,8.2,1),
                      labels = c(0,"",2,"",4,"",6,"",8),
                      sec.axis = sec_axis(trans = ~.*1,
                                          breaks = seq(0,8.2,1),
                                          labels = c(0,"",2,"",4,"",6,"",8)),
                      expand = expansion(mult = c(0, 0.1))
  )+ 
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     text = element_text(size = FONT_SIZE, face="bold"),
                     axis.text = element_text(color='black',size = FONT_SIZE-5, face="bold")
                     )

ggsave(filename ="plots/Quintiles.tiff", dpi=700)



