# new plots 
library(ggstatsplot)
library(ggplot2)
library(ggpubr)


dataset <- read.csv("data/dataset_ESS.csv") 
setDT(dataset)

theme_set(theme_bw() )
# +
#             theme(axis.text.x = element_text(size = 13, face = "bold"),
#                   axis.text.y = element_text(size = 13, face = "bold"),
#                   axis.title.x = element_text(size = 15, face = "bold"),
#                   axis.title.y = element_text(size = 15, face = "bold"),
#                   # plot.subtitle = element_text(size = 10, hjust = 0,face = "bold"),
#                   # axis.ticks = element_line(size = 0.5),
#                   legend.text = element_text(size=13),
#                   legend.title = element_text(size=14, face = "bold")))


create_box_plot = function(stratify_variable, dataset){
  
  plots = list()
  
  for(timepoint in unique(dataset$time)){
    p = ggbetweenstats(
          data = dataset[time == timepoint,],
          x = !!stratify_variable,
          y = exwas_individual_sum_z,
          # grouping.var = time,
          type = "nonparametric", # ANOVA or Kruskal-Wallis
          p.adjust.method = "fdr",
          pairwise.comparisons = TRUE,
          pairwise.display = "significant",
          centrality.plotting = FALSE,
          xlab = timepoint,
          ylab = "",
          title = ""
        ) + theme(plot.subtitle = element_text(size = 10, hjust = -0.3,face = "bold"))
     plots[[timepoint]] = p 
  }
  
  return(plots)
}


p_sex = create_box_plot("sex", dataset)
p_lgbt = create_box_plot("LGBT_inclusive", dataset)
p_race = create_box_plot("race_eth", dataset)





p1 = ggarrange(p_sex[["baseline"]] + ylab("Standardized Exposome Sum Score")+ ggtitle("Sex"),  
                p_sex[["1 year follow up"]],
                p_sex[["2 year follow up"]],
                nrow = 1) 

p2 = ggarrange(p_lgbt[["baseline"]] + ylab("Standardized Exposome Sum Score")+ ggtitle("LGBT+"),  
               p_lgbt[["1 year follow up"]],
               p_lgbt[["2 year follow up"]], 
               nrow = 1
               ) 

p3 = ggarrange(p_race[["baseline"]] + ylab("Standardized Exposome Sum Score")+ ggtitle("Race"),  
               p_race[["1 year follow up"]],
               p_race[["2 year follow up"]], 
               nrow = 1
) 

p = ggarrange(p1,p2,p3, nrow = 3, labels = "AUTO" )
p
ggsave(filename ="plots/differential_exposure.png", p, width = 20, height = 15, dpi=700)


t.test( exwas_individual_sum_z~sex,  dataset, [time == "2 year follow up",] )
wilcox.test(exwas_individual_sum_z~sex,  dataset[time == "2 year follow up",]) 
