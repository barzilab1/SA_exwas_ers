library(data.table)
library(readr)
library(ggplot2)
library(ggstatsplot)
library(ggpubr)
library(stringr)
library(readxl)
library(ggrepel)

source("config.R")



tagged_data = read_excel(file.path(project_path, "ExWAS ABCD Dictionary.xlsx"))

exwas_results <- read_csv("outputs/exwas_results.csv")
exwas_results$variable = sub("_(b|z)$", "", exwas_results$variable)
#after removing _z there is also _br
exwas_results$variable = sub("_br$", "", exwas_results$variable)

dataset_descriptive = merge(exwas_results[, c("variable", "fdr", "or", "lowerci", "upperci")], 
                            tagged_data[,c("var_name", "Category", "table_name_nda", "var_label")], 
                            by.x = "variable", by.y = "var_name", all.x = T)

setDT(dataset_descriptive)
dataset_descriptive = dataset_descriptive[order(or, decreasing = T),]
dataset_descriptive[fdr < 0.05 , forest_number := 1:.N ]


# sup table: all exwas features 
write.csv(dataset_descriptive, "outputs/abcd_exwas_features_supT1.csv", na = "", row.names = F)


TEXT_SIZE = 30

#############################
##### 1. Manhattan plot #####
#############################
manhattan_db = dataset_descriptive
setDT(manhattan_db)[,ord := {
  fcase(
    Category == "Neighborhood", 7,
    Category == "Family", 6,
    Category == "Prenatal", 1,
    Category == "School\\Peers",5 ,
    Category == "Trauma", 2,
    Category == "Substance", 3,
    Category == "Lifestyle", 4
  )
}]
manhattan_db = manhattan_db[order(fdr)]
manhattan_db = manhattan_db[order(ord)]
manhattan_db[, position := 1:.N]
manhattan_db[, center := mean(position), by = Category]

breaks = c(0,.05, .005, .0005, 5e-05, 5e-07 ,5e-10, 5e-13, 5e-15, 5e-20)
labels = c("0",".05", ".005", ".0005", "5e-05","5e-07", "5e-10","5e-13", "5e-15", "5e-20")
names(labels) = c(0, -log10(c(.05, .005, .0005, 5e-05, 5e-07, 5e-10,5e-13, 5e-15, 5e-20)))


manh_plot <- ggplot(manhattan_db, aes(x = position, y = -log10(fdr), color = as.factor(Category))) +
  # Show all points
  geom_point( size=1.5) +
  scale_color_manual(values = c("#0C93CD", "#87BA23", "#C9AA2C","#EB945F", "#E04A68", "#98399A", "#FF5733"))+
  scale_x_continuous(labels = str_wrap(manhattan_db$Category, 10), breaks = manhattan_db$center) +
  scale_y_continuous(breaks = -log10(breaks), labels=labels) + 
  
  # add p value horizontal line 
  geom_hline(yintercept = -log10(0.05), color = "black", linetype = "dashed") + 
  
  # add the labels
  geom_label_repel( data=subset(manhattan_db, forest_number %in% c(1:5,95:99)), aes(label= forest_number), size = TEXT_SIZE/2,
                    min.segment.length = unit(0, "lines"),
                    force = 5
                    # nudge_x = dat$nudge_x[!is.na(dat$description)],
                    # nudge_y = dat$nudge_y[!is.na(dat$description)]
  )+
  
  labs(x = element_blank(), 
       y = "P-value Threshold") + 
  theme_minimal() +
  theme( 
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = TEXT_SIZE, face = "bold", color = "black"),
    axis.text.y = element_text(size = TEXT_SIZE, face = "bold", color = "black"),
    axis.title.y = element_text(size = TEXT_SIZE, face = "bold", color = "black")
  )  

manh_plot


##########################
##### 2. forest plot #####
##########################
forest_db = dataset_descriptive[fdr< 0.05,]
forest_db[, feature := as.factor(forest_number) ]
forest_db[, feature := reorder(feature,or)]
forest_db[, lowerci := as.numeric(lowerci)]
forest_db[, upperci := as.numeric(upperci)]

forest_db = forest_db[order(or, decreasing = T),]

#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
forest_plot=ggplot(forest_db, aes(y=feature, x=or, xmin=lowerci, xmax=upperci))+ #, shape = tester
  #Add data points and color them black
  geom_point(color = 'black', size = 2)+
  #add the CI error bars
  geom_errorbarh(height=.1, linewidth =1)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous( name='Odds Ratio', limits=c(-0.25,16.5), breaks = c(0,1,seq(5,16.5, 5)),
                      expand = c(0,0))+ 
  #Give y-axis a meaningful label
  ylab(" ")+ # keep space for the combined plot
  #Add a vertical dashed line indicating an effect size of 1, for reference
  geom_vline(xintercept=1, color='black', linetype='dashed')+
  #Create sub-plots (i.e., facets) based on levels of setting
  #And allow them to have their own unique axes (so authors don't redundantly repeat)
  # facet_grid(Category~., scales= 'free', space='free') + 
  #Apply my APA theme
  theme_minimal() + #theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        axis.text.x = element_text(size = TEXT_SIZE, face = "bold", color = "black"),
        axis.text.y = element_text(size = TEXT_SIZE/1.5, face = "bold", color = "black"),
        text = element_text(color='black',size  = TEXT_SIZE, face="bold")) 
  

        
forest_plot


#############################
##### 3. ERS sub groups #####
#############################
sub_group_db = read_csv("data/dataset_ERS.csv") 
setDT(sub_group_db)
sub_group_db = sub_group_db[eventname == "2_year_follow_up_y_arm_1",]

sub_group_db[, LGBT_inclusive_ch := {
  fcase(
    LGBT_inclusive == 1, "LGBT",
    LGBT_inclusive == 0, "Non LGBT"
  )
}]

y_text = "Standardized Exposome Sum Score"

# LGBT_inclusive_ch
# kruskal.test( ers_z~LGBT_inclusive_ch,  sub_group_db )
# wilcox.test( ers_z~LGBT_inclusive_ch,  sub_group_db )
t.test( ers_z~LGBT_inclusive_ch,  sub_group_db )

LGBT_plot = ggbetweenstats(
  data = sub_group_db,
  x = LGBT_inclusive_ch,
  y = ers_z,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  p.adjust.method = "fdr",
  pairwise.comparisons = TRUE,
  centrality.plotting = FALSE,
  xlab = "",
  ylab = y_text,
  title = "LGBT") +
  geom_signif(
    comparisons = list(c("LGBT", "Non LGBT")),
    textsize = TEXT_SIZE/2,
    size = 1,
    y_position = c(9.5),
    annotation = c("***"),
    vjust = 0.5
  )+
  scale_y_continuous(breaks = c(0,4,8,12)) +
  scale_color_manual(values = c("#158EA7", "#844D88","#8DC34A"))+ 
  theme_bw()+
  theme(
    plot.title = element_text(size = TEXT_SIZE, face = "bold"),
    legend.position='none',
    plot.subtitle = element_blank(),
    panel.background = element_blank(), #transparent panel bg
    plot.background = element_blank(), #transparent plot bg
    text = element_text(size = TEXT_SIZE, face="bold"),
    axis.text = element_text(color='black'))

LGBT_plot


# race
# kruskal.test( ers_z~race_eth,  sub_group_db)
# sub_group_db[,pairwise.wilcox.test(ers_z ,race_eth, p.adj='fdr')]

summary(aov(ers_z~race_eth,  sub_group_db))
TukeyHSD(aov(ers_z~race_eth,  sub_group_db), p.adjust.methods = 'fdr')


race_plot = ggbetweenstats(
  data = sub_group_db,
  x = race_eth,
  y = ers_z,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  p.adjust.method = "fdr",
  pairwise.comparisons = TRUE,
  pairwise.display = "none",
  centrality.plotting = FALSE,
  xlab = "",
  ylab = y_text,
  title = "Race")+
  geom_signif(
    comparisons = list(c("Hispanic", "NH-Black"),
                       c("Hispanic", "NH-White"),
                       c("NH-White", "NH-Black")),
    textsize = TEXT_SIZE/2,
    size = 1,
    y_position = c(9.5, 10.3, 11.5),
    annotation = c("***","***","***"),
    vjust = 0.5
  )+
  scale_color_manual(values = c("#158EA7", "#844D88","#8DC34A"))+
  theme_bw()+
  theme(
    plot.title = element_text(size = TEXT_SIZE, face = "bold"),
    legend.position='none',
    plot.subtitle = element_blank(),
    panel.background = element_blank(), #transparent panel bg
    plot.background = element_blank(), #transparent plot bg
    text = element_text(size = TEXT_SIZE, face="bold"),
    axis.text = element_text(color='black'))
race_plot

#############################
##### 4. organize plot #####
#############################

sub_groups_plot = ggarrange( race_plot, LGBT_plot, ncol = 1, 
                            labels = c("C", "D"), font.label = list(size = TEXT_SIZE, color = "black"))

bottom = ggarrange(forest_plot, sub_groups_plot, nrow = 1, ncol = 2 )

p = ggarrange(manh_plot, bottom, ncol = 1, nrow = 2, heights = c(0.75,2), 
              labels = "AUTO" , font.label = list(size = TEXT_SIZE, color = "black"))


ggsave(filename ="plots/combined_figure_2.tiff", p, width = 30, height = 35, device='tiff')
ggsave(filename ="plots/2b.tiff", forest_plot, width = 15, height = 20, device='tiff', dpi = 300)
ggsave(filename ="plots/2a.tiff", manh_plot, width = 30, height = 12, device='tiff', dpi = 300)
ggsave(filename ="plots/2cd.tiff", sub_groups_plot, width = 15, height = 20, device='tiff', dpi = 300)






