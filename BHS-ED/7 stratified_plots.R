library(data.table)
library(readr)
library(ggplot2)
library(ggpubr)

setwd("~/sa_exwas")

dataset <- read_csv("data/df_testing_imputed.csv") 
setDT(dataset)

dataset[, TRANS_ch := {
  fcase(
    TRANS == 1, "Yes",
    TRANS == 0, "No"
  )
}]

FONT_SIZE = 25

theme_set(theme_minimal() + #scale_color_manual(values = c("#158EA7", "#844D88","#8DC34A"))+ 
            theme(
              plot.title = element_text(size = FONT_SIZE, face = "bold"),
              plot.subtitle = element_blank(),
              text = element_text(size = FONT_SIZE, face="bold"),
              axis.text = element_text(color='black',size = FONT_SIZE, face="bold")))
          

########################

dataset[, sex_sd := sd(ers_z, na.rm = T), by = sex]
dataset[, sex_se := sex_sd/sqrt(.N), by = sex]
dataset[, sex_mean := mean(ers_z), by = sex]
df_sex = unique(dataset[sex_abbr != "U", c("sex_abbr", "sex_se", "sex_mean")])
# kruskal.test(ers_z~sex,  dataset )
wilcox.test(ers_z~sex,  dataset )

p_sex <- ggplot(df_sex, aes(x = sex_abbr, y = sex_mean)) +
  xlab("Sex") + ylab("Standardized ERS Mean") + 
  geom_bar(stat = "identity", fill = c("#844D88","#158EA7"), width = .5)  +
  geom_errorbar( aes(x=sex_abbr, ymin=sex_mean-sex_se, ymax=sex_mean+sex_se), 
                  width=0.3, alpha=0.9, linewidth=1.5) + 
  
  geom_text(aes(x = 1.5, label = "**"), y = .052, size = 8) +
  geom_segment(aes(x = 1, xend = 2, y = .05, yend = .05), 
               lineend = "butt", linejoin = "mitre" , linewidth = 2) 
p_sex


########################

dataset[, trans_sd := sd(ers_z, na.rm = T), by = TRANS_ch]
dataset[, trans_se := trans_sd/sqrt(.N), by = TRANS_ch]
dataset[, trans_mean := mean(ers_z), by = TRANS_ch]
df_tr = unique(dataset[!is.na(TRANS_ch), c("TRANS_ch", "trans_se", "trans_mean")])
t.test( ers_z~TRANS_ch,  dataset )
kruskal.test( ers_z~TRANS_ch,  dataset )
wilcox.test(ers_z~TRANS_ch,  dataset )

p_trans <- ggplot(df_tr, aes(x = TRANS_ch, y = trans_mean)) +
  xlab("Transgender") + ylab(element_blank()) +
  geom_bar(stat = "identity", fill = c("#158EA7", "#844D88"), width = .5) +
  geom_errorbar( aes(x=TRANS_ch, ymin=trans_mean-trans_se, ymax=trans_mean+trans_se), 
                 width=0.3, alpha=0.9, linewidth=1.5) + 
  
  geom_text(aes(x = 1.5, label = "***"), y = .73, size = 8) +
  geom_segment(aes(x = 1, xend = 2, y = .71, yend = .71), 
               lineend = "butt", linejoin = "mitre" , linewidth = 2) 
p_trans


########################
dataset[, race_sd := sd(ers_z, na.rm = T), by = race_eth]
dataset[, race_se := race_sd/sqrt(.N), by = race_eth]
dataset[, race_mean := mean(ers_z), by = race_eth]
df_race = unique(dataset[!is.na(race_eth), c("race_eth", "race_se", "race_mean")])

kruskal.test( ers_z~race_eth,  dataset[race_eth != "NH-White"] )
kruskal.test( ers_z~race_eth,  dataset[race_eth != "NH-Black"] )
kruskal.test( ers_z~race_eth,  dataset[race_eth != "Hispanic"] )
dataset[,pairwise.wilcox.test(ers_z ,race_eth, p.adj='fdr')]
dataset[,pairwise.t.test(ers_z ,race_eth,p.adj='fdr')]

p_race <- ggplot(df_race, aes(x = race_eth, y = race_mean)) +
  xlab("Race") + ylab(element_blank()) +
  geom_bar(stat = "identity", fill = c('#8DC34A', "#158EA7", "#844D88" ), width = 0.5) +
  geom_errorbar( aes(x=race_eth, ymin=race_mean-race_se, ymax=race_mean+race_se), 
                 width=0.3, alpha=0.9, linewidth=1.5) + 
  
  geom_text(aes(x = 2, label = "***"), y = .092, size = 10) +
  geom_segment(aes(x = 1, xend = 3, y = .088, yend = .088), 
               lineend = "butt", linejoin = "mitre" , linewidth = 2) +

  geom_text(aes(x = 1.5, label = "***"), y = .072, size = 10) +
    geom_segment(aes(x = 1, xend = 2, y = .068, yend = .068), 
                 lineend = "butt", linejoin = "mitre" , linewidth = 2) +
    
  geom_text(aes(x = 2.5, label = "***"), y = .077, size = 10) +
  geom_segment(aes(x = 2, xend = 3, y = .073, yend = .073), 
               lineend = "butt", linejoin = "mitre" , linewidth = 2) 

p_race





p = ggarrange(p_sex,p_trans,p_race, nrow = 1 ,widths = c(.4,.4,.6) ) + 
  theme(panel.background = element_rect(fill = "white"),
       panel.grid = element_line(color = "black"))
p


ggsave(filename ="plots/differential_exposure.png", p, width = 18, height = 6, dpi=700)



t.test( ers_z~sex,  dataset )
wilcox.test(exwas_individual_sum_z~sex,  dataset) 
kruskal.test()
