library(data.table)
library(readr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(readxl)

source("config.R")


tagged_data = read_excel(file.path(project_path, "ExWAS ABCD Dictionary.xlsx"))

exwas_results <- read_csv("outputs/exwas_results.csv")
exwas_results$variable = gsub("_(b|z)$", "", exwas_results$variable)
#after removing _z there is also _br
exwas_results$variable = gsub("_br$", "", exwas_results$variable)

dataset_descriptive = merge(exwas_results[, c("variable", "fdr", "or", "lowerci", "upperci")], 
                            tagged_data[,c("var_name", "Category", "table_name_nda", "var_label")], 
                            by.x = "variable", by.y = "var_name", all.x = T)

# sup table: all exwas features 
write.csv(dataset_descriptive, "outputs/abcd_exwas_features_supT1.csv", na = "", row.names = F)


# View(dat[is.na(dat$Category),])
dataset_descriptive$variable[is.na(dataset_descriptive$Category)]
dataset_descriptive$Category[is.na(dataset_descriptive$Category)] = "Lifestyle"

setDT(dataset_descriptive)
dataset_descriptive[,ord := {
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
dataset_descriptive = dataset_descriptive[order(fdr)]
dataset_descriptive = dataset_descriptive[order(ord)]
dataset_descriptive[, position := 1:.N]
dataset_descriptive[, center := mean(position), by = Category]
# rbind(dat, dat[, tail(.SD, 1) , by = Category][, c("Pvalue", "Feature_code") := .(0,  "not show")])[order(Category)]
# dat[, is_highlight := ifelse(Feature_code %in% c("separated_or_divorced_ever", "dim_matrix_q3__1"), "yes", "no")]
# dat[, is_annotate := {ifelse(-log10(P_value)>15, "yes", "no")}]

ylim = dataset_descriptive[,abs(floor(log10(min(fdr)))) + 2]
setDF(dataset_descriptive)


sig = c(0.05)
breaks = c(0,.05, .005, .0005, 5e-05, 5e-07 ,5e-10, 5e-13, 5e-15, 5e-20)
# labels = c(0 = "0" , -log10(.05) = ".05", -log10(.005) = ".005", -log10(.0005) = ".0005",-log10(.00005) = "<.00005")
labels = c("0",".05", ".005", ".0005", "5e-05","5e-07", "5e-10","5e-13", "5e-15", "5e-20")
names(labels) = c(0, -log10(c(.05, .005, .0005, 5e-05, 5e-07, 5e-10,5e-13, 5e-15, 5e-20)))


TEXT_SIZE = 30
manhplot <- ggplot(dataset_descriptive, aes(x = position, y = -log10(fdr), color = as.factor(ord))) +
  # Show all points
  geom_point( size=1.5) +
  scale_color_manual(values = c("#ff0000", "#ffa500", "#ffff00", "#008000", "#0000ff", "#4b0082", "#ee82ee"))+#rep(c("#0C93CD", "#87BA23", "#C9AA2C","#EB945F", "#E04A68", "#98399A"), 
                                  #length(unique(dataset_descriptive$Category)))) +
  
  scale_x_continuous(labels = str_wrap(dataset_descriptive$Category, 10), breaks = dataset_descriptive$center) +
  scale_y_continuous(breaks = -log10(breaks), labels=labels) + #limits = c(0, ylim),
  # Add highlighted points
  # geom_point(data=subset(don, is_highlight=="yes"), color="orange", size=2) +
  
  # Add label using ggrepel to avoid overlapping
  # geom_label_repel( data=subset(dat, is_annotate=="yes"), aes(label=Feature_code), size=4) +
  # geom_label_repel( data=subset(dat, !is.na(description)), aes(label=description), size=8,
  #                   # box.padding = unit(0, 'lines'),
  #                   # label.padding = unit(.65, "lines"),
  #                   # point.padding = unit(0, 'lines'),
  #                   min.segment.length = unit(0, "lines"),
  #                   force = 3,
  #                   force_pull = 5,#unit(0, 'lines'),
  #                   # arrow = arrow(length = unit(0.25, 'cm'), type = 'closed')
  #                   max.overlaps = Inf
  #                   ) +
  
  # add p value horizontal line 
  geom_hline(yintercept = -log10(sig), color = "black", linetype = "dashed") + 
  
  labs(x = NULL, 
       y = "P-value threshold") + 
  theme_minimal() +
  theme( 
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = TEXT_SIZE, face = "bold", color = "black"),
    axis.text.y = element_text(size = TEXT_SIZE, face = "bold", color = "black"),
    axis.title.y = element_text(size = TEXT_SIZE, face = "bold", color = "black")
  )


manhplot
ggsave(filename = "plots/manhplot.png", width = 26.5, height = 5,  dpi=300)

