library(data.table)
library(ggplot2)
library(ggrepel)
library(stringr) 

dat = read.csv("~/Desktop/individual_level_results.csv")

setDT(dat)
dat[,ord := {
  fcase(
    Category == "Neighborhood", 1,
    Category == "Family", 2,
    Category == "Prenatal", 3,
    Category == "Peers", 4,
    Category == "Trauma", 5,
    Category == "School", 6,
    default = NA
  )
}]
dat = dat[order(P_value)]
dat = dat[order(ord)]
dat[, position := 1:.N]
dat[, center := mean(position), by = Category]
# rbind(dat, dat[, tail(.SD, 1) , by = Category][, c("Pvalue", "Feature_code") := .(0,  "not show")])[order(Category)]
# dat[, is_highlight := ifelse(Feature_code %in% c("separated_or_divorced_ever", "dim_matrix_q3__1"), "yes", "no")]
# dat[, is_annotate := {ifelse(-log10(P_value)>15, "yes", "no")}]

ylim = dat[,abs(floor(log10(min(P_value)))) + 2]
setDF(dat)

sig = c(0.05)
breaks = c(0,.05, .005, .0005, 5e-05, 5e-10, 5e-15, 5e-20, 5e-25)
# labels = c(0 = "0" , -log10(.05) = ".05", -log10(.005) = ".005", -log10(.0005) = ".0005",-log10(.00005) = "<.00005")
labels = c("0",".05", ".005", ".0005", "5e-05", "5e-10", "5e-15", "5e-20", "5e-25")
names(labels) = c(0, -log10(c(.05, .005, .0005, .00005)))

manhplot <- ggplot(dat, aes(x = position, y = -log10(fdr), color = as.factor(ord))) +
  # Show all points
  geom_point( size=1.5) +
  scale_color_manual(values = rep(c("#0C93CD", "#87BA23", "#C9AA2C","#EB945F", "#E04A68", "#98399A"), length(unique(dat$Category)))) +
  
  scale_x_continuous(labels = str_wrap(dat$Category, 10), breaks = dat$center) +
  scale_y_continuous(breaks = -log10(breaks), labels=labels) + #limits = c(0, ylim),
  # Add highlighted points
  # geom_point(data=subset(don, is_highlight=="yes"), color="orange", size=2) +
  
  # Add label using ggrepel to avoid overlapping
  # geom_label_repel( data=subset(dat, is_annotate=="yes"), aes(label=Feature_code), size=4) +
  geom_label_repel( data=subset(dat, !is.na(description)), aes(label=description), size=8,
                    # box.padding = unit(0, 'lines'),
                    # label.padding = unit(.65, "lines"),
                    # point.padding = unit(0, 'lines'),
                    min.segment.length = unit(0, "lines"),
                    force = 3,
                    force_pull = 5,#unit(0, 'lines'),
                    # arrow = arrow(length = unit(0.25, 'cm'), type = 'closed')
                    max.overlaps = Inf
                    ) +
  
  # add p value horizontal line 
  geom_hline(yintercept = -log10(sig), color = "black", linetype = "dashed") + 
  
  labs(x = NULL, 
       y = "P-value threshold") + 
  theme_minimal() +
  theme( 
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 25, face = "bold"),
    axis.text.y = element_text(size = 25, face = "bold"),
    axis.title.y = element_text(size = 30, face = "bold")
  )


manhplot

