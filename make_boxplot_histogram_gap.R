library(dplyr)
library(data.table)
library(purrr)
library(readxl)
library(writexl)

library(ggplot2)
library(ggpubr)
library(ggExtra)

gap = read_excel(path = "Master.xlsx", sheet = "gap")
gap$condition[gap$condition == "control"] = "Control"

make_plot = function(plot_xname, plot_name){
  
  plot_xlab = paste0(plot_name, " (mm)")
  
  plt1 = ggplot(data = gap, aes(x = .data[[plot_xname]], color = condition)) + 
    geom_histogram(aes(fill = condition), bins = 30,
                   alpha = 0.5, position = "identity") + 
    scale_color_manual(values = c("#7CAE00","#F8766D")) + 
    scale_fill_manual(values = c("#7CAE00","#F8766D")) +
    labs(fill = "", color = "") + 
    xlab(plot_xlab) + 
    ylab("Count") + 
    theme_pubclean() + 
    theme(text = element_text(size = 15))
  
  ggsave(filename = paste0(plot_name, " Histogram.png"), plot = plt1, device = "png",
         width = 5, height = 4, dpi = 1200)
  
  plt2 = ggplot(data = gap, aes(x = condition, y = .data[[plot_xname]])) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_point(aes(fill = condition, color = condition), 
               shape = 21, size = 3, alpha = 0.7,
               position = position_dodge2(0.2)) +
    scale_color_manual(values = c("#7CAE00","#F8766D")) + 
    scale_fill_manual(values = c("#7CAE00","#F8766D")) +
    stat_compare_means(method = "t.test", comparisons = list(c("Control", "MWD")),
                       symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf),
                                          symbols = c("****", "***", "**", "*", "NS"))) +
    # stat_compare_means(method = "t.test", comparisons = list(c("Control", "MWD"))) + 
    xlab("") + 
    ylab(plot_xlab) +
    theme_pubclean() + 
    theme(text = element_text(size = 15), legend.position = "none")
  plt2 = ggMarginal(plt2, groupColour = TRUE, groupFill = TRUE)
  
  ggsave(filename = paste0(plot_name, " Boxplot.png"), plot = plt2, device = "png",
         width = 5, height = 5, dpi = 1200)
  
}


make_plot("talonavicular_average", "Average Talonavicular Gap")
make_plot("talonavicular_min", "Minimum Talonavicular Gap")
make_plot("talonavicular_max", "Maximum Talonavicular Gap")
make_plot("talonavicular_median", "Median Talonavicular Gap")
make_plot("talonavicular_sd", "Talonavicular Gap Std")

make_plot("navicular_c1_average", "Average Navicular C1 Gap")
make_plot("navicular_c1_min", "Minimum Navicular C1 Gap")
make_plot("navicular_c1_max", "Maximum Navicular C1 Gap")
make_plot("navicular_c1_median", "Median Navicular C1 Gap")
make_plot("navicular_c1_sd", "Navicular C1 Gap Std")

make_plot("navicular_c2_average", "Average Navicular C2 Gap")
make_plot("navicular_c2_min", "Minimum Navicular C2 Gap")
make_plot("navicular_c2_max", "Maximum Navicular C2 Gap")
make_plot("navicular_c2_median", "Median Navicular C2 Gap")
make_plot("navicular_c2_sd", "Navicular C2 Gap Std")

make_plot("navicular_c3_average", "Average Navicular C3 Gap")
make_plot("navicular_c3_min", "Minimum Navicular C3 Gap")
make_plot("navicular_c3_max", "Maximum Navicular C3 Gap")
make_plot("navicular_c3_median", "Median Navicular C3 Gap")
make_plot("navicular_c3_sd", "Navicular C3 Gap Std")

make_plot("calcaneocuboid_average", "Average Calcaneocuboid Gap")
make_plot("calcaneocuboid_min", "Minimum Calcaneocuboid Gap")
make_plot("calcaneocuboid_max", "Maximum Calcaneocuboid Gap")
make_plot("calcaneocuboid_median", "Median Calcaneocuboid Gap")
make_plot("calcaneocuboid_sd", "Calcaneocuboid Gap Std")
