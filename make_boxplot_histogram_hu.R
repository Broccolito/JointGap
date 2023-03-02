library(dplyr)
library(data.table)
library(purrr)
library(readxl)
library(writexl)

library(ggplot2)
library(ggpubr)
library(ggExtra)

hu = read_excel(path = "Master.xlsx", sheet = "hu")

hu$condition[hu$condition == "control"] = "Control"

condition_vector = hu$condition
sample_name_vector = hu$sample_name

hu = hu %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(condition = condition_vector) %>%
  mutate(sample_name = sample_name_vector) %>%
  select(sample_name, condition, everything())

make_plot = function(plot_xname, plot_name){
  
  plot_xlab = paste0(plot_name, " [HU]")
  
  plt1 = ggplot(data = hu, aes(x = .data[[plot_xname]], color = condition)) + 
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
  
  plt2 = ggplot(data = hu, aes(x = condition, y = .data[[plot_xname]])) + 
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


make_plot("talonavicular_talus_average", "TN-Talus Side Bone Intensity")
make_plot("talonavicular_talus_min", "TN-Talus Side Bone Intensity Minimum")
make_plot("talonavicular_talus_max", "TN-Talus Side Bone Intensity Maximum")
make_plot("talonavicular_talus_median", "TN-Talus Side Bone Intensity Median")
make_plot("talonavicular_talus_sd", "TN-Talus Side Bone Intensity Std")

make_plot("talonavicular_navicular_average", "TN-Navicular Side Bone Intensity")
make_plot("talonavicular_navicular_min", "TN-Navicular Side Bone Intensity Minimum")
make_plot("talonavicular_navicular_max", "TN-Navicular Side Bone Intensity Maximum")
make_plot("talonavicular_navicular_median", "TN-Navicular Side Bone Intensity Median")
make_plot("talonavicular_navicular_sd", "TN-Navicular Side Bone Intensity Std")

make_plot("navicular_c1_navicular_average", "NC-medial-Navicular Side Bone Intensity")
make_plot("navicular_c1_navicular_min", "NC-medial-Navicular Side Bone Intensity Minimum")
make_plot("navicular_c1_navicular_max", "NC-medial-Navicular Side Bone Intensity Maximum")
make_plot("navicular_c1_navicular_median", "NC-medial-Navicular Side Bone Intensity Median")
make_plot("navicular_c1_navicular_sd", "NC-medial-Navicular Side Bone Intensity Std")

make_plot("navicular_c1_medial_cuneiform_average", "NC-medial-Cuneiform Side Bone Intensity")
make_plot("navicular_c1_medial_cuneiform_min", "NC-medial-Cuneiform Side Bone Intensity Minimum")
make_plot("navicular_c1_medial_cuneiform_max", "NC-medial-Cuneiform Side Bone Intensity Maximum")
make_plot("navicular_c1_medial_cuneiform_median", "NC-medial-Cuneiform Side Bone Intensity Median")
make_plot("navicular_c1_medial_cuneiform_sd", "NC-medial-Cuneiform Side Bone Intensity Std")

make_plot("navicular_c2_navicular_average", "NC-middle-Navicular Side Bone Intensity")
make_plot("navicular_c2_navicular_min", "NC-middle-Navicular Side Bone Intensity Minimum")
make_plot("navicular_c2_navicular_max", "NC-middle-Navicular Side Bone Intensity Maximum")
make_plot("navicular_c2_navicular_median", "NC-middle-Navicular Side Bone Intensity Median")
make_plot("navicular_c2_navicular_sd", "NC-middle-Navicular Side Bone Intensity Std")

make_plot("navicular_c2_intermediate_cuneiform_average", "NC-middle-Cuneiform Side Bone Intensity")
make_plot("navicular_c2_intermediate_cuneiform_min", "NC-middle-Cuneiform Side Bone Intensity Minimum")
make_plot("navicular_c2_intermediate_cuneiform_max", "NC-middle-Cuneiform Side Bone Intensity Maximum")
make_plot("navicular_c2_intermediate_cuneiform_median", "NC-middle-Cuneiform Side Bone Intensity Median")
make_plot("navicular_c2_intermediate_cuneiform_sd", "NC-middle-Cuneiform Side Bone Intensity Std")

make_plot("navicular_c3_navicular_average", "NC-lateral-Navicular side Bone Intensity")
make_plot("navicular_c3_navicular_min", "NC-lateral-Navicular side Bone Intensity Minimum")
make_plot("navicular_c3_navicular_max", "NC-lateral-Navicular side Bone Intensity Maximum")
make_plot("navicular_c3_navicular_median", "NC-lateral-Navicular side Bone Intensity Median")
make_plot("navicular_c3_navicular_sd", "NC-lateral-Navicular side Bone Intensity Std")

make_plot("navicular_c3_lateral_cuneiform_average", "NC-lateral-Cuneiform Side Bone Intensity")
make_plot("navicular_c3_lateral_cuneiform_min", "NC-lateral-Cuneiform Side Bone Intensity Minimum")
make_plot("navicular_c3_lateral_cuneiform_max", "NC-lateral-Cuneiform Side Bone Intensity Maximum")
make_plot("navicular_c3_lateral_cuneiform_median", "NC-lateral-Cuneiform Side Bone Intensity Median")
make_plot("navicular_c3_lateral_cuneiform_sd", "NC-lateral-Cuneiform Side Bone Intensity Std")

make_plot("calcaneocuboid_calcaneus_average", "CC-Calcaneus Side Bone Intensity")
make_plot("calcaneocuboid_calcaneus_min", "CC-Calcaneus Side Bone Intensity Minimum")
make_plot("calcaneocuboid_calcaneus_max", "CC-Calcaneus Side Bone Intensity Maximum")
make_plot("calcaneocuboid_calcaneus_median", "CC-Calcaneus Side Bone Intensity Median")
make_plot("calcaneocuboid_calcaneus_sd", "CC-Calcaneus Side Bone Intensity Std")

make_plot("calcaneocuboid_cuboid_average", "CC-Cuboid Side Bone Intensity")
make_plot("calcaneocuboid_cuboid_min", "CC-Cuboid Side Bone Intensity Minimum")
make_plot("calcaneocuboid_cuboid_max", "CC-Cuboid Side Bone Intensity Maximum")
make_plot("calcaneocuboid_cuboid_median", "CC-Cuboid Side Bone Intensity Median")
make_plot("calcaneocuboid_cuboid_sd", "CC-Cuboid Side Bone Intensity Std")


