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

plt1 = ggplot(data = gap, aes(x = talonavicular_min, talonavicular_max)) + 
  geom_point(aes(fill = condition, color = condition), 
             alpha = 0.5, shape = 21, size = 3) +
  geom_smooth(aes(fill = condition, color = condition), method = "lm", alpha = 0.2) + 
  scale_color_manual(values = c("#7CAE00","#F8766D")) + 
  scale_fill_manual(values = c("#7CAE00","#F8766D")) +
  labs(fill = "", color = "") + 
  xlab("Minimum Talonavicular Gap (mm)") + 
  ylab("Maximum Talonavicular Gap (mm)") +
  theme_minimal() + 
  theme(text = element_text(size = 15), legend.position = "bottom")

plt2 = ggplot(data = gap, aes(x = navicular_c1_min, navicular_c1_max)) + 
  geom_point(aes(fill = condition, color = condition), 
             alpha = 0.5, shape = 21, size = 3) +
  geom_smooth(aes(fill = condition, color = condition), method = "lm", alpha = 0.2) + 
  scale_color_manual(values = c("#7CAE00","#F8766D")) + 
  scale_fill_manual(values = c("#7CAE00","#F8766D")) +
  labs(fill = "", color = "") + 
  xlab("Minimum Navicular C1 Gap (mm)") + 
  ylab("Maximum Navicular C1 Gap (mm)") +
  theme_minimal() + 
  theme(text = element_text(size = 15), legend.position = "bottom")

plt3 = ggplot(data = gap, aes(x = navicular_c2_min, navicular_c2_max)) + 
  geom_point(aes(fill = condition, color = condition), 
             alpha = 0.5, shape = 21, size = 3) +
  geom_smooth(aes(fill = condition, color = condition), method = "lm", alpha = 0.2) + 
  scale_color_manual(values = c("#7CAE00","#F8766D")) + 
  scale_fill_manual(values = c("#7CAE00","#F8766D")) +
  labs(fill = "", color = "") + 
  xlab("Minimum Navicular C2 Gap (mm)") + 
  ylab("Maximum Navicular C2 Gap (mm)") +
  theme_minimal() + 
  theme(text = element_text(size = 15), legend.position = "bottom")

plt4 = ggplot(data = gap, aes(x = navicular_c3_min, navicular_c3_max)) + 
  geom_point(aes(fill = condition, color = condition), 
             alpha = 0.5, shape = 21, size = 3) +
  geom_smooth(aes(fill = condition, color = condition), method = "lm", alpha = 0.2) + 
  scale_color_manual(values = c("#7CAE00","#F8766D")) + 
  scale_fill_manual(values = c("#7CAE00","#F8766D")) +
  labs(fill = "", color = "") + 
  xlab("Minimum Navicular C3 Gap (mm)") + 
  ylab("Maximum Navicular C3 Gap (mm)") +
  theme_minimal() + 
  theme(text = element_text(size = 15), legend.position = "bottom")

plt5 = ggplot(data = gap, aes(x = calcaneocuboid_min, calcaneocuboid_max)) + 
  geom_point(aes(fill = condition, color = condition), 
             alpha = 0.5, shape = 21, size = 3) +
  geom_smooth(aes(fill = condition, color = condition), method = "lm", alpha = 0.2) + 
  scale_color_manual(values = c("#7CAE00","#F8766D")) + 
  scale_fill_manual(values = c("#7CAE00","#F8766D")) +
  labs(fill = "", color = "") + 
  xlab("Minimum Calcaneocuboid Gap (mm)") + 
  ylab("Maximum Calcaneocuboid Gap (mm)") +
  theme_minimal() + 
  theme(text = element_text(size = 15), legend.position = "bottom")

plt1 = ggMarginal(plt1, groupColour = TRUE, groupFill = TRUE)
plt2 = ggMarginal(plt2, groupColour = TRUE, groupFill = TRUE)
plt3 = ggMarginal(plt3, groupColour = TRUE, groupFill = TRUE)
plt4 = ggMarginal(plt4, groupColour = TRUE, groupFill = TRUE)
plt5 = ggMarginal(plt5, groupColour = TRUE, groupFill = TRUE)

ggsave(filename = "talonavicular_min_max.png", plot = plt1, dpi = 1200, width = 6, height = 6)
ggsave(filename = "navicular_c1_min_max.png", plot = plt2, dpi = 1200, width = 6, height = 6)
ggsave(filename = "navicular_c2_min_max.png", plot = plt3, dpi = 1200, width = 6, height = 6)
ggsave(filename = "navicular_c3_min_max.png", plot = plt4, dpi = 1200, width = 6, height = 6)
ggsave(filename = "calcaneocuboid_min_max.png", plot = plt5, dpi = 1200, width = 6, height = 6)

# Statistics
lm(data = filter(gap, condition == "Control"),
   formula = talonavicular_max ~ talonavicular_min) %>% summary()

lm(data = filter(gap, condition == "MWD"),
   formula = talonavicular_max ~ talonavicular_min) %>% summary()


lm(data = filter(gap, condition == "Control"),
   formula = navicular_c1_max ~ navicular_c1_min) %>% summary()

lm(data = filter(gap, condition == "MWD"),
   formula = navicular_c1_max ~ navicular_c1_min) %>% summary()


lm(data = filter(gap, condition == "Control"),
   formula = navicular_c2_max ~ navicular_c2_min) %>% summary()

lm(data = filter(gap, condition == "MWD"),
   formula = navicular_c2_max ~ navicular_c2_min) %>% summary()


lm(data = filter(gap, condition == "Control"),
   formula = navicular_c3_max ~ navicular_c3_min) %>% summary()

lm(data = filter(gap, condition == "MWD"),
   formula = navicular_c3_max ~ navicular_c3_min) %>% summary()


lm(data = filter(gap, condition == "Control"),
   formula = calcaneocuboid_max ~ calcaneocuboid_min) %>% summary()

lm(data = filter(gap, condition == "MWD"),
   formula = calcaneocuboid_max ~ calcaneocuboid_min) %>% summary()


