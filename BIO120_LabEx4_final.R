library(tidyverse)
library(ggpubr)
library(ggthemes)
library(ggh4x)
library(patchwork)
library(readxl)
setwd("/Users/an2n/Desktop/BS\ BIO\ III_2nd\ sem/BIO\ 122\ -\ Animal\ Physio/Lab\ -\ 122/122\ -\ LabEx\ 4")

# Effect of load, muslce size, & length on velocity------
wght.lift <- read_excel("Exercise 4_Skeletal Muscle Contraction.xlsx", 
                       sheet = "BLOCK B", range = "A28:F34") %>% 
  mutate(`Group No.` = factor(`Group No.`, levels = c("1", "2", "3", "4", "5", "6")))

wl.profile <- read_excel("Exercise 4_Skeletal Muscle Contraction.xlsx", 
                        sheet = "BLOCK B", range = "A39:E45") %>% 
  mutate(`Group No.` = factor(`Group No.`, levels = c("1", "2", "3", "4", "5", "6")))

load.long <- wght.lift %>% 
  gather(key = "weights", value = "flexion", -`Group No.`) 

load.summ <- wght.lift %>% 
  gather(key = "weights", value = "flexion", - `Group No.`) %>% 
  group_by(weights) %>% 
  summarize(mean = mean(flexion),
            sd = sd(flexion))

ggplot() +
  geom_col(data = load.summ, aes(weights, mean), 
           fill = "pink", color = "black", alpha = 0.5) +
  geom_point(data = load.long, aes(weights, flexion, color = `Group No.`), 
             size = 3) +
  geom_errorbar(data = load.summ, width = 0.3,
                aes(x = weights, ymin = mean - sd, ymax = mean + sd)) +
  coord_cartesian(ylim = c(30, NA)) +
  labs(x = "Weights (L)", y = "Muscle Contraction Velocity 
(lifts/min)", color = "Subject") +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(color = "black", size = 14))
  
# ggsave("rplot01.png", dpi = 300)

# Correlation ------
load <- bind_cols(wl.profile, wght.lift) %>% 
  select(-`Group No....6`) %>% 
  rename(Group = `Group No....1`) %>% 
  gather(key = "weights", value = "flexion", -Group, -Forearm_circ, -Forearm_len,
         -Upperarm_circ, -Upperarm_len) %>% 
  gather(key = "key", value = "mm", -Group, -weights, - flexion) %>% 
  separate(key, c("region", "measurement")) %>% 
  mutate(weights = paste0(weights, " L")) %>% 
  mutate(Group = factor(Group, levels = c("1", "2", "3", "4", "5", "6")))

len <- load %>% 
  filter(measurement == "len") %>% 
  ggplot(aes(mm, flexion)) +
  geom_point(aes(color = Group)) +
  geom_smooth(method = "lm", se = F, linewidth = 0.5) +
  facet_grid2(region ~ weights, scales = "free_x", independent = "x") +
  stat_cor(size = 3) +
  labs(title = "A. Length", 
       x = "Length (mm)", 
       y = "Muscle Contraction Velocity 
(lifts/min)",
       color = "Subject") +
  theme_base() +
  theme(strip.text = element_text(size = 15, face = "bold"),
        plot.background = element_rect(color = "white"))


write.csv(load, "load.csv", row.names=FALSE)

test <- load %>% filter(measurement == "circ" & region == "Forearm")

# ggsave("rplot02.png", dpi = 300)
  
circ <- load %>% 
  filter(measurement == "circ") %>% 
  ggplot(aes(mm, flexion)) +
  geom_point(aes(color = Group)) +
  geom_smooth(method = "lm", se = F, linewidth = 0.5) +
  facet_grid2(region ~ weights, scales = "free", independent = "x") +
  stat_cor(size = 3) +
  labs(title = "B. Circumference", 
       x = "Circumference (mm)", 
       y = "Muscle Contraction Velocity 
(lifts/min)",
       color = "Subject") +
  theme_base() +
  theme(strip.text = element_text(size = 15, face = "bold"),
        plot.background = element_rect(color = "white"))

# ggsave("rplot03.png", dpi = 300)

len / circ

# ggsave("rplot04.png", dpi = 300)

# Match 1 ------
subject1 <- read_excel("Exercise 4_Skeletal Muscle Contraction.xlsx", 
                        sheet = "BLOCK B", range = "A53:F71") %>% 
  select(-`Group No.`) %>% 
  gather(key = "key", value = "value", -`Trial No.`) %>% 
  separate(key, c("region", "measurement")) %>% 
  mutate(Subject = 1)

subject2 <- read_excel("Exercise 4_Skeletal Muscle Contraction.xlsx", 
                       sheet = "BLOCK B", range = "A82:F100") %>% 
  select(-`Group No.`) %>% 
  gather(key = "key", value = "value", -`Trial No.`) %>% 
  separate(key, c("region", "measurement")) %>% 
  mutate(Subject = 1)

profile.1 <- bind_rows(subject1, subject2)

profile.1.summ <- profile.1 %>% 
  group_by(`Trial No.`, region, measurement) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  mutate(time = "Before") %>% 
  as.data.frame()
  
match1 <- read_excel("Exercise 4_Skeletal Muscle Contraction.xlsx", 
                                 sheet = "BLOCK B", range = "A140:H152") %>% 
  select(-`Group No.`) %>% 
  gather(key = "key", value = "value", -`Trial No.`, -`Subject No.`) %>% 
  separate(key, c("region", "measurement"))

match1.summ <- match1 %>% 
  na.omit() %>% 
  group_by(`Trial No.`, region, measurement) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate(time = "During") %>% 
  as.data.frame()

base.dur.1 <- bind_rows(profile.1.summ, match1.summ) %>% 
  mutate(measurement = case_when(measurement == "circ" ~ "Circumference",
                                 measurement == "len" ~ "Length"),
         measurement = factor(measurement, levels = c("Length", "Circumference")))

sim1 <- base.dur.1 %>% 
  mutate(`Trial No.` = factor(`Trial No.`, levels = c("Winner", "Loser"))) %>% 
  ggplot(aes(time, mean, fill = `Trial No.`, 
             ymin = mean - sd, ymax = mean + sd)) +
  geom_col(position = position_dodge(), color = "black", width = 0.75) +
  geom_errorbar(width = 0.1, position = position_dodge(width = 0.75)) +
  facet_grid(measurement ~ region, scales = "free") +
  coord_cartesian(ylim = c(150, NA)) +
  labs(title = "A. Simulation 1", 
       x = NULL, y = "Avg. measurement (mm)", fill = NULL) +
  theme_base() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 13.3, color = "black"),
        title = element_text(size = 14),
        axis.text = element_text(color = "black"),
        strip.text = element_text(size = 14, color = "black"),
        plot.background = element_rect(color = "white"),
        legend.position = "bottom")

# ggsave("rplot05.png", dpi = 300)
  
# Match 2 -----
subject3 <- read_excel("Exercise 4_Skeletal Muscle Contraction.xlsx", 
                       sheet = "BLOCK B", range = "B218:F242") %>% 
  gather(key = "key", value = "value", -`Trial No.`) %>% 
  separate(key, c("region", "measurement")) %>% 
  mutate(time = "Before")

profile.2.summ <- subject3 %>% 
  group_by(time, `Trial No.`, region, measurement) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  as.data.frame()

match2 <- read_excel("Exercise 4_Skeletal Muscle Contraction.xlsx", 
                     sheet = "BLOCK B", range = "A181:H193") %>% 
  select(-`Group No.`) %>% 
  na.omit() %>% 
  gather(key = "key", value = "value", -`Trial No.`, -`Subject No.`) %>% 
  separate(key, c("region", "measurement")) %>% 
  mutate(time = "During")

match2.summ <- match2 %>% 
  na.omit() %>% 
  group_by(time, `Trial No.`, region, measurement) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  as.data.frame()

base.dur.2 <- bind_rows(profile.2.summ, match2.summ) %>% 
  mutate(measurement = case_when(measurement == "circ" ~ "Circumference",
                                 measurement == "len" ~ "Length"),
         measurement = factor(measurement, levels = c("Length", "Circumference")))

sim2 <- base.dur.2 %>% 
  mutate(`Trial No.` = factor(`Trial No.`, levels = c("Winner", "Loser"))) %>% 
  ggplot(aes(time, mean, fill = `Trial No.`, 
             ymin = mean - sd, ymax = mean + sd)) +
  geom_col(position = position_dodge(), color = "black", width = 0.75) +
  geom_errorbar(width = 0.1, position = position_dodge(width = 0.75)) +
  facet_grid(measurement ~ region, scales = "free") +
  coord_cartesian(ylim = c(150, NA)) +
  labs(title = "B. Simulation 2", x = NULL, y = "Avg. measurement (mm)", fill = NULL) +
  theme_base() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 13.3, color = "black"),
        title = element_text(size = 14),
        axis.text = element_text(color = "black"),
        strip.text = element_text(size = 14, color = "black"),
        plot.background = element_rect(color = "white"),
        legend.position = "bottom")

# ggsave("rplot06.png", dpi = 300)

sim1 + sim2

# ggsave("rplot07.png", dpi = 300)

# Arm angle ----
angle1 <- match1 %>% 
  filter(region == "Angle") %>% 
  select(-measurement, -`Subject No.`) %>% 
  group_by(`Trial No.`) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate(Simulation = "Simulation 1") %>% 
  as.data.frame()

angle2 <- match2 %>% 
  filter(region == "Angle") %>% 
  select(-measurement, -`Subject No.`) %>% 
  group_by(`Trial No.`) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate(Simulation = "Simulation 2") %>% 
  as.data.frame()

ang.flex <- bind_rows(angle1, angle2) %>% 
  mutate(`Trial No.` = factor(`Trial No.`, levels = c("Winner", "Loser")))

ang.flex %>% 
  ggplot(aes(`Trial No.`, mean, fill = `Trial No.`, ymin = mean - sd, ymax = mean + sd)) +
  geom_col(position = position_dodge(), color = "black") +
  geom_errorbar(width = 0.2) +
  facet_wrap(~Simulation) +
  scale_y_continuous(expand = expansion(), limits = c(0, 160)) +
  labs(x = NULL, y = "Avg. angle of flexion (°)") +
  theme_base() +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 14.5, color = "black"),
        strip.text = element_text(size = 15, color = "black", face = "bold"),
        plot.background = element_rect(color = "white"))

ggsave("rplot08.png", dpi = 300)

ang.flex %>% 
  ggplot(aes(Simulation, mean, fill = `Trial No.`, ymin = mean - sd, ymax = mean + sd)) +
  geom_col(position = position_dodge(), width = 0.75, color = "black") +
  geom_errorbar(width = 0.2, position = position_dodge(0.75)) +
  labs(x = NULL, y = "Avg. arm angle (°)", fill = NULL) +
  scale_y_continuous(expand = expansion(), limits = c(0, 160)) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.ticks.x = element_blank(),
        axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(color = "black", size = 14),
        legend.text = element_text(size = 12))

