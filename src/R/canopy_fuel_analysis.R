library(sciplot)
library(data.table)
library(agricolae)
library(ggplot2)
library(tidyverse)
library(broom)

source('src/R/ggplot_theme.R')

cfl <- fread("data/CFL.csv") %>%
  mutate(AFCL = AFCL*10,
         LCW = LCW*10,
         DCW = DCW*10,
         LiDe = ifelse(Status == "Li", "Live", "Dead"))
cc <- fread("data/CC.csv") %>%
  group_by(Site, Year, Age) %>%
  summarise_all(c("sum")) %>%
  ungroup()
cc <- cc %>%
  select(-Site) %>%
  group_by(Year, Age) %>%
  summarise_all(c("mean", "sd")) %>%
  ungroup()

# Prep the data
pfuel <- cfl %>%
  filter(S.F == "S" & Plot != "0") %>%
  group_by(Site, Year, Age, Plot) %>%
  select(CBD, CBH, AFCL, LCW, DCW, BA_Li, BA_Gr, BA_Ye, BA_Nd, BA_Tw, BA_Br, BA_Sn, BA_De,
         LiFol, DeFol, Li.1hr, De.1hr, Li.10hr, De.10hr, Li.100hr, De.100hr) %>%
  summarise_all(c("sum", "mean", "se")) %>%
  ungroup() %>%
  mutate(sb_age = paste0(Year, Age),
         ba_g50pct_nd = BA_Gr_mean + BA_Ye_mean + BA_Nd_mean,
         ba_l50pct_tw = BA_Br_mean + BA_Sn_mean)

pstat <- cfl %>%
  filter(S.F == "S" & Plot != "0") %>%
  group_by(Site, Year, Age) %>%
  select(CBD, CBH, AFCL, LCW, DCW, BA_Li, BA_Gr, BA_Ye, BA_Nd, BA_Tw, BA_Br, BA_Sn, BA_De,
         LiFol, DeFol, Li.1hr, De.1hr, Li.10hr, De.10hr, Li.100hr, De.100hr) %>%
  summarise_all(c("sum")) %>%
  group_by(Year, Age) %>%
  summarise_all(c("mean", "sd")) %>%
  ungroup() %>%
  mutate(sb_age = paste0(Year, Age),
         ba_g50pct_nd_mean = BA_Gr_mean + BA_Ye_mean + BA_Nd_mean,
         ba_l50pct_tw_mean = BA_Br_mean + BA_Sn_mean,
         ba_g50pct_nd_sd = BA_Gr_sd + BA_Ye_sd + BA_Nd_sd,
         ba_l50pct_tw_sd = BA_Br_sd + BA_Sn_sd)

# Create the ANOVA model and Tukey HSD tests
aov.models <- pfuel %>%
  select(-Year, -Age) %>%
  gather(variable, value, -sb_age, -Site, -Plot) %>%
  split(.$variable) %>%
  map(~ aov(value ~ sb_age + Site/Plot, data = .x)) %>%
  map(HSD.test, trt = 'sb_age', alpha = 0.05)

# Plot the data
#Create Figure 2
p1 <- ggfunction(pstat, "BA_Li_mean", "BA_Li_sd", "BA_Li_mean + BA_Li_sd", "a", "FALSE") + 
  xlab("") + ylab("Basal Area (m2/ha)") + ggtitle("(a) Live")

p2 <- ggfunction(pstat, "ba_g50pct_nd_mean", "ba_g50pct_nd_sd", "ba_g50pct_nd_mean + ba_g50pct_nd_sd", "a", "FALSE") + 
  xlab("") + ylab("Basal Area (m2/ha)") + ggtitle("(b) > 50% needle retention")

p3 <- ggfunction(pstat, "BA_Tw_mean", "BA_Tw_sd", "BA_Tw_mean + BA_Tw_sd", "a", "FALSE") + 
  xlab("") + ylab("Basal Area (m2/ha)") + ggtitle("(c) < 50% needles, > 50% 1-hr")

p4 <- ggfunction(pstat, "ba_l50pct_tw_mean", "ba_l50pct_tw_sd", "ba_l50pct_tw_mean + ba_l50pct_tw_sd", "a", "FALSE") + 
  xlab("Decades of spruce beetle outbreaks") + ylab("Basal Area (m2/ha)") + ggtitle("(d) < 50% 1-hr") +
  theme(axis.text.x = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position=c(0.9, 0.7))

grid.arrange(p1, p2, p3, p4, ncol = 1)
g <- arrangeGrob(p1, p2, p3, p4, ncol = 1)

ggsave(filename = "results/Figure2.jpg", g, height = 8, width = 6, 
       scale = 3, units = "cm", dpi=1200)

# Create Figure 3
p1 <- ggfunction(pstat, "LiFol_mean", "LiFol_sd", "LiFol_mean + LiFol_sd", "a", "TRUE") + 
  xlab("") + ylab("Live foliage (Mg/ha)")

p2 <- ggfunction(pstat, "Li.1hr_mean", "Li.1hr_sd", "Li.1hr_mean + Li.1hr_sd", "d", "TRUE") + 
  xlab("") + ylab("Live 1hr (Mg/ha)")

p3 <- ggfunction(pstat, "Li.10hr_mean", "Li.10hr_sd", "Li.10hr_mean + Li.10hr_sd", "g", "TRUE") + 
  xlab("") + ylab("Live 10hr (Mg/ha)")

p4 <- ggfunction(pstat, "Li.100hr_mean", "Li.100hr_sd", "Li.100hr_mean + Li.100hr_sd", "j", "TRUE") + 
  xlab("Decades of spruce beetle outbreaks") + ylab("Live 100hr (Mg/ha)") +
  theme(axis.text.x = element_text(hjust = 0.5))

p5 <- ggfunction(pstat, "DeFol_mean", "DeFol_sd", "DeFol_mean + DeFol_sd", "b", "TRUE") + 
  xlab("") + ylab("Dead foliage (Mg/ha)")

p6 <- ggfunction(pstat, "De.1hr_mean", "De.1hr_sd", "De.1hr_mean + De.1hr_sd", "e", "TRUE") + 
  xlab("") + ylab("Dead 1hr (Mg/ha)")

p7 <- ggfunction(pstat, "De.10hr_mean", "De.10hr_sd", "De.10hr_mean + De.10hr_sd", "h", "TRUE") + 
  xlab("") + ylab("Dead 10hr (Mg/ha)")

p8 <- ggfunction(pstat, "De.100hr_mean", "De.100hr_sd", "De.100hr_mean + De.100hr_sd", "k", "TRUE") + 
  xlab("Decades of spruce beetle outbreaks") + ylab("Dead 100hr (Mg/ha)") +
  theme(axis.text.x = element_text(hjust = 0.5))

p9 <- ggfunction(pstat, "CBH_mean", "CBH_sd", "CBH_mean + CBH_sd", "c", "TRUE") + 
  xlab("") + ylab("Canopy base height (m)")

p10 <- ggfunction(pstat, "CBD_mean", "CBD_sd", "CBD_mean + CBD_sd", "f", "TRUE") + 
  xlab("") + ylab("Canopy bulk density (kg/m2)")

p11 <- ggfunction(pstat, "AFCL_mean", "AFCL_sd", "AFCL_mean + AFCL_sd", "i", "TRUE") + 
  xlab("") + ylab("Available crown fuel load (Mg/ha)")

p12 <- ggfunction(cc, "mean", "sd", "mean + sd", "l", "TRUE") +
  xlab("Decades of spruce beetle outbreaks") + ylab("Canopy openness (m)") +
  theme(axis.text.x = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position=c(0.25, 0.82))

grid.arrange(p1, p5, p9,
             p2, p6, p10,
             p3, p7, p11,
             p4, p8, p12,
             ncol = 3)

g <- arrangeGrob(p1, p5, p9,
                 p2, p6, p10,
                 p3, p7, p11,
                 p4, p8, p12,
                 ncol = 3)

ggsave(filename = "results/Figure3.jpg", g, height = 9, width = 10, 
       scale = 2.75, units = "cm", dpi=1200)

# geom_text(data = aov.models$LiFol_sum$groups, 
#           aes(x = sub("\\s+","", value), y = 0, label = groups), 
#           vjust = -1) +

