library(ggplot2)
library(tidyverse)

source('src/R/ggplot_theme.R')

CBD <- fread("data/CBD.csv")

p1 <- CBD %>% 
  gather(sb_age, CBD, -Height)  %>%
  filter(Height != "NA" & CBD != 0) %>%
  separate(sb_age, c("Age", "Year"), sep = " ") %>%
  arrange(Age, Year, Height) %>%
  filter(Age == "Young") %>%
  ggplot(aes(x = CBD, y = Height)) +
  geom_path(aes(color = factor(Year)), size = 0.75, stat="identity") +
  scale_color_manual(values = c( "brown", "red", "green", "dark green"),
                     name = "Decades of SB \noutbreaks") +
  ylab("Canopy Height (m)") + xlab("") +
  theme_pub() +
  scale_x_continuous(limits = c(0, 0.3)) +
  ggtitle("(a) Young Stands (< 120 yrs)") +
  theme(plot.title  = element_text(hjust = 0, size = 12),
        strip.text.x = element_text(hjust = 0.5, size = 14, face = 'bold'),
        legend.position = "none")

p2 <- CBD %>% 
  gather(sb_age, CBD, -Height)  %>%
  filter(Height != "NA" & CBD != 0) %>%
  separate(sb_age, c("Age", "Year"), sep = " ") %>%
  arrange(Age, Year, Height) %>%
  filter(Age == "Old") %>%
  ggplot(aes(x = CBD, y = Height)) +
  geom_path(aes(color = factor(Year)), size = 0.75, stat="identity") +
  scale_color_manual(values = c("black", "dark gray", "brown", 
                                "red", "green", "dark green"),
                     name = "Decades of SB \noutbreaks") +
  ylab("Canopy Height (m)") + xlab("Canopy bulk density (kg/m2)") +
  theme_pub() +
  scale_x_continuous(limits = c(0, 0.3)) +
  ggtitle("(b) Old Stands (> 120 yrs)") +
  theme(plot.title  = element_text(hjust = 0, size = 12),
        strip.text.x = element_text(hjust = 0.5, size = 14, face = 'bold'),
        legend.position = c(0.85, 0.70))

grid.arrange(p1, p2, ncol = 1)
g <- arrangeGrob(p1, p2, ncol = 1)

ggsave(filename = "results/Figure4.jpg", g, height = 8, width = 5, 
       scale = 3, units = "cm", dpi=1200)
