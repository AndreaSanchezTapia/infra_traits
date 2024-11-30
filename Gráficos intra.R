#setwd("C:/Users/Nathalie/Desktop/DOUTORADO/TESE/Capítulo 2/RStudio")
dado <- read.table("completa.csv", header=T, sep=";", dec = ",")

pal <- c( "#c2e699", "#78c679", "#006837")
pal <- c( "grey20", "grey50", "grey50")
#viridis::magma(3, begin = .2, end = .8)
#plot(pal)
#installviridis#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("devtools")
#install.packages("tidyverse")
#require(ggplot2)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(dplyr)
#dado <- na.omit(dado)
dado
dados <- tidyr::pivot_longer(dado, -c(1,2), names_to = "variable")

theme_plot <-
  theme(axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(), #panel.grid.major = element_blank(),
        axis.ticks.y = element_blank(), #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        #plot.title.position#
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0, face = 2),
        strip.text.x = element_text(face = 2, hjust = 0.2),
        title = element_text(face = 2),
        strip.placement = "inside")

LA <- dados %>%
  filter(variable %in% c("LA")) %>%
  ggplot(aes(x = value, col = Age, linetype = Age)) +
  geom_line(stat = "density") +
  facet_grid(Species~., scales = "free") +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_color_manual(values = pal) +
  labs(title = "G.") +
  ylab("") +
  xlab("LA (cm²)") +
  theme_plot
LA
CCI <- dados %>%
  filter(variable %in% c("CCI")) %>%
  ggplot(aes(x = value, col = Age, linetype = Age)) +
  geom_line(stat = "density") +
  facet_grid(Species~., scales = "free") +
  labs(title = "A.") +
  ylab("") +
  xlab("CCI (nm.mmˉ².)") +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_color_manual(values = pal) +
  theme_plot
CCI

SLA <- dados %>%
  filter(variable %in% c("SLA")) %>%
  ggplot(aes(x = value, col = Age, linetype = Age)) +
  geom_line(stat = "density", adjust = 1.5) +
  facet_grid(Species~., scales = "free") +
  labs(title = "B.") +
  ylab("") +
  xlab("Specific Leaf Area (cm².gˉ¹)") +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_color_manual(values = pal) +
  theme_plot
SLA
LDMC <- dados %>%
  filter(variable %in% c("LDMC")) %>%
  ggplot(aes(x = value, col = Age, linetype = Age)) +
  geom_line(stat = "density", adjust = 1) +
  facet_grid(Species~., scales = "free") +
  labs(title = "C.") +
ylab("") +
  xlab("LDMC (mg.gˉ¹)") +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_color_manual(values = pal) +
  theme_plot
LDMC

LNC <- dados %>%
  filter(variable %in% c("LNC")) %>%
  ggplot(aes(x = value, col = Age, linetype = Age)) +
  geom_line(stat = "density") +
  facet_grid(Species~., scales = "free") +
  labs(title = "D.") +
  ylab("Density") +
  xlab("LNC (dag/kg)") +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_color_manual(values = pal) +
  theme_plot
LNC

LPC <- dados %>%
  filter(variable %in% c("LPC")) %>%
  ggplot(aes(x = value, col = Age, linetype = Age)) +
  geom_line(stat = "density") +
  facet_grid(Species~., scales = "free") +
  labs(title = "E.") +
  ylab("") +
  xlab("LPC (dag/kg)") +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_x_continuous(labels = function(x) pretty(x)) +
  scale_color_manual(values = pal) +
  theme_plot
LPC
LCC <- dados %>%
  filter(variable %in% c("LCC")) %>%
  ggplot(aes(x = value, col = Age, linetype = Age)) +
  geom_line(stat = "density") +
  facet_grid(Species~., scales = "free") +
  xlab("LCC (dag/kg)") +
  ylab("") +
  labs(title = "F.") +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_color_manual(values = pal) +
  theme_plot
LCC


library(patchwork)
CCI + SLA + LDMC + LNC + LPC + LCC + LA +
  plot_layout(guides = "collect", axes = "collect") &
    theme(legend.position = 'bottom')

CCI + SLA + LDMC +
  plot_layout(guides = "collect", axes = "collect")
CCI + SLA + LDMC +
  plot_layout(ncol = 1, guides = "collect", axes = "collect")
CCI + SLA + LDMC + LNC + LCC + LPC +
  plot_layout(guides = "collect", axes = "collect")
