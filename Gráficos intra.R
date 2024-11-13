setwd("C:/Users/Nathalie/Desktop/DOUTORADO/TESE/Capítulo 2/RStudio")
dado<-read.table("completa.csv", header=T, sep=";", dec = ",")

install.packages("ggplot2")
install.packages("ggthemes")
install.packages("devtools")
install.packages("tidyverse")
require(ggplot2)
library(ggplot2)
library(tidyverse)
library(ggthemes)
dado<-na.omit(dado)
ggplot(dado, 
       aes(x=LA,colour=Age)) +
  geom_line(stat="density", adjust=2, size=1)+
  facet_grid(Species~.)+theme_classic()+xlab("LA (cm²)")+
  xlim(14,210)
 
ggplot(dado, aes(x = CCI, colour = Age)) +
  geom_density(adjust = 2, size = 1) +  # Usando geom_density() em vez de geom_line() para densidade
  geom_label(aes(label = rownames(dado)), nudge_y = 0.100, color = "green", fill = "white") +
  facet_grid(Species ~ .) +
  theme_classic() +
  xlab("CCI (nm.mmˉ².)") +
  xlim(8, 120) +
  labs(title = "Gráfico")

ggplot(dado, 
       aes(x=SLA,colour=Age)) +
  geom_line(stat="density", adjust=2, size=1)+
  facet_grid(Species~.)+theme_classic()+xlab("Specific Leaf Area (cm².gˉ¹)")+
  xlim(40,300)

ggplot(dado, 
       aes(x=LDMC,colour=Age)) +
  geom_line(stat="density", adjust=2, size=1)+
  facet_grid(Species~.)+theme_classic()+xlab("LDMC (mg.gˉ¹)")+
  xlim(160,540)

ggplot(dado, 
       aes(x=LNC,colour=Age)) +
  geom_line(stat="density", adjust=2, size=1)+
  facet_grid(Species~.)+theme_classic()+xlab("LNC (dag/kg)")+
  xlim(0.70,3.9)

ggplot(dado, 
       aes(x=LPC,colour=Age)) +
  geom_line(stat="density", adjust=2, size=1)+
  facet_grid(Species~.)+theme_classic()+xlab("LPC (dag/kg)")+
  xlim(0.03,0.35)

theme_classic()

help("geom_line")
head(dado)

ggplot(dado, 
       aes(x=LCC,colour=Age)) +
  geom_line(stat="density", adjust=2, size=1)+
  facet_grid(Species~.)+theme_classic()+xlab("LCC (dag/kg)")+
  xlim(51.5,57.5)
