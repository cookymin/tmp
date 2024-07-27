rm(list=ls())     

options(width = 125)
wd <- 'P:/Xiaomin Lu/Liver/Liver Fibrosis/FXR PBC-PSC/GS9674_Liver_Fibrosis_428-4025/For New PSC Study/Current Data'
setwd(wd)

library(readxl)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggrepel)
library(directlabels)
library(cowplot)
library(reshape2)
library(gtable)
library(grid)
library(gridBase)
library(gridExtra)

## Exact EAIR method ##
poisson.exact <- function(n,t){
  1-pbinom(n[1]-1,sum(n),t[1]/sum(t))
}

## BD Phase ##
dat <- read.csv("dat_prut_swimmer_BD.csv")
SUBJID = as.character(dat$SubjID)
dat = cbind(dat, SUBJID)
x.max = (max(dat$DEnAE_BD) + 10)/7

title1 = "GS9674-428-4025 Pruritis in BD Phase"
title2 = "as of 12-06-2018"
out.plot <- ggplot(dat) + geom_segment(aes(x = DStAE_BD/7, xend=DEnAE_BD/7,y=SUBJID, yend=SUBJID, colour = AtoxGr), size = 2) +
  xlab("Study Time (Weeks)") + ylab("Subject ID") + 
  ggtitle(bquote(atop(.(title1), atop(italic(.(title2)))))) + 
  geom_text(aes(label = Drop_BD, x = (dur_BD-30)/7, y = SUBJID), size = 2, color = "blue", vjust = 0.5) +
  geom_text(aes(label = Drop, x = (dur_BD-30)/7, y = SUBJID), size = 2, color = "blue", vjust = 0.5) +
  geom_text(aes(label = ifelse(AE_disc == 'Y', '^', ''), x = (dur_BD-30)/7, y = SUBJID), size = 2, color = "blue", vjust = 0) +
  labs(caption = "\n # - Discontinued study drug (^ indicates disc due to pruritus)\n $ - Discontinued study") +
  scale_color_manual(name = "Tox Grade", values=c('pink1','hotpink', 'hotpink4', 'purple4'), 
                     breaks=c("Grade 1", "Grade 2", "Grade 3", "Grade 4"), labels=c("Grade 1", "Grade 2", "Grade 3", "Grade 4")) +
  scale_x_continuous(minor_breaks=seq(0, x.max, 1), breaks=seq(0, x.max, 2)) +
  # facet_grid(~Armc, scales="free") + theme_minimal() + 
  facet_wrap(~Armc, ncol = 1, scales = "free_y") + theme_bw() + 
  theme(strip.text.y=element_text(angle=0), plot.title = element_text(hjust = 0.5))

out.plot    
ggsave("Pruritis onset by grade and arm - BD as of 12-06-2018.jpg", width=7, height=6, plot = out.plot)




## OLE Phase ##
dat <- read.csv("dat_prut_swimmer_OL.csv")
SUBJID = as.character(dat$SubjID)
dat = cbind(dat, SUBJID)
x.max = ceiling((max(dat$DEnAE_OL) + 10)/7)

title1 = "GS9674-428-4025 Pruritis in OLE Phase"
title2 = "as of 12-06-2018"
out.plot <- ggplot(dat) + geom_segment(aes(x = DStAE_OL/7, xend=DEnAE_OL/7,y=SUBJID, yend=SUBJID, colour = AtoxGr), size = 2) +
  xlab("Study Time (Weeks)") + ylab("Subject ID") +
  ggtitle(bquote(atop(.(title1), atop(italic(.(title2)))))) + 
  geom_text(aes(label = Drop_OL, x = (dur_OL-30)/7, y = SUBJID), size = 2, color = "blue", vjust = 0.5) +
  geom_text(aes(label = ifelse(AE_disc == 'Y', '^', ''), x = (dur_OL-30)/7, y = SUBJID), size = 2, color = "blue", vjust = 0) +
  labs(caption = "\n # - Discontinued study drug (^ indicates disc due to pruritus)") +
  scale_color_manual(name = "Tox Grade", values=c('pink1','hotpink', 'hotpink4', 'purple4'), 
                     breaks=c("Grade 1", "Grade 2", "Grade 3", "Grade 4"), labels=c("Grade 1", "Grade 2", "Grade 3", "Grade 4")) +
  scale_x_continuous(minor_breaks=seq(-10, x.max, 2), breaks=seq(-10, x.max, 4), limits = c(-10, x.max)) +
  # facet_grid(~Armc, scales="free") + theme_minimal() + 
  facet_wrap(~Armc, ncol = 1, scales = "free_y") + theme_bw() + 
  theme(strip.text.y=element_text(angle=0), plot.title = element_text(hjust = 0.5))

out.plot    
ggsave("Pruritis onset by grade and arm - OLE as of 12-06-2018.jpg", width=7, height=6, plot = out.plot)




## BD Phase ##
dat <- read.csv("dat_prut_swimmer_LT.csv")
SUBJID = as.character(dat$SubjID)
dat = cbind(dat, SUBJID)
x.max = max(dat$DEnAE_LT)/7


title1 = "GS9674-428-4025 Pruritis in LT Phase"
title2 = "as of 12-06-2018"
out.plot <- ggplot(dat) + geom_segment(aes(x = DStAE_LT/7, xend=DEnAE_LT/7, y=SUBJID, yend=SUBJID, colour = AtoxGr), size = 2) +
  xlab("Study Time (Weeks)") + ylab("Subject ID") +
  ggtitle(bquote(atop(.(title1), atop(italic(.(title2)))))) + 
  geom_text(aes(label = Drop_LT, x = (dur_LT-30)/7, y = SUBJID), size = 2, color = "blue", vjust = 0.5) +
  geom_text(aes(label = Drop, x = (dur_LT-30)/7, y = SUBJID), size = 2, color = "blue", vjust = 0.5) +
  geom_text(aes(label = "*", y = SUBJID, x = Day_OLE/7), colour = "green2", size = 4, hjust = 0.5, vjust = 0.67) + 
  geom_text(aes(label = ifelse(AE_disc == 'Y', '^', ''), x = (dur_LT-30)/7, y = SUBJID), size = 2, color = "blue", vjust = 0) +
  labs(caption = "\n * - OLE start time\n # - Discontinued study drug (^ indicates disc due to pruritus)\n $ - Discontinued study") +
  scale_color_manual(name = "Tox Grade", values=c('pink1','hotpink', 'hotpink4', 'purple4'), 
                     breaks=c("Grade 1", "Grade 2", "Grade 3", "Grade 4"), labels=c("Grade 1", "Grade 2", "Grade 3", "Grade 4")) +
  scale_x_continuous(minor_breaks=seq(0, x.max, 2), breaks=seq(0, x.max, 4), limits = c(0, x.max)) +
  facet_wrap(~Armc, ncol = 1, scales = "free_y") + theme_bw() + 
  theme(strip.text.y=element_text(angle=0), plot.title = element_text(hjust = 0.5))

out.plot    
ggsave("Pruritis onset by grade and arm - LT as of 12-06-2018.jpg", width=7, height=6, plot = out.plot)




