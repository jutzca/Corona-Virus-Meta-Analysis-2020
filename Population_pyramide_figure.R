####Create population pyramide figure to compare ratio of male and female patients in the study
####Overall and by outcome (survival, death, severe, non-severe)
##Created by C.Jutzeler, April 1st, 2020

#Clear working space
rm(list=ls())

#Install libraries if required
# if(!require('ggthemes')){install.packages("ggthemes")}
# if(!require(ggpubr)){install.packages("ggpubr")}
# if(!require(ggplot2)){install.packages("ggplot2")}

#Load packages
library(ggplot2)
library('ggthemes')
library(ggpubr)

#Load data
corona_sex_distr <- read.csv(file.choose())

#Show variable names of data file loaded
names(corona_sex_distr)

##Plot data for the male patients
gg.male <- ggplot(data = subset(corona_sex_distr,Sex=='m'), 
                  mapping = aes(
                    x = as.factor(Study), 
                    y = frequency, 
                    fill = Sex,
                    label=paste(round(frequency, 0), "%", sep="")
                  )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=as.vector("#3E606F"))+
  geom_text(hjust=(1.1), size=3.5, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 12, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 14),
        legend.text.align = 0)+ 
  ggtitle("Male") + 
  coord_flip()    

##Plot data for the female patients
gg.female <-  ggplot(data = subset(corona_sex_distr,Sex=='f'), 
                     mapping = aes(
                       x = as.factor(Study), 
                       y = frequency, 
                       fill = Sex,
                       label=paste(round(frequency, 0), "%", sep="")
                     )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(-0.1), size=3.5, colour="#FFFFFF") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0)+ 
  ggtitle("Female") + 
  coord_flip()

## Plutting the graphs together together
grid.arrange(gg.female,
             gg.male,
             widths=c(0.4,0.4),
             ncol=2
)

