####Create population pyramide figure to compare ratio of male and female patients in the study
####Overall and by outcome (survival, death, severe, non-severe)
##Created by C.Jutzeler, April 1st, 2020

#Clear working space
rm(list=ls())

#Install libraries if required
if(!require('ggthemes')){install.packages("ggthemes")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(gridExtra)){install.packages("gridExtra")}

#Load packages
library(ggplot2)
library('ggthemes')
library(ggpubr)
library(gridExtra)

#Load data
corona_sex_distr <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_demographics_all_patients.csv", sep =',', header = TRUE)

#Show variable names of data file loaded
names(corona_sex_distr)

#------------------------- Adult: Cohort, case series, and case studies (pooled------------------------------------------------------------------------------------------------------
##Plot data for the male patients
#Subset male patients
#Note: All case studies were pooled -> 'Case Studies Pooled'
corona_sex_distr_m <- subset(corona_sex_distr, (!(is.na(Male))& (!(Study_type== 'Case Study')) & 	Study_Population =='Adult'))

#create plot
gg.male <- ggplot(data = corona_sex_distr_m, 
                  mapping = aes(
                    x = as.factor(Study_nr), 
                    y = Male_frequency, 
                    fill = Sex_m,
                    label=paste(round(Male_frequency, 0),  "% ", "[n=", (Male), "]",   sep="")
                  )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=("#3E606F"))+
  geom_text(hjust=(1.1), size=3, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 12),
        legend.text.align = 0, 
  axis.ticks.y = element_blank())+ 
  ggtitle("Male") + 
  coord_flip()   

gg.male


##Plot data for the female patients
#Subset female patients
#Note: All case studies were pooled -> 'Case Studies Pooled'
corona_sex_distr_f <- subset(corona_sex_distr, (!(is.na(Female))& (!(Study_type== 'Case Study')) & 	Study_Population =='Adult'))

#create plot
gg.female <-  ggplot(data = corona_sex_distr_f, 
                     mapping = aes(
                       x = as.factor(Study_nr), 
                       y = as.numeric(Female_frequency), 
                       fill = Sex_f,
                       label=paste(round(Female_frequency, 0), "% ", "[n=", (Female), "]",   sep="")
                     )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(-0.1), size=3, colour="#FFFFFF") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0,
        axis.ticks.y = element_blank())+ 
  ggtitle("Adult: Female") + 
  coord_flip()

gg.female



#---Pool figure---#
library("cowplot")
adult <-plot_grid(gg.female, gg.male, 
               rel_widths = c(0.4,0.45),
               ncol = 2, nrow = 1)
adult


#------------------------- Children: Cohort, case series, and case studies (pooled) ------------------------------------------------------------------------------------------------------
##Plot data for the male patients
#Subset male patients
corona_sex_distr_children_m <- subset(corona_sex_distr, (!(is.na(Male)) & (!(Study_type== 'Case Study')) & (Study_Population =='Pediatric' | Study_Population =='Neonatal')))

#create plot
gg.male_children <- ggplot(data = corona_sex_distr_children_m, 
                  mapping = aes(
                    x = as.factor(Study_nr), 
                    y = Male_frequency, 
                    fill = Sex_m,
                    label=paste(round(Male_frequency, 0),  "% ", "[n=", (Male), "]",   sep="")
                  )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=("#3E606F"))+
  geom_text(hjust=(1.1), size=3, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 12),
        legend.text.align = 0, 
        axis.ticks.y = element_blank())+ 
  ggtitle("Male") + 
  coord_flip()   

gg.male_children


##Plot data for the female patients
#Subset female patients, cohort and case series
#Note: All case studies were pooled -> 'Case Studies Pooled'
corona_sex_distr_children_f <- subset(corona_sex_distr, (!(is.na(Male)) & (!(Study_type== 'Case Study'))& (Study_Population =='Pediatric' | Study_Population =='Neonatal')))

#create plot
gg.female_children <-  ggplot(data = corona_sex_distr_children_f, 
                     mapping = aes(
                       x = as.factor(Study_nr), 
                       y = as.numeric(Female_frequency), 
                       fill = Sex_f,
                       label=paste(round(Female_frequency, 0), "% ", "[n=", (Female), "]",   sep="")
                     )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(-0.1), size=3, colour="#FFFFFF") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=10),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0,
        axis.ticks.y = element_blank())+ 
  ggtitle("Pediatric/ neonatal: Female") + 
  coord_flip()

gg.female_children



#---Pool figure;---#
library("cowplot")
children <-plot_grid(gg.female_children, gg.male_children, 
                  rel_widths = c(0.4,0.45),
                  ncol = 2, nrow = 1)
children


####--Pooling Plots: Adults and children----####
plot_grid(adult, children, 
          labels = c( "A", "B"),
          rel_heights= c(4, 1),
          ncol = 1, nrow = 2)


#------------------------- Severe ------------------------------------------------------------------------------------------------------

#Clear working space
rm(list=ls())

#Load data
corona_sex_distr_severe_non_severe <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_demographics_of_severity_mortality_cohort.csv", sep =',', header = TRUE)

##Subset data for severe CoVID-19 disease
corona_sex_distr_severe <- subset(corona_sex_distr_severe_non_severe, disease_status=='severe')

##Plot data for the male patients with severe CoID-19

#Subset male patients with severe CoID-19 and exclude missing data
corona_sex_distr_severe_m <- subset(corona_sex_distr_severe, (!(is.na(Male))))

#create plot
gg.male_severe <- ggplot(data = corona_sex_distr_severe_m, 
                           mapping = aes(
                             x = as.factor(Study_nr), 
                             y = Male_frequency, 
                             fill = Sex_m,
                             label=paste(round(Male_frequency, 0),  "% ", "[n=", (Male), "]",   sep="")
                           )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=("#3E606F"))+
  geom_text(hjust=(1.1), size=3, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 12),
        legend.text.align = 0, 
        axis.ticks.y = element_blank())+ 
  ggtitle("Male") + 
  coord_flip()   

gg.male_severe

##Plot data for the female patients with severe CoID-19

#Subset female patients with severe COVID-19 and exclude missing data
corona_sex_distr_severe_f<- subset(corona_sex_distr_severe, (!(is.na(Female))))
#create plot
gg.female_severe <-  ggplot(data = corona_sex_distr_severe_f, 
                              mapping = aes(
                                x = as.factor(Study_nr), 
                                y = as.numeric(Female_frequency), 
                                fill = Sex_f,
                                label=paste(round(Female_frequency, 0), "% ", "[n=", (Female), "]",   sep="")
                              )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(-0.1), size=3, colour="#FFFFFF") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0,
        axis.ticks.y = element_blank())+ 
  ggtitle("Female") + 
  coord_flip()

gg.female_severe 


#---Pool figure: Severe CoVID-19 disease ---#
library("cowplot")
severe <-plot_grid(gg.female_severe, gg.male_severe, 
                     rel_widths = c(0.4,0.45),
                   labels=c("B     Severe"), hjust = -0.01,vjust = 1,
                     ncol = 2, nrow = 1)
severe


#------------------------- Non-severe ------------------------------------------------------------------------------------------------------

# #Clear working space
# rm(list=ls())

#Load data
corona_sex_distr_nonsevere_non_nonsevere <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_demographics_of_severity_mortality_cohort.csv", sep =',', header = TRUE)

##Subset data for patients with non-severe CoID-19
corona_sex_distr_nonsevere <- subset(corona_sex_distr_nonsevere_non_nonsevere, disease_status=='non_severe')

##Plot data for the male patients with non-severe CoID-19

#Subset male patients and exclude missing data
corona_sex_distr_nonsevere_m <- subset(corona_sex_distr_nonsevere, (!(is.na(Male))))

#create plot
gg.male_nonsevere <- ggplot(data = corona_sex_distr_nonsevere_m, 
                         mapping = aes(
                           x = as.factor(Study_nr), 
                           y = Male_frequency, 
                           fill = Sex_m,
                           label=paste(round(Male_frequency, 0),  "% ", "[n=", (Male), "]",   sep="")
                         )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=("#3E606F"))+
  geom_text(hjust=(1.1), size=3, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 12),
        legend.text.align = 0, 
        axis.ticks.y = element_blank())+ 
  ggtitle("Male") + 
  coord_flip()   

gg.male_nonsevere

##Plot data for female patients with non-severe CoID-19
#Subset femaale patients with non-severe CoID-19 and exclude missing data

corona_sex_distr_nonsevere_f<- subset(corona_sex_distr_nonsevere, (!(is.na(Female))))
#create plot
gg.female_nonsevere <-  ggplot(data = corona_sex_distr_nonsevere_f, 
                            mapping = aes(
                              x = as.factor(Study_nr), 
                              y = as.numeric(Female_frequency), 
                              fill = Sex_f,
                              label=paste(round(Female_frequency, 0), "% ", "[n=", (Female), "]",   sep="")
                            )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(-0.1), size=3, colour="#FFFFFF") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.text.x = element_text(size=10),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0,
        axis.ticks.y = element_blank())+ 
  ggtitle("Female") + 
  coord_flip()

gg.female_nonsevere 


#---Pool figure: Patients with non-severe CoID-19 ---#
library("cowplot")
nonsevere <-plot_grid(gg.female_nonsevere, gg.male_nonsevere, 
                   rel_widths = c(0.4,0.45),
                   labels = ("A     Non-severe"), hjust = -0.01,vjust = 1,
                   ncol = 2, nrow = 1)
nonsevere





#------------------------- Non survivors ------------------------------------------------------------------------------------------------------

# #Clear working space
# rm(list=ls())

#Load data
corona_sex_distr_nonsurvivor <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_demographics_of_severity_mortality_cohort.csv", sep =',', header = TRUE)

##Subset data for nonsurvivors
corona_sex_distr_nonsurv <- subset(corona_sex_distr_nonsurvivor, disease_status=='death')

##Plot data for the male patients with CoVID-19 that died
#Subset male patients with CoVID-19 that died and exclude missing data
corona_sex_distr_nonsurv_m <- subset(corona_sex_distr_nonsurv, (!(is.na(Male))))

#create plot
gg.male_nonsurvivor <- ggplot(data = corona_sex_distr_nonsurv_m , 
                           mapping = aes(
                             x = as.factor(Study_nr), 
                             y = Male_frequency, 
                             fill = Sex_m,
                             label=paste(round(Male_frequency, 0),  "% ", "[n=", (Male), "]",   sep="")
                           )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=("#3E606F"))+
  geom_text(hjust=(1.1), size=3, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 12),
        legend.text.align = 0, 
        axis.ticks.y = element_blank())+ 
  ggtitle("Male") + 
  coord_flip()   

gg.male_nonsurvivor

##Plot data for the female patients with CoVID-19 that died
#Subset female patients with CoVID-19 that died and exclude missing data
corona_sex_distr_nonsurv <- subset(corona_sex_distr_nonsurvivor, disease_status=='death')

corona_sex_distr_nonsurv_f <- subset(corona_sex_distr_nonsurv, (!(is.na(Female))))

#create plot
gg.female_nonsurvivor <-  ggplot(data = corona_sex_distr_nonsurv_f, 
                              mapping = aes(
                                x = as.factor(Study_nr), 
                                y = as.numeric(Female_frequency), 
                                fill = Sex_f,
                                label=paste(round(Female_frequency, 0), "% ", "[n=", (Female), "]",   sep="")
                              )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(-0.1), size=3, colour="#FFFFFF") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=10),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0,
        axis.ticks.y = element_blank())+ 
  ggtitle("Female") + 
  coord_flip()

gg.female_nonsurvivor


#---Pool figure: Patients with CoVID-19 that died---#
library("cowplot")
death<-plot_grid(gg.female_nonsurvivor, gg.male_nonsurvivor, 
                      rel_widths = c(0.4,0.45),
                      labels = ("D     Non-survivor"), hjust = -0.01,vjust = 1,
                      ncol = 2, nrow = 1)
death


#------------------------- Survivors ------------------------------------------------------------------------------------------------------

# #Clear working space
# rm(list=ls())

#Load data
corona_sex_distr_surv <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_demographics_of_severity_mortality_cohort.csv", sep =',', header = TRUE)

##Subset data for survivors
corona_sex_distr_surv <- subset(corona_sex_distr_surv, disease_status=='survivor')

##Plot data for the male patients with CoVID-19 that survived
#Subset male patients with CoVID-19 that survivedand exclude missing data
corona_sex_distr_surv_m <- subset(corona_sex_distr_surv, (!(is.na(Male))))

#create plot
gg.male_survivor <- ggplot(data = corona_sex_distr_surv_m, 
                              mapping = aes(
                                x = as.factor(Study_nr), 
                                y = Male_frequency, 
                                fill = Sex_m,
                                label=paste(round(Male_frequency, 0),  "% ", "[n=", (Male), "]",   sep="")
                              )) +
  geom_bar(stat = "identity") +
  scale_y_continuous('Frequency [%]', limits = c(0, 100)) + 
  scale_fill_manual(values=("#3E606F"))+
  geom_text(hjust=(1.1), size=3, colour="#FFFFFF") +
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_text(size = 10, color = "#5D646F"),
        axis.text.x = element_text(size = 10, color = "#5D646F"),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 12),
        legend.text.align = 0, 
        axis.ticks.y = element_blank())+ 
  ggtitle("Male") + 
  coord_flip()   

gg.male_survivor

##Plot data for the feale patients with CoVID-19 that survived
#Subset female patients with CoVID-19 that survived and exclude missing data
corona_sex_distr_surv_f <- subset(corona_sex_distr_surv, (!(is.na(Female))))

#create plot
gg.female_survivor <-  ggplot(data = corona_sex_distr_surv_f, 
                                 mapping = aes(
                                   x = as.factor(Study_nr), 
                                   y = as.numeric(Female_frequency), 
                                   fill = Sex_f,
                                   label=paste(round(Female_frequency, 0), "% ", "[n=", (Female), "]",   sep="")
                                 )) +
  geom_bar(stat = "identity") +
  geom_text(hjust=(-0.1), size=3, colour="#FFFFFF") +
  scale_y_continuous('Frequency [%]', limits = c(100, 0), trans = 'reverse') + 
  scale_fill_manual(values=as.vector("#8C3F4D"))+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10, margin = margin(b = 10), hjust = 0.9),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 12, margin = margin(b = 10, t = 50), color = "#5D646F"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=10),
        strip.text = element_text(color = "#5D646F", size = 12, face = "bold", hjust = 0.030),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin=unit(c(0.1,0.1,0.1,0.05),"cm"),
        legend.position = "none",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(size = 10),
        legend.text.align = 0,
        axis.ticks.y = element_blank())+ 
  ggtitle("Female") + 
  coord_flip()

gg.female_survivor


#---Pool figure: Patients with CoVID-19 that survived ---#
library("cowplot")
survivor<-plot_grid(gg.female_survivor, gg.male_survivor, 
                 rel_widths = c(0.4,0.45),
                 labels = ("C    Survivor"), hjust = -0.01,vjust = 1,
                 ncol = 2, nrow = 1)
survivor




#---Pool figure: OVERALL severe, nonsevere, death, survivors ---####
library("cowplot")
overall<-plot_grid(nonsevere,severe , survivor, death,
                                        ncol = 2, nrow = 2)
overall







