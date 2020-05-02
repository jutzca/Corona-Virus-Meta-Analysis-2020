#--------------------Forrest Plot for Age Groups------------------------------------------------------------------------------------------------------
#Clear working space
rm(list=ls())

#Install libraries if required
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(plyr)) install.packages("plyr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(forcats)) install.packages("forcats")
if(!require(metamedian)) install.packages("metamedian")
if(!require(cowplot)) install.packages("cowplot")

#Load libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(forcats)
library(metamedian)
library("cowplot")

#-------------------------Age distribution of adult COVID-19 patients------------------------------------------------------------------------------------------------------
#Load data
age_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_demographics_all_patients.csv", sep =',', header = TRUE)

#Subset data: Adult COVID-19 patients, case series and cohort studies, and available information on median age
age_distr_adults <- subset(age_data,(!(is.na(Age_median_nd)) & (!(Study_type== 'Case Study')) & 	Study_Population =='Adult'))

#Show names of colimns
names(age_distr_adults)

#Calculate pooled median, Q1, and Q3 of all studies using sample size as weight
median_age_adults_overall<-pool.med(age_distr_adults$Age_median_nd, age_distr_adults$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_age_adults_overall

Q1_age_adults_overall<-pool.med(age_distr_adults$Age_Q1_nd, age_distr_adults$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_age_adults_overall

Q3_age_adults_overall<-pool.med(age_distr_adults$Age_Q3_nd, age_distr_adults$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_age_adults_overall

#Plot age distribution
age_adult_plot<- age_distr_adults %>% 
  mutate(study_sorted = fct_reorder(Study_nr2, desc(Age_median_nd))) %>%  # order studies in descending order of median age
  ggplot(aes(x = study_sorted, y = as.numeric(Age_median_nd), ymin = Age_Q1_nd, ymax = Age_Q3_nd)) +
  geom_point(position = position_dodge(width = 0.2), aes(size = number_of_patients)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  geom_hline(yintercept=median_age_adults_overall$pooled.est, size= 1, color="red", linetype='dotdash')+
  ggtitle("Adults")+
  scale_y_continuous(limits=c(0, 100), breaks=c(0, 25, 50, 75, 100))+
  coord_flip() +
  theme_bw()+
  ylab("Median Age [years]")+
  xlab("")+
  theme(axis.title.x = element_text(size=10),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.title = element_text(size = 10, hjust = 0.5, face = 'bold'))+  labs(size='Study Size')+
  guides(size = guide_legend(override.aes = list(size=c(1,2,3,4))))

age_adult_plot

#-------------------------Age distribution of pregnant COVID-19 patients-----------------------------------------------------------------------------------------------------

#Subset data: Pregnant COVID-19 patients-and available information on age
age_distr_pregnant<- subset(age_data,(!(is.na(Age_median_nd))  & 	(Study_Population =='Pregnant' )))

#Calculate pooled median, Q1, and Q3 of all studies using sample size as weight
median_age_pregnants_overall<-pool.med(age_distr_pregnant$Age_median_nd,age_distr_pregnant$number_of_patients, norm.approx = T, coverage.prob = 1)
median_age_pregnants_overall

Q1_age_pregnants_overall<-pool.med(age_distr_pregnant$Age_Q1_nd,age_distr_pregnant$number_of_patients, norm.approx = T, coverage.prob = 1)
Q1_age_pregnants_overall

Q3_age_pregnants_overall<-pool.med(age_distr_pregnant$Age_Q3_nd,age_distr_pregnant$number_of_patients, norm.approx = T, coverage.prob = 1)
Q3_age_pregnants_overall

#Plot age distribution
age_pregnant_plot<-age_distr_pregnant %>% 
  mutate(study_sorted = fct_reorder(Study_nr2, desc(Age_median_nd))) %>%  # order studies in descending order of median age
  ggplot(aes(x = study_sorted, y = as.numeric(Age_median_nd), ymin = Age_Q1_nd, ymax = Age_Q3_nd)) +
  geom_point(position = position_dodge(width = 0.2), aes(size = number_of_patients), shape=18) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  geom_hline(yintercept=median_age_pregnants_overall$pooled.est, size= 1, color="red", linetype='dotdash')+
  ggtitle("Pregnant Women")+
  scale_y_continuous(limits=c(0, 40), breaks=c(0, 20, 40))+
  coord_flip() +
  theme_bw()+
  ylab("Median Age [years] \n")+
  xlab("")+
  theme(axis.title.x = element_text(size=10),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.title = element_text(size = 10, hjust = 0.5, face = 'bold'))+  labs(size='Study Size')+
  guides(size = guide_legend(override.aes = list(size=c(1,2,3,4,5))))

age_pregnant_plot

#-------------------------Age distribution of pediatric and neonatal COVID-19 patients------------------------------------------------------------------------------------------------------

#Subset data: Pediatric and neonatal COVID-19 patients
age_distr_pediatric_neonates <- subset(age_data,(!(is.na(Age_median_nd))  & 	(!(Study_Population =='Adult' |Study_Population =='Pregnant' ))))
names(age_distr_pediatric_neonates)

#Calculate pooled median, Q1, and Q3 of all studies using sample size as weight
median_age_pediatrics_overall<-pool.med(age_distr_pediatric_neonates$Age_median_nd, age_distr_pediatric_neonates$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_age_pediatrics_overall

Q1_age_pediatrics_overall<-pool.med(age_distr_pediatric_neonates$Age_Q1_nd, age_distr_pediatric_neonates$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_age_pediatrics_overall

Q3_age_pediatrics_overall<-pool.med(age_distr_pediatric_neonates$Age_Q3_nd, age_distr_pediatric_neonates$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_age_pediatrics_overall

#Plot age distribution
age_pediatric_plot <- age_distr_pediatric_neonates %>% 
  mutate(study_sorted = fct_reorder(Study_nr2, desc(Age_median_nd))) %>%  # order studies in descending order of median age
  ggplot(aes(x = study_sorted, y = as.numeric(Age_median_nd), ymin = Age_Q1_nd, ymax = Age_Q3_nd)) +
  geom_point(position = position_dodge(width = 0.2), aes(size = number_of_patients), shape=17) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  geom_hline(yintercept=median_age_pediatrics_overall$pooled.est, size= 1, color="red", linetype='dotdash')+
  ggtitle("Pediatrics/Neonates")+
  coord_flip() +
  theme_bw()+
  ylab("Median Age [years]")+
  xlab("")+
  theme(axis.title.x = element_text(size=10),
        legend.position = "none",
              panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.title = element_text(size = 10, hjust = 0.5, face = 'bold'))+  labs(size='Study Size')+
  guides(size = guide_legend(override.aes = list(size=c(1,2,3))))

age_pediatric_plot

#---Pool figure---#
library("cowplot")
pregnant_pediatric<-plot_grid(age_pregnant_plot, age_pediatric_plot, 
          labels = c( "B", "C"),
          ncol = 1, nrow = 2)
pregnant_pediatric

all_patients  <-plot_grid(age_adult_plot, pregnant_pediatric, 
               labels = c( "A"),
          rel_widths = c(1.5, 1),
               ncol = 2, nrow = 1)
all_patients 


#-------------------------Age distribution of severe patients ------------------------------------------------------------------------------------------------------
#Clear working space
rm(list=ls())

#Load data
age_distr_severe_severe_non_severe <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_demographics_of_severity_mortality_cohort.csv", sep =',', header = TRUE)

##Subset data for survivors
age_distr_severe <- subset(age_distr_severe_severe_non_severe, disease_status=='severe' & Median>0)

#Show names of colimns
names(age_distr_severe)

#Calculate pooled median, Q1, and Q3 of all studies using sample size as weight
median_age_severe_overall<-pool.med(age_distr_severe$Median, age_distr_severe$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_age_severe_overall
class(median_age_severe_overall$pooled.est)

Q1_age_severe_overall<-pool.med(age_distr_severe$Q1, age_distr_severe$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_age_severe_overall

Q3_age_severe_overall<-pool.med(age_distr_severe$Q3, age_distr_severe$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_age_severe_overall

#Plot age distribution
age_adult_severe_plot<- age_distr_severe %>% 
  mutate(study_sorted = fct_reorder(Study_nr, desc(Median))) %>%  # order studies in descending order of median age
  ggplot(aes(x = study_sorted, y = as.numeric(Median), ymin = Q1, ymax = Q3)) +
  geom_point(position = position_dodge(width = 0.2), aes(size = number_of_patients),shape=18) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  geom_hline(yintercept=median_age_severe_overall$pooled.est, size= 1, color="red", linetype='dotdash')+
  ggtitle("Severe")+
  scale_y_continuous(limits=c(0, 100), breaks=c(0, 25, 50, 75, 100))+
  coord_flip() +
  theme_bw()+
  ylab("Median Age [years]")+
  xlab("")+
  theme(axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.x= element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'))+  labs(size='Study Size')+
  guides(size = guide_legend(override.aes = list(size=c(1,2,3,4))))

age_adult_severe_plot


#-------------------------Age distribution of non-severe patients ------------------------------------------------------------------------------------------------------

# #Clear working space
# rm(list=ls())

#Load data
age_distr_nonsevere_nonsevere_non_nonsevere <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_demographics_of_severity_mortality_cohort.csv", sep =',', header = TRUE)

##Subset data for survivors
age_distr_nonsevere <- subset(age_distr_nonsevere_nonsevere_non_nonsevere, disease_status=='non_severe' & Median>0)

#Show names of colimns
names(age_distr_nonsevere)

#Calculate pooled median, Q1, and Q3 of all studies using sample size as weight
median_age_nonsevere_overall<-pool.med(age_distr_nonsevere$Median, age_distr_nonsevere$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_age_nonsevere_overall
class(median_age_nonsevere_overall$pooled.est)

Q1_age_nonsevere_overall<-pool.med(age_distr_nonsevere$Q1, age_distr_nonsevere$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_age_nonsevere_overall

Q3_age_nonsevere_overall<-pool.med(age_distr_nonsevere$Q3, age_distr_nonsevere$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_age_nonsevere_overall

#Plot age distribution
age_adult_nonsevere_plot<- age_distr_nonsevere %>% 
  mutate(study_sorted = fct_reorder(Study_nr, desc(Median))) %>%  # order studies in descending order of median age
  ggplot(aes(x = study_sorted, y = as.numeric(Median), ymin = Q1, ymax = Q3)) +
  geom_point(position = position_dodge(width = 0.2), aes(size = number_of_patients),shape=18) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  geom_hline(yintercept=median_age_nonsevere_overall$pooled.est, size= 1, color="red", linetype='dotdash')+
  ggtitle("Non-severe")+
  scale_y_continuous(limits=c(0, 100), breaks=c(0, 25, 50, 75, 100))+
  coord_flip() +
  theme_bw()+
  ylab("Median Age [years]")+
  xlab("")+
  theme(axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.x= element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'))+  labs(size='Study Size')+
  guides(size = guide_legend(override.aes = list(size=c(1,2,3,4))))

age_adult_nonsevere_plot

#---Pool figure---#
library("cowplot")
pooled_plot <-plot_grid(age_adult_nonsevere_plot, age_adult_severe_plot, 
               labels = c( "A", "B"),
               ncol = 2, nrow = 1)

pooled_plot

#-------------------------Age distribution of death patients ------------------------------------------------------------------------------------------------------
# #Clear working space
# rm(list=ls())

#Load data
age_distr_death_death_non_death <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_demographics_of_severity_mortality_cohort.csv", sep =',', header = TRUE)

##Subset data for non-survivors
age_distr_death <- subset(age_distr_death_death_non_death, disease_status=='death')

#Show names of colimns
names(age_distr_death)

#Calculate pooled median, Q1, and Q3 of all studies using sample size as weight
median_age_death_overall<-pool.med(age_distr_death$Median, age_distr_death$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_age_death_overall
class(median_age_death_overall$pooled.est)

Q1_age_death_overall<-pool.med(age_distr_death$Q1, age_distr_death$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_age_death_overall

Q3_age_death_overall<-pool.med(age_distr_death$Q3, age_distr_death$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_age_death_overall

#Plot age distribution
age_adult_death_plot<- age_distr_death %>% 
  mutate(study_sorted = fct_reorder(Study_nr, desc(Median))) %>%  # order studies in descending order of median age
  ggplot(aes(x = study_sorted, y = as.numeric(Median), ymin = Q1, ymax = Q3)) +
  geom_point(position = position_dodge(width = 0.2), aes(size = number_of_patients),shape=18) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  geom_hline(yintercept=median_age_death_overall$pooled.est, size= 1, color="red", linetype='dotdash')+
  ggtitle("Non-survivors")+
  scale_y_continuous(limits=c(0, 100), breaks=c(0, 25, 50, 75, 100))+
  coord_flip() +
  theme_bw()+
  ylab("Median Age [years]")+
  xlab("")+
  theme(axis.title.x = element_text(size=10),
        legend.position = "none",
        # legend.title = element_text(size=10, face='bold'),
        # legend.text = element_text(size=9),
        # legend.background = element_rect(fill = "#EFF2F4"),
        # legend.key=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'))+  labs(size='Study Size')+
  guides(size = guide_legend(override.aes = list(size=c(1,2,3,4))))

age_adult_death_plot


#-------------------------Age distribution of survivor patients ------------------------------------------------------------------------------------------------------
  
# #Clear working space
# rm(list=ls())

#Load data
age_distr_survivor_survivor_non_survivor <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_demographics_of_severity_mortality_cohort.csv", sep =',', header = TRUE)

##Subset data for survivors
age_distr_survivor <- subset(age_distr_survivor_survivor_non_survivor, disease_status=='survivor')

#Show names of colimns
names(age_distr_survivor)

#Calculate pooled median, Q1, and Q3 of all studies using sample size as weight
median_age_survivor_overall<-pool.med(age_distr_survivor$Median, age_distr_survivor$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_age_survivor_overall

Q1_age_survivor_overall<-pool.med(age_distr_survivor$Q1, age_distr_survivor$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_age_survivor_overall

Q3_age_survivor_overall<-pool.med(age_distr_survivor$Q3, age_distr_survivor$number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_age_survivor_overall

#Plot age distribution
age_adult_survivor_plot<- age_distr_survivor %>% 
  mutate(study_sorted = fct_reorder(Study_nr, desc(Median))) %>%  # order studies in descending order of median age
  ggplot(aes(x = study_sorted, y = as.numeric(Median), ymin = Q1, ymax = Q3)) +
  geom_point(position = position_dodge(width = 0.2), aes(size = number_of_patients),shape=18) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  geom_hline(yintercept=median_age_survivor_overall$pooled.est, size= 1, color="red", linetype='dotdash')+
  ggtitle("Survivors")+
  scale_y_continuous(limits=c(0, 100), breaks=c(0, 25, 50, 75, 100))+
  coord_flip() +
  theme_bw()+
  ylab("Median Age [years]")+
  xlab("")+
  theme(axis.title.x = element_text(size=10),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'))+  labs(size='Study Size')+
  guides(size = guide_legend(override.aes = list(size=c(1,2,3,4))))

age_adult_survivor_plot

#---Pool figure---#
library("cowplot")
pooled_plot_severity_mortality <-plot_grid(age_adult_nonsevere_plot, age_adult_severe_plot, age_adult_survivor_plot,age_adult_death_plot,
               labels = c( "A", "B", "C", "D"),
               ncol = 2, nrow = 2)

pooled_plot_severity_mortality


