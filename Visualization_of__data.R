#----------------R Code to create balloon plots of comorbidities, signs and symptoms,
#                 treatments, laboratory parameters, imaging features, and outcomes ----------------#
           
#Created by C. Jutzeler, April 9th, 2020

#clear working space
rm(list = ls())

#Install libraries if required
# if(!require(ggpubr)) install.packages("ggpubr")
# if(!require(httr)) install.packages("httr")
# if(!require(rvest)) install.packages("rvest")
# if(!require(ggplot2)) install.packages("ggplot2")
# if(!require(plyr)) install.packages("plyr")
# if(!require(dplyr)) install.packages("dplyr")
# if(!require(purrr)) install.packages("purrr")
# if(!require(jsonlite)) install.packages("jsonlite")
# if(!require(dplyr)) install.packages("dplyr")
# if(!require(lubridate)) install.packages("lubridate")
# if(!require(stringr)) install.packages("stringr")
# if(!require(viridis)) install.packages("viridis")
# if(!require(tidyr)) install.packages("tidyr")
# if(!require(gridExtra)) install.packages("gridExtra")
# if(!require(grid)) install.packages("grid")
# if(!require(lattice)) install.packages("lattice")

#load library
library(ggpubr)
library(httr)
library(rvest)
library(purrr)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(lubridate)
library(stringr)
library(viridis)
library(tidyr)
library(gridExtra)
library(grid)
library(lattice)

####----Data preparation-----####
#####----Data Visualization: Comorbidities in Adult COVID-19 Patients-----#####
#Clear working space
# rm(list=ls())

#load data
data_comorbidities_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_comorbidities_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: Adult, Cohort Studies and Case series, and reported comorbidities yes
data_comorbidities_adult <- subset(data_comorbidities_all, Study_Population =='Adult' & Comorbidties_reported == 'Yes')

#Remove all empty columns
emptycols <- sapply(data_comorbidities_adult, function (k) all(is.na(k)))
data_comorbidities_adult <- data_comorbidities_adult[!emptycols]
names(data_comorbidities_adult)

#Reformat data from wide to long
data_comorbidities_adult_long <- gather(data_comorbidities_adult, comorbidities, frequency, any_comorbidities:Heart_failure_congestive_or_chronic, factor_key=TRUE)

#count number of patients with different comorbidities
data_comorbidities_adult_long_count<- aggregate(as.numeric(data_comorbidities_adult_long2$frequency), by= list(comorbidities=data_comorbidities_adult_long2$comorbidities), FUN=sum, na.rm = TRUE)

# #Relable variables
levels(data_comorbidities_adult_long2$comorbidities) <- c("Any comorbidities","Hypertension", "Diabetes mellitus", "Carcinoma",'COPD',
                                                          "Cardiovascular disease", "Chronic kidney disease", 'Coronary heart disease', 'Liver disease',
                                                          'Cerebrovascular disease', 'Smoking', 'Hepatitis B', "Chronic liver disease",
                                                          'Respiratory system disease', 'Heart failure')   #'Immunedeficiency',"2nd pulmonary infection", 'Cardio-cerebrovascular diseas'


#----Plot the data: Faceted heat map of proportion of symptoms reported per study---#
comorbidities_adult_plot <-data_comorbidities_adult_long2%>%
  ggplot(aes(x=comorbidities,y=Study_nr, fill=percent))+
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion \n of patients\n", direction = -1, na.value = '#EFF2F4') +   
  ggtitle("Comorbidities") +
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text(face = 'regular'),
    legend.position = 'none'
  ) 

comorbidities_adult_plot

###Save as 3X8 inches

#####----Data Visualization: Comorbidities in Pregnant COVID-19 Patients-----#####
#Clear working space
#rm(list=ls())

#load data
data_comorbidities_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_comorbidities_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: pregnant, Cohort Studies and Case series, and reported comorbidities yes
data_comorbidities_pregnant <- subset(data_comorbidities_all, Study_Population =='Pregnant' & Comorbidties_reported == 'Yes')

#Remove all empty columns
emptycols <- sapply(data_comorbidities_pregnant, function (k) all(is.na(k)))
data_comorbidities_pregnant <- data_comorbidities_pregnant[!emptycols]
names(data_comorbidities_pregnant)

#Reformat data from wide to long
data_comorbidities_pregnant_long <- gather(data_comorbidities_pregnant, comorbidities, frequency, any_comorbidities:Chronic_pharyngitis, factor_key=TRUE)

# Calculate percentage of patients that reported certain symptoms
data_comorbidities_pregnant_long2 <- data_comorbidities_pregnant_long %>%
  group_by(Study_nr, comorbidities, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of symptoms/number of patients included in study

#----Plot the data: Faceted heat map of proportion of symptoms reported per study---#
comorbidities_pregnant_plot <-data_comorbidities_pregnant_long2%>%
  ggplot(aes(x=comorbidities,y=Study_nr, fill=percent))+
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion of Patients", direction = -1, na.value = '#EFF2F4') +   
  ggtitle(" ") +
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text(face = 'regular')
  ) 
 

comorbidities_pregnant_plot

#####----Data Visualization: Comorbidities in Pediatric and Neonatal COVID-19 Patients------#####
#Clear working spacee
rm(list=ls())

#load data
data_comorbidities_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_comorbidities.csv", sep =',', header = TRUE)

#Subset data: pediatric, Cohort Studies and Case series, and reported comorbidities yes
data_comorbidities_pediatric <- subset(data_comorbidities_all, (Study_Population =='Pediatric' | Study_Population =='Neonatal') & Comorbidties_reported == 'Yes')

#Remove all empty columns
emptycols <- sapply(data_comorbidities_pediatric, function (k) all(is.na(k)))
data_comorbidities_pediatric <- data_comorbidities_pediatric[!emptycols]
names(data_comorbidities_pediatric)

#Reformat data from wide to long
data_comorbidities_pediatric_long <- gather(data_comorbidities_pediatric, comorbidities, frequency, any_comorbidities:Hypertension, factor_key=TRUE)

# Calculate percentage of patients that reported certain symptoms
data_comorbidities_pediatric_long2 <- data_comorbidities_pediatric_long %>%
  group_by(Study_nr, comorbidities, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of symptoms/number of patients included in study

#----Plot the data: Faceted heat map of proportion of symptoms reported per study---#
comorbidities_pediatric_plot <-data_comorbidities_pediatric_long2%>%
  ggplot(aes(x=comorbidities,y=Study_nr, fill=percent))+
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion of Patients", direction = -1, na.value = 'white') +   
  ggtitle(" ") +
  theme_grey(base_size=8)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(face="bold"),
    axis.ticks=element_line(size=0.4),
    plot.background=element_blank(),
    strip.background = element_blank(), 
    strip.text.x = element_text(face = 'bold'),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3)
  ) 

comorbidities_pediatric_plot



#####----Data Visualization: Symptoms of Adult COVID-19 Patients-----####
#clear working space
#rm(list=ls())

#load data
data_symptoms_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_symptoms_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: Adult patients and reported symptoms: yes
data_symptoms_adult <- subset(data_symptoms_all, Study_Population =='Adult' & symptoms_reported == 'Yes')

#Remove all empty columns
emptycols <- sapply(data_symptoms_adult, function (k) all(is.na(k)))
data_symptoms_adult <- data_symptoms_adult[!emptycols]
names(data_symptoms_adult)

#Reformat data from wide to long
data_symptoms_adult_long <- gather(data_symptoms_adult, symptoms, frequency, Fever:Dizziness_or_confusion, factor_key=TRUE)

#Calculate percentage of patients that reported certain symptoms
data_symptoms_adult_long2 <- data_symptoms_adult_long %>%
  group_by(Study_nr, symptoms, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of symptoms/number of patients included in study

###Plot the data: Faceted heat map of proportion of symptoms reported per study
symptoms_adult_plot <-data_symptoms_adult_long2%>%
  ggplot(aes(x=symptoms,y=Study_nr, fill=percent))+
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion \n of patients\n", direction = -1, na.value = '#EFF2F4') +   
  ggtitle(" ") +
  ggtitle('Clinical symptoms of adult COVID-19 patients')+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text(face = 'regular'),
    legend.position = 'none'
  ) 
symptoms_adult_plot

#####----Data Visualization: Symptoms of Pregnant COVID-19 Patients-----####
#Clear working space
rm(list=ls())

#load data
data_symptoms_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_symptoms_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: pregnant women, reported symptoms: yes
data_symptoms_pregnant <- subset(data_symptoms_all, (Study_Population =='Pregnant') & symptoms_reported == 'Yes')

#Remove all empty columns
emptycols <- sapply(data_symptoms_pregnant, function (k) all(is.na(k)))
data_symptoms_pregnant <- data_symptoms_pregnant[!emptycols]
names(data_symptoms_pregnant)

#Reformat data from wide to long
data_symptoms_pregnant_long <- gather(data_symptoms_pregnant, symptoms, frequency, Fever:Chest_pain, factor_key=TRUE)

# Calculate percentage of patients that reported certain symptoms
data_symptoms_pregnant_long2 <- data_symptoms_pregnant_long %>%
  group_by(Study_nr, symptoms, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of symptoms/number of patients included in study

###Plot the data: Faceted heat map of proportion of symptoms reported per study
symptoms_pregnant_plot <-data_symptoms_pregnant_long2%>%
  ggplot(aes(x=symptoms,y=Study_nr, fill=percent))+
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
    scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion \n of Patients", direction = -1, na.value = '#EFF2F4') +   
  ggtitle('Clinical sign and symptoms in pregnant COVID-19 patients')+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text(face = 'regular')
      ) 

symptoms_pregnant_plot



#####----Data Visualization: Symptoms of Pediatric and Neonatal COVID-19 Patients-----####
#Clear working space
rm(list=ls())

#load data
data_symptoms_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_symptoms_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: Children and reported symptoms yes
data_symptoms_children <- subset(data_symptoms_all, (Study_Population =='Pediatric' | Study_Population =='Neonatal') & symptoms_reported == 'Yes')
names(data_symptoms_children)

#Remove all empty columns
emptycols <- sapply(data_symptoms_children, function (k) all(is.na(k)))
data_symptoms_children <- data_symptoms_children[!emptycols]
names(data_symptoms_children)

#Reformat data from wide to long
data_symptoms_children_long <- gather(data_symptoms_children, symptoms, frequency, Fever:Constipation, factor_key=TRUE)

# Calculate percentage of patients that reported certain symptoms
data_symptoms_children_long2 <- data_symptoms_children_long %>%
  group_by(Study_nr, symptoms, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of symptoms/number of patients included in study

###Plot the data: Faceted heat map of proportion of symptoms reported per study
symptoms_children_plot <-data_symptoms_children_long2%>%
  ggplot(aes(x=symptoms,y=Study_nr, fill=percent))+
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion \n of Patients", direction = -1, na.value = '#EFF2F4') +   
  theme_grey(base_size=10)+
  ggtitle('Clinical signs and symptoms in pediatric and neonatal COVID-19 patients')+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text(face = 'regular')
  ) 

symptoms_children_plot

####----Data Visualization: Treatment of Adult COVID-19 Patients-----#####
#Clear working space
#rm(list=ls())

#load data
data_treatment_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_treatment_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: Adult, Cohort Studies and Case series, and reported treatment yes
data_treatment_adult <- subset(data_treatment_all, Study_Population =='Adult' & Medication_reported == 'Yes')
names(data_treatment_adult)

#Remove all empty columns
emptycols <- sapply(data_treatment_adult, function (k) all(is.na(k)))
data_treatment_adult <- data_treatment_adult[!emptycols]
names(data_treatment_adult)

#Reformat data from wide to long
data_treatment_adult_long <- gather(data_treatment_adult, treatment, frequency, Antibiotics_n:Alpha_interferon_aerosol_inhalation, factor_key=TRUE)

# Calculate percentage of patients that reported certain treatment
data_treatment_adult_long2 <- data_treatment_adult_long %>%
  group_by(Study_nr, treatment, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of symptoms/number of patients included in study

#Add Variable 'Treatment Type'
data_treatment_adult_long2$Treatment_type <- "Other" #default is 'other'

#Change treatment type of all antibiotics
data_treatment_adult_long2[data_treatment_adult_long2$treatment %in% c("Antibiotics_n",	"Carbapenem_linezolid",	"Amoxicilin",
                                                                       "Cefaclor", "Cefazolin", "Levofloxacin",	"Azithromycin",
                                                                       "Ornidozole",	"Ceftazidime",	"Cefotiam_hydrochloride",
                                                                       "Piperacillin_tazobactam",	"Teicoplanin",	"Moxifloxacin_hydrochloride",
                                                                       "Vancomycin",	"Cefepime",	"Ceftriaxone",	"Clavulnate",	"Sulbactam"),]$Treatment_type <- "Antibiotics"
#Change treatment type of all antivirals
data_treatment_adult_long2[data_treatment_adult_long2$treatment %in% c("Antiviral_treatment",	"Oseltamivir",	"virazole",	"Veletonavir",	"Lopinavir",	"Ritonavir",	"Ribavirin",
                                                                       "Remdesivir",	"Oseltamivir",	"ganciclovir"),]$Treatment_type <- "Antivirals"


#Change treatment type of all ventilation
data_treatment_adult_long2[data_treatment_adult_long2$treatment %in% c("Oxygen_therapy",	"High_flow_nasal_cannula",	"All_mechanical_ventilation_together",
                                                                       "All_mechanical_ventilation_together", "Non_invasive_mechanical_ventilation","Invasive_mechanical_ventilation",	
                                                                       "ECMO"),]$Treatment_type <- "bOxygen treatment"


#----Plot the data: Faceted heat map of proportion of symptoms reported per study---#
treatment_adult_plot <-data_treatment_adult_long2%>%
  ggplot(aes(x=treatment,y=Study_nr, fill=percent))+
  facet_grid(~ Treatment_type, scales='free_x', space="free_x") +  
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion", direction = -1, na.value = '#EFF2F4') +   
  ggtitle('Treatments')+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text()
  ) 


treatment_adult_plot

####----Data Visualization: Treatment of Pregnant COVID-19 Patients-----#####

#Clear working space
rm(list=ls())

#load data
data_treatment_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_treatment_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: pregnant, Cohort Studies and Case series, and reported treatment yes
data_treatment_pregnant <- subset(data_treatment_all, Study_Population =='Pregnant' & Medication_reported == 'Yes')
names(data_treatment_pregnant)

#Remove all empty columns
emptycols <- sapply(data_treatment_pregnant, function (k) all(is.na(k)))
data_treatment_pregnant <- data_treatment_pregnant[!emptycols]
names(data_treatment_pregnant)

#Reformat data from wide to long
data_treatment_pregnant_long <- gather(data_treatment_pregnant, treatment, frequency, Antibiotics_n:Alpha_interferon_aerosol_inhalation, factor_key=TRUE)

# Calculate percentage of patients that reported certain treatment
data_treatment_pregnant_long2 <- data_treatment_pregnant_long %>%
  group_by(Study_nr, treatment, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of symptoms/number of patients included in study

#Add Variable 'Treatment Type'
data_treatment_pregnant_long2$Treatment_type <- "Other" #default is 'other'

#Change treatment type of all antibiotics
data_treatment_pregnant_long2[data_treatment_pregnant_long2$treatment %in% c("Antibiotics_n",	"Carbapenem_linezolid",	"Amoxicilin",
                                                                       "Cefaclor", "Cefazolin", "Levofloxacin",	"Azithromycin",
                                                                       "Ornidozole",	"Ceftazidime",	"Cefotiam_hydrochloride",
                                                                       "Piperacillin_tazobactam",	"Teicoplanin",	"Moxifloxacin_hydrochloride",
                                                                       "Vancomycin",	"Cefepime",	"Ceftriaxone",	"Clavulnate",	"Sulbactam"),]$Treatment_type <- "Antibiotics"
#Change treatment type of all antivirals
data_treatment_pregnant_long2[data_treatment_pregnant_long2$treatment %in% c("Antiviral_treatment",	"Oseltamivir",	"virazole",	"Veletonavir",	"Lopinavir",	"Ritonavir",	"Ribavirin",
                                                                       "Remdesivir",	"Oseltamivir",	"ganciclovir"),]$Treatment_type <- "Antivirals"


#Change treatment type of all ventilation
data_treatment_pregnant_long2[data_treatment_pregnant_long2$treatment %in% c("Oxygen_therapy",	"High_flow_nasal_cannula",	"All_mechanical_ventilation_together",
                                                                       "All_mechanical_ventilation_together", "Non_invasive_mechanical_ventilation","Invasive_mechanical_ventilation",	
                                                                       "ECMO"),]$Treatment_type <- "bOxygen treatment"



#----Plot the data: Faceted heat map of proportion of symptoms reported per study---#
treatment_pregnant_plot <-data_treatment_pregnant_long2%>%
  ggplot(aes(x=treatment,y=Study_nr, fill=percent))+
  facet_grid(~ Treatment_type, scales='free_x', space="free_x") +   
  
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion \n of Patients", direction = -1, na.value = '#EFF2F4') +   
  ggtitle('Treatments administered to pregnant COVID-19 patients')+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text()
  ) 


treatment_pregnant_plot

####----Data Visualization: Treatment of Pediatric and Neonatal COVID-19 Patients-----#####
#Clear working space
rm(list=ls())

#load data
data_treatment_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_treatment_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: children, Cohort Studies and Case series, and reported treatment yes
data_treatment_children <- subset(data_treatment_all, (Study_Population =='Pediatric' | Study_Population =='Neonatal')  & Medication_reported == 'Yes')
names(data_treatment_children)

#Remove all empty columns
emptycols <- sapply(data_treatment_children, function (k) all(is.na(k)))
data_treatment_children <- data_treatment_children[!emptycols]
names(data_treatment_children)

#Reformat data from wide to long
data_treatment_children_long <- gather(data_treatment_children, treatment, frequency, Antibiotics_n:Alpha_interferon_aerosol_inhalation, factor_key=TRUE)

# Calculate percentage of patients that reported certain treatment
data_treatment_children_long2 <- data_treatment_children_long %>%
  group_by(Study_nr, treatment, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of symptoms/number of patients included in study

#Add Variable 'Treatment Type'
data_treatment_children_long2$Treatment_type <- "Other" #default is 'other'

#Change treatment type of all antibiotics
data_treatment_children_long2[data_treatment_children_long2$treatment %in% c("Antibiotics_n",	"Carbapenem_linezolid",	"Amoxicilin",
                                                                             "Cefaclor", "Cefazolin", "Levofloxacin",	"Azithromycin",
                                                                             "Ornidozole",	"Ceftazidime",	"Cefotiam_hydrochloride",
                                                                             "Piperacillin_tazobactam",	"Teicoplanin",	"Moxifloxacin_hydrochloride",
                                                                             "Vancomycin",	"Cefepime",	"Ceftriaxone",	"Clavulnate",	"Sulbactam"),]$Treatment_type <- "Antibiotics"
#Change treatment type of all antivirals
data_treatment_children_long2[data_treatment_children_long2$treatment %in% c("Antiviral_treatment",	"Oseltamivir",	"virazole",	"Veletonavir",	"Lopinavir",	"Ritonavir",	"Ribavirin",
                                                                             "Remdesivir",	"Oseltamivir",	"ganciclovir"),]$Treatment_type <- "Antivirals"


#Change treatment type of all ventilation
data_treatment_children_long2[data_treatment_children_long2$treatment %in% c("Oxygen_therapy",	"High_flow_nasal_cannula",	"All_mechanical_ventilation_together",
                                                                             "All_mechanical_ventilation_together", "Non_invasive_mechanical_ventilation","Invasive_mechanical_ventilation",	
                                                                             "ECMO"),]$Treatment_type <- "bOxygen treatment"


#----Plot the data: Faceted heat map of proportion of symptoms reported per study---#
treatment_children_plot <-data_treatment_children_long2%>%
  ggplot(aes(x=treatment,y=Study_nr, fill=percent))+
  facet_grid(~ Treatment_type, scales='free_x', space="free_x") + 
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion \n of Patients", direction = -1, na.value = '#EFF2F4') +   
  ggtitle('Treatment administered in pediatric and neonatal COVID-19 patients')+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text()
  ) 

treatment_children_plot

#####----Data Visualization: Pooled Graph TREATMENT-----#####
grid.arrange(treatment_adult_plot,
             treatment_pregnant_plot,
             treatment_children_plot,
             heights=c(0.8,0.3,0.4),
             ncol=1)

#####----Data Visualization: Pooled Graph Adults-----#####
######Combine figures for adults: Comorbidities, clinical symptoms, imaging fetures, laboratory, treatment

#---Pool figure---#
library("cowplot")
p1 = plot_grid(comorbidities_adult_plot, symptoms_adult_plot, outcome_adult_plot,
          labels = c( "A", "B", "C"),
                 ncol = 3, nrow = 1)

plot_grid( p1, treatment_adult_plot,
          labels = c( "", "D"),
          # rel_widths = c(1.5, 1),
          ncol = 1, nrow = 2)

#####----Data Visualization: Imaging features of Adult COVID-19 Patients-----####
#Select only rows that contain information on imaging
#load data
data_imaging_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_imaging_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: Adult patients and reported imaging: yes
data_imaging_adult <- subset(data_imaging_all, Study_Population =='Adult' & imaging_reported == 'Yes')

#Remove all empty columns
emptycols <- sapply(data_imaging_adult, function (k) all(is.na(k)))
data_imaging_adult <- data_imaging_adult[!emptycols]

#Reformat data from wide to long
data_imaging_adult_long <- gather(data_imaging_adult, imaging, frequency, pathologic_findings:Atelectasis, factor_key=TRUE)


# Calculate percentage of patients that reported certain imaging
data_imaging_adult_long2 <- data_imaging_adult_long %>%
  group_by(Study_nr, imaging, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of imaging/number of patients included in study

###Plot the data: Faceted heat map of proportion of imaging reported per study
imaging_adult_plot <-data_imaging_adult_long2%>%
  ggplot(aes(x=imaging,y=Study_nr, fill=percent))+
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion \n of Patients", direction = -1, na.value = '#EFF2F4') +   
  ggtitle('Imaging features of adult COVID-19 patients')+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text()
  ) 

imaging_adult_plot

#####----Data Visualization: Imaging features of Pregnant COVID-19 Patients-----####
#Select only rows that contain information on imaging
#load data
data_imaging_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_imaging_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: pregnant patients and reported imaging: yes
data_imaging_pregnant <- subset(data_imaging_all, Study_Population =='Pregnant' & imaging_reported == 'Yes')
names(data_imaging_pregnant)

#Remove all empty columns
emptycols <- sapply(data_imaging_pregnant, function (k) all(is.na(k)))
data_imaging_pregnant <- data_imaging_pregnant[!emptycols]

#Reformat data from wide to long
data_imaging_pregnant_long <- gather(data_imaging_pregnant, imaging, frequency, pathologic_findings:Pleural_effusion, factor_key=TRUE)


# Calculate percentage of patients that reported certain imaging
data_imaging_pregnant_long2 <- data_imaging_pregnant_long %>%
  group_by(Study_nr, imaging, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of imaging/number of patients included in study

###Plot the data: Faceted heat map of proportion of imaging reported per study
imaging_pregnant_plot <-data_imaging_pregnant_long2%>%
  ggplot(aes(x=imaging,y=Study_nr, fill=percent))+
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion \n of Patients", direction = -1, na.value = '#EFF2F4') +   
   ggtitle("Imaging features of pregnant COVID-19 patients")+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text()
  ) 

imaging_pregnant_plot


#####----Data Visualization: Imaging features of Pediatric and Neonatal COVID-19 Patients-----####
#Select only rows that contain information on imaging
#load data
data_imaging_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_imaging_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: pediatric patients and reported imaging: yes
data_imaging_pediatric <- subset(data_imaging_all, (Study_Population =='Pediatric'| Study_Population =='Neonatal') & imaging_reported == 'Yes')
names(data_imaging_pediatric)

#Remove all empty columns
emptycols <- sapply(data_imaging_pediatric, function (k) all(is.na(k)))
data_imaging_pediatric <- data_imaging_pediatric[!emptycols]

#Reformat data from wide to long
data_imaging_pediatric_long <- gather(data_imaging_pediatric, imaging, frequency, pathologic_findings:Bulla, factor_key=TRUE)

# Calculate percentage of patients that reported certain imaging
data_imaging_pediatric_long2 <- data_imaging_pediatric_long %>%
  group_by(Study_nr, imaging, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of imaging/number of patients included in study

###Plot the data: Faceted heat map of proportion of imaging reported per study
imaging_pediatric_plot <-data_imaging_pediatric_long2%>%
  ggplot(aes(x=imaging,y=Study_nr, fill=percent))+
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion \n of Patients", direction = -1, na.value = '#EFF2F4') +   
  ggtitle('Imaging features of pediatric and neonatal COVID-19 patients')+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text()
  )

imaging_pediatric_plot


#####----Data Visualization: Laboratory features of Adult COVID-19 Patients-----####
#Clear working space
rm(list=ls())

#load data
data_laboratory_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_labor_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: Adult patients and reported laboratory: yes
data_laboratory_adult <- subset(data_laboratory_all, Study_Population =='Adult' & IL6_Median >=0)
names(data_laboratory_adult)

#Remove all empty columns
emptycols <- sapply(data_laboratory_adult, function (k) all(is.na(k)))
data_laboratory_adult <- data_laboratory_adult[!emptycols]
names(data_laboratory_adult)

#Calculate pooled median of all studies using sample size as weight
median_IL6_adult_overall<-pool.med(data_laboratory_adult$IL6_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_IL6_adult_overall
class(median_IL6_adult_overall$pooled.est)


#Plot age distribution
IL6_plot <- data_laboratory_adult%>% 
  mutate(study_sorted = fct_reorder(PMID, desc(IL6_Median))) %>%  # order studies in descending order of median age
  ggplot(aes(x = study_sorted, y = as.numeric(IL6_Median), ymin = IL6_Q1, ymax = IL6_Q3)) +
  geom_point(position = position_dodge(width = 0.2), aes(size = Total_number_of_patients), shape=16) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  geom_hline(yintercept=median_IL6_adult_overall$pooled.est, size= 1, color="red", linetype='dotdash')+
  ggtitle("IL6")+
  coord_flip() +
  theme_bw()+
  ylab("")+
  xlab("")+
  theme(axis.title.x = element_text(size=10),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
        panel.background = element_rect(fill = "#EFF2F4"),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'))+  labs(size='Study Size')+
  guides(size = guide_legend(override.aes = list(size=c(1,2,3))))


IL6_plot

#####----Data Visualization: outcome features of Adult COVID-19 Patients-----####
#Clear working space
#rm(list=ls())

#load data
data_outcome_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_outcomes_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: Adult patients and reported outcome: yes
data_outcome_adult <- subset(data_outcome_all, Study_Population =='Adult' & outcome_reported == 'Yes')
names(data_outcome_adult)

#Remove all empty columns
emptycols <- sapply(data_outcome_adult, function (k) all(is.na(k)))
data_outcome_adult <- data_outcome_adult[!emptycols]
names(data_outcome_adult)

#Reformat data from wide to long
data_outcome_adult_long <- gather(data_outcome_adult, outcome, frequency, Death:Heart.Failure, factor_key=TRUE)


# Calculate percentage of patients that reported certain outcome
data_outcome_adult_long2 <- data_outcome_adult_long %>%
  group_by(Study_nr, outcome, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of outcome/number of patients included in study

###Plot the data: Faceted heat map of proportion of outcome reported per study
outcome_adult_plot <-data_outcome_adult_long2%>%
  ggplot(aes(x=outcome,y=Study_nr, fill=percent))+
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion", direction = -1, na.value = '#EFF2F4') +   
  ggtitle('Outcome of adult COVID-19 patients')+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text(),
    legend.position = "none"
  )


outcome_adult_plot


#####----Data Visualization: outcome features of pregnant COVID-19 patients-----####
#Clear working space
rm(list=ls())

#load data
data_outcome_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_outcomes_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: pregnant patients and reported outcome: yes
data_outcome_pregnant <- subset(data_outcome_all, Study_Population =='Pregnant' & outcome_reported == 'Yes')
names(data_outcome_pregnant)

#Remove all empty columns
emptycols <- sapply(data_outcome_pregnant, function (k) all(is.na(k)))
data_outcome_pregnant <- data_outcome_pregnant[!emptycols]
names(data_outcome_pregnant)

#Reformat data from wide to long
data_outcome_pregnant_long <- gather(data_outcome_pregnant, outcome, frequency, Death:ICU.Admission, factor_key=TRUE)

# Calculate percentage of patients that reported certain outcome
data_outcome_pregnant_long2 <- data_outcome_pregnant_long %>%
  group_by(Study_nr, outcome, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of outcome/number of patients included in study

###Plot the data: Faceted heat map of proportion of outcome reported per study
outcome_pregnant_plot <-data_outcome_pregnant_long2%>%
  ggplot(aes(x=outcome,y=Study_nr, fill=percent))+
  geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion \n of Patients", direction = -1, na.value = 'white') +   
  ggtitle('Outcome of pregnant COVID-19 patients')+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text()
  )

outcome_pregnant_plot

#####----Data Visualization: outcome features of pediatric COVID-19 patients-----####
#Clear working space
rm(list=ls())

#load data
data_outcome_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_outcomes_all_patients_final.csv", sep =',', header = TRUE)

#Subset data: children patients and reported outcome: yes
data_outcome_children <- subset(data_outcome_all, (Study_Population =='Pediatric'|Study_Population =='Neonatal')  & complications_reported == 'Yes')
names(data_outcome_children)

#Remove all empty columns
emptycols <- sapply(data_outcome_children, function (k) all(is.na(k)))
data_outcome_children <- data_outcome_children[!emptycols]
names(data_outcome_children)

#Reformat data from wide to long
data_outcome_children_long <- gather(data_outcome_children, outcome, frequency, Septic.shock:Acute.renal.injury, factor_key=TRUE)

# Calculate percentage of patients that reported certain outcome
data_outcome_children_long2 <- data_outcome_children_long %>%
  group_by(Study_nr, outcome, as.numeric(frequency)) %>% 
  mutate(percent = as.numeric(frequency) / sum(as.numeric(Total_number_of_patients)) * 100)  #Number of outcome/number of patients included in study

###Plot the data: Faceted heat map of proportion of outcome reported per study
outcome_children_plot <-data_outcome_children_long2%>%
  ggplot(aes(x=outcome,y=Study_nr, fill=percent))+
   geom_tile(colour="grey",size=0.25)+
  labs(x="",y="") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_viridis(option = "C", name = "Proportion \n of Patients", direction = -1, na.value = '#EFF2F4') +   
  ggtitle('Outcome of pediatric and neonatal COVID-19 patients')+
  theme_grey(base_size=12)+
  theme(
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.2),
    axis.text=element_text(),
    axis.ticks=element_line(size=0.4),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=14, face='bold'),
    legend.key=element_blank(),
    legend.background =  element_rect(fill = "#EFF2F4"),
    legend.title = element_text(size=10),
    panel.background = element_rect(fill = "#EFF2F4"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    plot.background = element_rect(fill = "#EFF2F4"),
    strip.background = element_blank(), 
    strip.text.x = element_text()
  )

outcome_children_plot








