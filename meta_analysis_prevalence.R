#Clear working space
rm(list=ls())

#load data
data_comorbidities_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_comorbidities.csv", sep =',', header = TRUE)

#####----Comorbidities in adult CoVID-19 patients----####

#Subset data: Adult, Cohort Studies and Case series, and reported comorbidities yes
data_comorbidities_adult <- subset(data_comorbidities_all, Study_Population =='Adult' & Comorbidties_reported == 'Yes')

#count number of adult patients
sum_comorbidities_adults <- sum(data_comorbidities_adult$Total_number_of_patients)
sum_comorbidities_adults

#Remove all empty columns
emptycols <- sapply(data_comorbidities_adult, function (k) all(is.na(k)))
data_comorbidities_adult <- data_comorbidities_adult[!emptycols]
names(data_comorbidities_adult)

#Reformat data from wide to long
data_comorbidities_adult_long <- gather(data_comorbidities_adult, comorbidities, frequency, any_comorbidities:Organ_transplant, factor_key=TRUE)

freq <- as.numeric(data_comorbidities_adult_long$frequency)

#Run meta analysis for all comorbidities listed in 'data_comorbidities_adult'

library(meta)
mtprop <- metaprop(event=as.numeric(frequency), n=Total_number_of_patients, studlab=Study_nr, data=subset(data_comorbidities_adult_long, data_comorbidities_adult_long$comorbidities=='Coronary_Heart_Disease'  ##Replace with comorbidity of interest 
                                                                                                          & data_comorbidities_adult_long$frequency>=0))
#Number of patients with comorbidity of interest
sum(mtprop$event)
#Total number of patients
sum(mtprop$n)

#print results from the meta-analysis
print(mtprop)

#Plot results from the meta-analysis using a Forest plot
forest(mtprop, xlim = c(-10,100), pscale = 100, 
       comb.fixed = F,
       comb.random = mtprop$comb.random,
       leftcols = c("studlab", "event",  "n"),
       rightcols =           c( "effect", "ci"), 
     leftlabs = c("Study", "Number", "Total"), 
     rightlabs=c("Prevalence (%)", "95% CI"),
       xlab = "Prevalence (%)", addspace = TRUE, digits = 1,
       squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2",type.random = "diamond")

#####----symptoms in adult CoVID-19 patients----####

data_symptoms_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_symptoms.csv", sep =',', header = TRUE)


#Subset data: Adult, Cohort Studies and Case series, and reported symptoms yes
data_symptoms_adult <- subset(data_symptoms_all, Study_Population =='Adult' & symptoms_reported == 'Yes')

#count number of adult patients
sum_symptoms_adults <- sum(data_symptoms_adult$Total_number_of_patients)
sum_symptoms_adults

#Remove all empty columns
emptycols <- sapply(data_symptoms_adult, function (k) all(is.na(k)))
data_symptoms_adult <- data_symptoms_adult[!emptycols]
names(data_symptoms_adult)

#Reformat data from wide to long
data_symptoms_adult_long <- gather(data_symptoms_adult, symptoms, frequency, number_of_asymptomatic:Belching, factor_key=TRUE)

freq <- as.numeric(data_symptoms_adult_long$frequency)

#Run meta analysis for all symptoms listed in 'data_symptoms_adult'

library(meta)
mtprop <- metaprop(event=as.numeric(frequency), n=Total_number_of_patients, studlab=Study_nr, data=subset(data_symptoms_adult_long, data_symptoms_adult_long$symptoms=='Fever'  ##Replace with comorbidity of interest 
                                                                                                          & data_symptoms_adult_long$frequency>=0))
#Number of patients with comorbidity of interest
sum(mtprop$event)
#Total number of patients
sum(mtprop$n)

#print results from the meta-analysis
print(mtprop)

#Plot results from the meta-analysis using a Forest plot
forest(mtprop, xlim = c(-10,100), pscale = 100, 
       comb.fixed = F,
       comb.random = mtprop$comb.random,
       leftcols = c("studlab", "event",  "n"),
       rightcols =           c( "effect", "ci"), 
       leftlabs = c("Study", "Number", "Total"), 
       rightlabs=c("Prevalence (%)", "95% CI"),
       xlab = "Prevalence (%)", addspace = TRUE, digits = 1,
       squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2",type.random = "diamond")


#####----symptoms in pregnant CoVID-19 patients----####

data_symptoms_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_symptoms.csv", sep =',', header = TRUE)


#Subset data: pregnant, Cohort Studies and Case series, and reported symptoms yes
data_symptoms_pregnant <- subset(data_symptoms_all, Study_Population =='Pregnant' & symptoms_reported == 'Yes')

#count number of pregnant patients
sum_symptoms_pregnants <- sum(data_symptoms_pregnant$Total_number_of_patients)
sum_symptoms_pregnants

#Remove all empty columns
emptycols <- sapply(data_symptoms_pregnant, function (k) all(is.na(k)))
data_symptoms_pregnant <- data_symptoms_pregnant[!emptycols]
names(data_symptoms_pregnant)

#Reformat data from wide to long
data_symptoms_pregnant_long <- gather(data_symptoms_pregnant, symptoms, frequency, number_of_asymptomatic:Chest_pain, factor_key=TRUE)

freq <- as.numeric(data_symptoms_pregnant_long$frequency)

#Run meta analysis for all symptoms listed in 'data_symptoms_pregnant'

library(meta)
mtprop <- metaprop(event=as.numeric(frequency), n=Total_number_of_patients, studlab=Study_nr, data=subset(data_symptoms_pregnant_long, data_symptoms_pregnant_long$symptoms=='Myalgia'  ##Replace with comorbidity of interest 
                                                                                                          & data_symptoms_pregnant_long$frequency>=0))
#Number of patients with comorbidity of interest
sum(mtprop$event)
#Total number of patients
sum(mtprop$n)

#print results from the meta-analysis
print(mtprop)

#Plot results from the meta-analysis using a Forest plot
forest(mtprop, xlim = c(-10,100), pscale = 100, 
       comb.fixed = F,
       comb.random = mtprop$comb.random,
       leftcols = c("studlab", "event",  "n"),
       rightcols =           c( "effect", "ci"), 
       leftlabs = c("Study", "Number", "Total"), 
       rightlabs=c("Prevalence (%)", "95% CI"),
       xlab = "Prevalence (%)", addspace = TRUE, digits = 1,
       squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2",type.random = "diamond")

#####----symptoms in children CoVID-19 patients----####

data_symptoms_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_symptoms.csv", sep =',', header = TRUE)


#Subset data: children, Cohort Studies and Case series, and reported symptoms yes
data_symptoms_children <- subset(data_symptoms_all, Study_Population =='Pediatric' & symptoms_reported == 'Yes')

#count number of children patients
sum_symptoms_childrens <- sum(data_symptoms_children$Total_number_of_patients)
sum_symptoms_childrens

#Remove all empty columns
emptycols <- sapply(data_symptoms_children, function (k) all(is.na(k)))
data_symptoms_children <- data_symptoms_children[!emptycols]
names(data_symptoms_children)

#Reformat data from wide to long
data_symptoms_children_long <- gather(data_symptoms_children, symptoms, frequency, number_of_asymptomatic:Nasal_congestion, factor_key=TRUE)

freq <- as.numeric(data_symptoms_children_long$frequency)

#Run meta analysis for all symptoms listed in 'data_symptoms_children'

library(meta)
mtprop <- metaprop(event=as.numeric(frequency), n=Total_number_of_patients, studlab=Study_nr, data=subset(data_symptoms_children_long, data_symptoms_children_long$symptoms=='Fever'  ##Replace with comorbidity of interest 
                                                                                                          & data_symptoms_children_long$frequency>=0))
#Number of patients with comorbidity of interest
sum(mtprop$event)
#Total number of patients
sum(mtprop$n)

#print results from the meta-analysis
print(mtprop)

#Plot results from the meta-analysis using a Forest plot
forest(mtprop, xlim = c(-10,100), pscale = 100, 
       comb.fixed = F,
       comb.random = mtprop$comb.random,
       leftcols = c("studlab", "event",  "n"),
       rightcols =           c( "effect", "ci"), 
       leftlabs = c("Study", "Number", "Total"), 
       rightlabs=c("Prevalence (%)", "95% CI"),
       xlab = "Prevalence (%)", addspace = TRUE, digits = 1,
       squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2",type.random = "diamond")


#####----imaging in adult CoVID-19 patients----####

data_imaging_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_imaging.csv", sep =',', header = TRUE)


#Subset data: Adult, Cohort Studies and Case series, and reported imaging yes
data_imaging_adult <- subset(data_imaging_all, Study_Population =='Adult' & imaging_reported == 'Yes')

#count number of adult patients
sum_imaging_adults <- sum(data_imaging_adult$Total_number_of_patients)
sum_imaging_adults

#Remove all empty columns
emptycols <- sapply(data_imaging_adult, function (k) all(is.na(k)))
data_imaging_adult <- data_imaging_adult[!emptycols]
names(data_imaging_adult)

#Reformat data from wide to long
data_imaging_adult_long <- gather(data_imaging_adult, imaging, frequency, pathologic_findings:Crazy_paving_pattern, factor_key=TRUE)

freq <- as.numeric(data_imaging_adult_long$frequency)

#Run meta analysis for all imaging listed in 'data_imaging_adult'

library(meta)
mtprop <- metaprop(event=as.numeric(frequency), n=Total_number_of_patients, studlab=Study_nr, data=subset(data_imaging_adult_long, data_imaging_adult_long$imaging=='Crazy_paving_pattern'  ##Replace with comorbidity of interest 
                                                                                                          & data_imaging_adult_long$frequency>=0))
#Number of patients with comorbidity of interest
sum(mtprop$event)
#Total number of patients
sum(mtprop$n)

#print results from the meta-analysis
print(mtprop)

#Plot results from the meta-analysis using a Forest plot
forest(mtprop, xlim = c(-10,100), pscale = 100, 
       comb.fixed = F,
       comb.random = mtprop$comb.random,
       leftcols = c("studlab", "event",  "n"),
       rightcols =           c( "effect", "ci"), 
       leftlabs = c("Study", "Number", "Total"), 
       rightlabs=c("Prevalence (%)", "95% CI"),
       xlab = "Prevalence (%)", addspace = TRUE, digits = 1,
       squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2",type.random = "diamond")


#####----imaging in pregnant CoVID-19 patients----####

data_imaging_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_imaging.csv", sep =',', header = TRUE)


#Subset data: pregnant, Cohort Studies and Case series, and reported imaging yes
data_imaging_pregnant <- subset(data_imaging_all, Study_Population =='Pregnant' & imaging_reported == 'Yes')

#count number of pregnant patients
sum_imaging_pregnants <- sum(data_imaging_pregnant$Total_number_of_patients)
sum_imaging_pregnants

#Remove all empty columns
emptycols <- sapply(data_imaging_pregnant, function (k) all(is.na(k)))
data_imaging_pregnant <- data_imaging_pregnant[!emptycols]
names(data_imaging_pregnant)

#Reformat data from wide to long
data_imaging_pregnant_long <- gather(data_imaging_pregnant, imaging, frequency, pathologic_findings:Bronchiectasis, factor_key=TRUE)

freq <- as.numeric(data_imaging_pregnant_long$frequency)

#Run meta analysis for all imaging listed in 'data_imaging_pregnant'

library(meta)
mtprop <- metaprop(event=as.numeric(frequency), n=Total_number_of_patients, studlab=Study_nr, data=subset(data_imaging_pregnant_long, data_imaging_pregnant_long$imaging=='pathologic_findings'  ##Replace with comorbidity of interest 
                                                                                                          & data_imaging_pregnant_long$frequency>=0))
#Number of patients with comorbidity of interest
sum(mtprop$event)
#Total number of patients
sum(mtprop$n)

#print results from the meta-analysis
print(mtprop)

#Plot results from the meta-analysis using a Forest plot
forest(mtprop, xlim = c(-10,100), pscale = 100, 
       comb.fixed = F,
       comb.random = mtprop$comb.random,
       leftcols = c("studlab", "event",  "n"),
       rightcols =           c( "effect", "ci"), 
       leftlabs = c("Study", "Number", "Total"), 
       rightlabs=c("Prevalence (%)", "95% CI"),
       xlab = "Prevalence (%)", addspace = TRUE, digits = 1,
       squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2",type.random = "diamond")

#####----imaging in children CoVID-19 patients----####

data_imaging_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_imaging.csv", sep =',', header = TRUE)


#Subset data: children, Cohort Studies and Case series, and reported imaging yes
data_imaging_children <- subset(data_imaging_all, Study_Population =='Pediatric' & imaging_reported == 'Yes')

#count number of children patients
sum_imaging_childrens <- sum(data_imaging_children$Total_number_of_patients)
sum_imaging_childrens

#Remove all empty columns
emptycols <- sapply(data_imaging_children, function (k) all(is.na(k)))
data_imaging_children <- data_imaging_children[!emptycols]
names(data_imaging_children)

#Reformat data from wide to long
data_imaging_children_long <- gather(data_imaging_children, imaging, frequency, pathologic_findings:Bulla, factor_key=TRUE)

freq <- as.numeric(data_imaging_children_long$frequency)

#Run meta analysis for all imaging listed in 'data_imaging_children'

library(meta)
mtprop <- metaprop(event=as.numeric(frequency), n=Total_number_of_patients, studlab=Study_nr, data=subset(data_imaging_children_long, data_imaging_children_long$imaging=='pathologic_findings'  ##Replace with comorbidity of interest 
                                                                                                          & data_imaging_children_long$frequency>=0))
#Number of patients with comorbidity of interest
sum(mtprop$event)
#Total number of patients
sum(mtprop$n)

#print results from the meta-analysis
print(mtprop)

#Plot results from the meta-analysis using a Forest plot
forest(mtprop, xlim = c(-10,100), pscale = 100, 
       comb.fixed = F,
       comb.random = mtprop$comb.random,
       leftcols = c("studlab", "event",  "n"),
       rightcols =           c( "effect", "ci"), 
       leftlabs = c("Study", "Number", "Total"), 
       rightlabs=c("Prevalence (%)", "95% CI"),
       xlab = "Prevalence (%)", addspace = TRUE, digits = 1,
       squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2",type.random = "diamond")


#####----treatment in adult CoVID-19 patients----####

data_treatment_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_treatment.csv", sep =',', header = TRUE)


#Subset data: Adult, Cohort Studies and Case series, and reported treatment yes
data_treatment_adult <- subset(data_treatment_all, Study_Population =='Adult' & Medication_reported == 'Yes')

#count number of adult patients
sum_treatment_adults <- sum(data_treatment_adult$Total_number_of_patients)
sum_treatment_adults

#Remove all empty columns
emptycols <- sapply(data_treatment_adult, function (k) all(is.na(k)))
data_treatment_adult <- data_treatment_adult[!emptycols]
names(data_treatment_adult)

#Reformat data from wide to long
data_treatment_adult_long <- gather(data_treatment_adult, treatment, frequency, pathologic_findings:Crazy_paving_pattern, factor_key=TRUE)

freq <- as.numeric(data_treatment_adult_long$frequency)

#Run meta analysis for all treatment listed in 'data_treatment_adult'

library(meta)
mtprop <- metaprop(event=as.numeric(frequency), n=Total_number_of_patients, studlab=Study_nr, data=subset(data_treatment_adult_long, data_treatment_adult_long$treatment=='Crazy_paving_pattern'  ##Replace with comorbidity of interest 
                                                                                                          & data_treatment_adult_long$frequency>=0))
#Number of patients with comorbidity of interest
sum(mtprop$event)
#Total number of patients
sum(mtprop$n)

#print results from the meta-analysis
print(mtprop)

#Plot results from the meta-analysis using a Forest plot
forest(mtprop, xlim = c(-10,100), pscale = 100, 
       comb.fixed = F,
       comb.random = mtprop$comb.random,
       leftcols = c("studlab", "event",  "n"),
       rightcols =           c( "effect", "ci"), 
       leftlabs = c("Study", "Number", "Total"), 
       rightlabs=c("Prevalence (%)", "95% CI"),
       xlab = "Prevalence (%)", addspace = TRUE, digits = 1,
       squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2",type.random = "diamond")


#####----treatment in pregnant CoVID-19 patients----####

data_treatment_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_treatment.csv", sep =',', header = TRUE)


#Subset data: pregnant, Cohort Studies and Case series, and reported treatment yes
data_treatment_pregnant <- subset(data_treatment_all, Study_Population =='Pregnant' & Medication_reported == 'Yes')

#count number of pregnant patients
sum_treatment_pregnants <- sum(data_treatment_pregnant$Total_number_of_patients)
sum_treatment_pregnants

#Remove all empty columns
emptycols <- sapply(data_treatment_pregnant, function (k) all(is.na(k)))
data_treatment_pregnant <- data_treatment_pregnant[!emptycols]
names(data_treatment_pregnant)

#Reformat data from wide to long
data_treatment_pregnant_long <- gather(data_treatment_pregnant, treatment, frequency, Nr_of_studies_w_AB:Alpha_interferon_aerosol_inhalation, factor_key=TRUE)

freq <- as.numeric(data_treatment_pregnant_long$frequency)

#Run meta analysis for all treatment listed in 'data_treatment_pregnant'

library(meta)
mtprop <- metaprop(event=as.numeric(frequency), n=Total_number_of_patients, studlab=Study_nr, data=subset(data_treatment_pregnant_long, data_treatment_pregnant_long$treatment=='Nr_of_studies_w_AB'  ##Replace with comorbidity of interest 
                                                                                                          & data_treatment_pregnant_long$frequency>=0))
#Number of patients with comorbidity of interest
sum(mtprop$event)
#Total number of patients
sum(mtprop$n)

#print results from the meta-analysis
print(mtprop)

#Plot results from the meta-analysis using a Forest plot
forest(mtprop, xlim = c(-10,100), pscale = 100, 
       comb.fixed = F,
       comb.random = mtprop$comb.random,
       leftcols = c("studlab", "event",  "n"),
       rightcols =           c( "effect", "ci"), 
       leftlabs = c("Study", "Number", "Total"), 
       rightlabs=c("Prevalence (%)", "95% CI"),
       xlab = "Prevalence (%)", addspace = TRUE, digits = 1,
       squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2",type.random = "diamond")

#####----treatment in children CoVID-19 patients----####

data_treatment_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_treatment.csv", sep =',', header = TRUE)


#Subset data: children, Cohort Studies and Case series, and reported treatment yes
data_treatment_children <- subset(data_treatment_all, Study_Population =='Pediatric' & Medication_reported == 'Yes')

#count number of children patients
sum_treatment_childrens <- sum(data_treatment_children$Total_number_of_patients)
sum_treatment_childrens

#Remove all empty columns
emptycols <- sapply(data_treatment_children, function (k) all(is.na(k)))
data_treatment_children <- data_treatment_children[!emptycols]
names(data_treatment_children)

#Reformat data from wide to long
data_treatment_children_long <- gather(data_treatment_children, treatment, frequency, Nr_of_studies_w_AB:Alpha_interferon_aerosol_inhalation, factor_key=TRUE)

freq <- as.numeric(data_treatment_children_long$frequency)

#Run meta analysis for all treatment listed in 'data_treatment_children'

library(meta)
mtprop <- metaprop(event=as.numeric(frequency), n=Total_number_of_patients, studlab=Study_nr, data=subset(data_treatment_children_long, data_treatment_children_long$treatment=='Antibiotics_n'  ##Replace with comorbidity of interest 
                                                                                                          & data_treatment_children_long$frequency>=0))
#Number of patients with comorbidity of interest
sum(mtprop$event)
#Total number of patients
sum(mtprop$n)

#print results from the meta-analysis
print(mtprop)

#Plot results from the meta-analysis using a Forest plot
forest(mtprop, xlim = c(-10,100), pscale = 100, 
       comb.fixed = F,
       comb.random = mtprop$comb.random,
       leftcols = c("studlab", "event",  "n"),
       rightcols =           c( "effect", "ci"), 
       leftlabs = c("Study", "Number", "Total"), 
       rightlabs=c("Prevalence (%)", "95% CI"),
       xlab = "Prevalence (%)", addspace = TRUE, digits = 1,
       squaresize = 0.5, text.I2 = "I2", text.tau2 = "tau2",type.random = "diamond")






