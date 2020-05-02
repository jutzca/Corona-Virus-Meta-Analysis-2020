#-----Meta analysis of categorical variables for severity (non-severe vs severe) and mortalitty (survivors vs non-survivors)
##Created by Catherine Jutzeler, April 22nd, 2020

####Load libraries
library(dmetar)
library(meta)
library(metafor)

###Get information of currentsession
sessionInfo()

#######------------------------Meta-analysis with categorical variables-------------------------------------#####
#####Severe vs non-severe####

#Load data
meta_severe_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Meta_analysis_severe_non_severe.csv", sep =',', header = TRUE)
names(meta_severe_data)

condition1 <- subset(meta_severe_data, meta_severe_data$Condition=="Hypertension" & (!(is.na(meta_severe_data$Ec))) & (!(is.na(meta_severe_data$Ee))))

condition_list <- unique(meta_severe_data$Condition)
condition_list

m.bin_hypertension <- metabin(Ee,
                 Ne,
                 Ec,
                 Nc,
                 data = condition1,
                 studlab = paste(study_nr),
                 comb.fixed = FALSE,
                 comb.random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 prediction = TRUE,
                 incr = 0.1,
                 sm = "RR")
m.bin_hypertension


forest(m.bin_hypertension)


#number of events (death)
sum(m.bin_hypertension$event.e)
sum(m.bin_hypertension$n.e)

#number of events (survivor)
sum(m.bin_hypertension$event.c)
sum(m.bin_hypertension$n.c)

eggers.test(x = m.bin_hypertension)


eggers.test(x = m.bin_hypertension)
funnel(m.bin_hypertension, xlab="Risk Ratio (RR)")
mtext(paste("Hypertension"), side=3, line=2, font=2, cex=1.0)
mtext(paste("Funnel Plot Asymmetry: p=0.66"),side=3,line=0.7)


#Load data
rm(list=ls())
meta_severe_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Meta_analysis_severe_non_severe.csv", sep =',', header = TRUE)
names(meta_severe_data)

condition1 <- subset(meta_severe_data, meta_severe_data$Condition=="Diabetes" & (!(is.na(meta_severe_data$Ec))) & (!(is.na(meta_severe_data$Ee))))

condition_list <- unique(meta_severe_data$Condition)

m.bin_Diabetes <- metabin(Ee,
                              Ne,
                              Ec,
                              Nc,
                              data = condition1,
                              studlab = paste(study_nr),
                              comb.fixed = FALSE,
                              comb.random = TRUE,
                              method.tau = "REML",
                              hakn = TRUE,
                              prediction = TRUE,
                              incr = 0.1,
                              sm = "RR")
m.bin_Diabetes


forest(m.bin_Diabetes)

eggers.test(x = m.bin_Diabetes)
funnel(m.bin_Diabetes, xlab="Risk Ratio (RR)")
mtext(paste("Diabetes Mellitus"), side=3, line=2, font=2, cex=1.0)
mtext(paste("Funnel Plot Asymmetry: p=0.28"),side=3,line=0.7)


#Load data
rm(list=ls())
meta_severe_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Meta_analysis_severe_non_severe.csv", sep =',', header = TRUE)
names(meta_severe_data)

condition1 <- subset(meta_severe_data, meta_severe_data$Condition=="COPD" & (!(is.na(meta_severe_data$Ec))) & (!(is.na(meta_severe_data$Ee))))

condition_list <- unique(meta_severe_data$Condition)

m.bin_COPD <- metabin(Ee,
                          Ne,
                          Ec,
                          Nc,
                          data = condition1,
                          studlab = paste(study_nr),
                          comb.fixed = FALSE,
                          comb.random = TRUE,
                          method.tau = "REML",
                          hakn = TRUE,
                          prediction = TRUE,
                          incr = 0.1,
                          sm = "RR")
m.bin_COPD


forest(m.bin_COPD)

eggers.test(x = m.bin_COPD)
funnel(m.bin_COPD, xlab="Risk Ratio (RR)")
mtext(paste("COPD"), side=3, line=2, font=2, cex=1.0)
mtext(paste("Funnel Plot Asymmetry: p=0.03"),side=3,line=0.7)


#Load data
rm(list=ls())
meta_severe_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Meta_analysis_severe_non_severe.csv", sep =',', header = TRUE)
names(meta_severe_data)

condition1 <- subset(meta_severe_data, meta_severe_data$Condition=="ARDS" & (!(is.na(meta_severe_data$Ec))) & (!(is.na(meta_severe_data$Ee))))

condition_list <- unique(meta_severe_data$Condition)

m.bin_ARDS <- metabin(Ee,
                      Ne,
                      Ec,
                      Nc,
                      data = condition1,
                      studlab = paste(study_nr),
                      comb.fixed = FALSE,
                      comb.random = TRUE,
                      method.tau = "REML",
                      hakn = TRUE,
                      prediction = TRUE,
                      incr = 0.1,
                      sm = "RR")
m.bin_ARDS


forest(m.bin_ARDS)

eggers.test(x = m.bin_ARDS)
funnel(m.bin_ARDS, xlab="Risk Ratio (RR)")
mtext(paste("ARDS"), side=3, line=2, font=2, cex=1.0)
mtext(paste("Funnel Plot Asymmetry: p=0.07"),side=3,line=0.7)




#####Death vs survivor####

#Load data
meta_survivor_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Meta_analysis_severe_non_severe.csv", sep =',', header = TRUE)
names(meta_survivor_data)


condition_list <- unique(meta_survivor_data$Condition)
condition_list

condition1 <- subset(meta_survivor_data, meta_survivor_data$Condition=="Any heart condition" & (!(is.na(meta_survivor_data$Ec))) & (!(is.na(meta_survivor_data$Ee))))

m.bin_Anyheartcondition <- metabin(Ee,
                              Ne,
                              Ec,
                              Nc,
                              data = condition1,
                              studlab = paste(study_nr),
                              comb.fixed = FALSE,
                              comb.random = TRUE,
                              method.tau = "REML",
                              hakn = TRUE,
                              prediction = TRUE,
                              incr = 0.1,
                              sm = "RR")
m.bin_Anyheartcondition


forest(m.bin_Anyheartcondition)


#number of events (death)
sum(m.bin_Anyheartcondition$event.e)
sum(m.bin_Anyheartcondition$n.e)

#number of events (survivor)
sum(m.bin_Anyheartcondition$event.c)
sum(m.bin_Anyheartcondition$n.c)

eggers.test(x = m.bin_Anyheartcondition)


eggers.test(x = m.bin_Anyheartcondition)
funnel(m.bin_Anyheartcondition, xlab="Risk Ratio (RR)")
mtext(paste("Any heart condition"), side=3, line=2, font=2, cex=1.0)
mtext(paste("Funnel Plot Asymmetry: p=0.67"),side=3,line=0.7)


#Load data
meta_survivor_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Meta_analysis_survivor_death.csv", sep =',', header = TRUE)
names(meta_survivor_data)


condition_list <- unique(meta_survivor_data$Condition)
condition_list

condition1 <- subset(meta_survivor_data, meta_survivor_data$Condition=="All mechanical ventilation" & (!(is.na(meta_survivor_data$Ec))) & (!(is.na(meta_survivor_data$Ee))))

m.bin_all_ventilation <- metabin(Ee,
                                   Ne,
                                   Ec,
                                   Nc,
                                   data = condition1,
                                   studlab = paste(study_nr),
                                   comb.fixed = FALSE,
                                   comb.random = TRUE,
                                   method.tau = "REML",
                                   hakn = TRUE,
                                   prediction = TRUE,
                                   incr = 0.1,
                                   sm = "RR")
m.bin_all_ventilation


forest(m.bin_all_ventilation)


#number of events (death)
sum(m.bin_all_ventilation$event.e)
sum(m.bin_all_ventilation$n.e)

#number of events (survivor)
sum(m.bin_all_ventilation$event.c)
sum(m.bin_all_ventilation$n.c)

eggers.test(x = m.bin_all_ventilation)


eggers.test(x = m.bin_all_ventilation)
funnel(m.bin_all_ventilation, xlab="Risk Ratio (RR)")
mtext(paste("All mechanical ventilation"), side=3, line=2, font=2, cex=1.0)
mtext(paste("Funnel Plot Asymmetry: p=0.686"),side=3,line=0.7)



#Load data
meta_survivor_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Meta_analysis_survivor_death.csv", sep =',', header = TRUE)
names(meta_survivor_data)


condition_list <- unique(meta_survivor_data$Condition)
condition_list

condition1 <- subset(meta_survivor_data, meta_survivor_data$Condition=="ARDS" & (!(is.na(meta_survivor_data$Ec))) & (!(is.na(meta_survivor_data$Ee))))

m.bin_ARDS <- metabin(Ee,
                                 Ne,
                                 Ec,
                                 Nc,
                                 data = condition1,
                                 studlab = paste(study_nr),
                                 comb.fixed = FALSE,
                                 comb.random = TRUE,
                                 method.tau = "REML",
                                 hakn = TRUE,
                                 prediction = TRUE,
                                 incr = 0.1,
                                 sm = "RR")
m.bin_ARDS


forest(m.bin_ARDS)


#number of events (death)
sum(m.bin_ARDS$event.e)
sum(m.bin_ARDS$n.e)

#number of events (survivor)
sum(m.bin_ARDS$event.c)
sum(m.bin_ARDS$n.c)

eggers.test(x = m.bin_ARDS)


eggers.test(x = m.bin_ARDS)
funnel(m.bin_ARDS, xlab="Risk Ratio (RR)")
mtext(paste("ARDS"), side=3, line=2, font=2, cex=1.0)
mtext(paste("Funnel Plot Asymmetry: p=0.20"),side=3,line=0.7)





#Load data
meta_survivor_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Meta_analysis_survivor_death.csv", sep =',', header = TRUE)
names(meta_survivor_data)


condition_list <- unique(meta_survivor_data$Condition)
condition_list

condition1 <- subset(meta_survivor_data, meta_survivor_data$Condition=="Acute kidney injury" & (!(is.na(meta_survivor_data$Ec))) & (!(is.na(meta_survivor_data$Ee))))

m.bin_acutekidneyinjury <- metabin(Ee,
                      Ne,
                      Ec,
                      Nc,
                      data = condition1,
                      studlab = paste(study_nr),
                      comb.fixed = FALSE,
                      comb.random = TRUE,
                      method.tau = "REML",
                      hakn = TRUE,
                      prediction = TRUE,
                      incr = 0.1,
                      sm = "RR")
m.bin_acutekidneyinjury


forest(m.bin_acutekidneyinjury)


#number of events (death)
sum(m.bin_acutekidneyinjury$event.e)
sum(m.bin_acutekidneyinjury$n.e)

#number of events (survivor)
sum(m.bin_acutekidneyinjury$event.c)
sum(m.bin_acutekidneyinjury$n.c)

eggers.test(x = m.bin_acutekidneyinjury)


eggers.test(x = m.bin_acutekidneyinjury)
funnel(m.bin_acutekidneyinjury, xlab="Risk Ratio (RR)")
mtext(paste("Acute kidney injury"), side=3, line=2, font=2, cex=1.0)
mtext(paste("Funnel Plot Asymmetry: p=0.23"),side=3,line=0.7)



###-----------Sensitivity analysis-------###
##Exlude studies that classify patients based on cardiac injury into sever and non-severe
#Load data
meta_survivor_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Meta_analysis_severe_non_severe_without_cardiac.csv", sep =',', header = TRUE)
names(meta_survivor_data)


condition_list <- unique(meta_survivor_data$Condition)
condition_list

condition1 <- subset(meta_survivor_data, meta_survivor_data$Condition=="Any heart condition" & (!(is.na(meta_survivor_data$Ec))) & (!(is.na(meta_survivor_data$Ee))))

m.bin_Anyheartcondition <- metabin(Ee,
                                   Ne,
                                   Ec,
                                   Nc,
                                   data = condition1,
                                   studlab = paste(study_nr),
                                   comb.fixed = FALSE,
                                   comb.random = TRUE,
                                   method.tau = "REML",
                                   hakn = TRUE,
                                   prediction = TRUE,
                                   incr = 0.1,
                                   sm = "RR")
m.bin_Anyheartcondition


forest(m.bin_Anyheartcondition)


#number of events (death)
sum(m.bin_Anyheartcondition$event.e)
sum(m.bin_Anyheartcondition$n.e)

#number of events (survivor)
sum(m.bin_Anyheartcondition$event.c)
sum(m.bin_Anyheartcondition$n.c)

eggers.test(x = m.bin_Anyheartcondition)


eggers.test(x = m.bin_Anyheartcondition)
funnel(m.bin_Anyheartcondition, xlab="Risk Ratio (RR)")
mtext(paste("Any heart condition"), side=3, line=2, font=2, cex=1.0)
mtext(paste("Funnel Plot Asymmetry: p=0.67"),side=3,line=0.7)





#######------------------------Meta-analysis with continous variables-------------------------------------#####
#####Severe vs non-severe####
#Load data
meta_severe_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Meta_analysis_age_labs_only-survivors.csv", sep =',', header = TRUE)
names(meta_severe_data)

#Show list of laboratory parameters and time from onset of sympytoms to admission to the hospital
labs_list <- unique(meta_severe_data$labs)
labs_list

#Change the variable of interest (e.g.,Time_onset_admission)
lab1 <- subset(meta_severe_data, meta_severe_data$labs=="Time_onset_admission" &  meta_severe_data$disease_status=='non-survivor' &  (!(is.na(meta_severe_data$mean_ne))))

##Run meta-analysis using the metacont
meta_cont_model<-metacont(ne, 
                   mean_ne, 
                   se_ne, 
                   nc, 
                   mean_nc, 
                   se_nc, 
                   studlab = paste(PMID),
                   data=lab1, 
                   sm="SMD",
                   level = 0.95,
                   comb.fixed=F, comb.random=TRUE,
                   title="", complab="", outclab="",
                   label.e="Cases", label.c="Control",
                   print.byvar=TRUE)

meta_cont_model

###Show meta forestplot
forest(meta_cont_model)

#number of events (death)
sum(meta_cont_model$n.c)
sum(meta_cont_model$n.e)

#Run Egger's test to determine publication bias
eggers.test(x = meta_cont_model)


#####Survivor vs non-survivors####
#Load data
meta_survivor_data <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Meta_analysis_age_labs_survivor_non_survivor.csv", sep =',', header = TRUE)
names(meta_survivor_data)

labs_list <- unique(meta_survivor_data$labs)
labs_list


lab1 <- subset(meta_survivor_data, meta_survivor_data$labs=="Time_onset_admission" &  meta_survivor_data$disease_status=='survivor' &  (!(is.na(meta_survivor_data$mean_ne))))


meta_age<-metacont(ne, 
                   mean_ne, 
                   se_ne, 
                   nc, 
                   mean_nc, 
                   se_nc, 
                   studlab = paste(PMID),
                   data=lab1, 
                   sm="SMD",
                   level = 0.95,
                   comb.fixed=F, comb.random=TRUE,
                   title="", complab="", outclab="",
                   label.e="Cases", label.c="Control",
                   print.byvar=TRUE)

meta_age

forest(meta_age)

#number of events (death)
sum(meta_age$n.c)
sum(meta_age$n.e)

eggers.test(x = meta_age)

