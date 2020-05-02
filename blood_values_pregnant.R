rm(list=ls())

data_laboratory_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_labor_all_patients.csv", sep =',', header = TRUE)


#########pregnantS##########
#Subset data: pregnant patients and reported laboratory: yes
data_laboratory_pregnant <- subset(data_laboratory_all, Study_Population =='Pregnant' &laboratory_reported=='Yes')
names(data_laboratory_pregnant)

#count number of pregnant patients
sum_pregnants_laboratory <-sum(data_laboratory_pregnant$Total_number_of_patients)
sum_pregnants_laboratory

#Remove all empty columns
emptycols <- sapply(data_laboratory_pregnant, function (k) all(is.na(k)))
data_laboratory_pregnant <- data_laboratory_pregnant[!emptycols]
names(data_laboratory_pregnant)

#---Leucocytes-------------------------------------------------------------------
#Calculate pooled median of all studies using sample size as weight
median_Leucocytes_pregnant_overall<-pool.med(data_laboratory_pregnant$Leucocytes_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Leucocytes_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Leucocytes_pregnant_overall<-pool.med(data_laboratory_pregnant$Leucocytes_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Leucocytes_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Leucocytes_pregnant_overall<-pool.med(data_laboratory_pregnant$Leucocytes_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Leucocytes_pregnant_overall

#---Lymphocyte----------------------------------------------------------
#Calculate pooled median of all studies using sample size as weight
median_Lymphocyte_pregnant_overall<-pool.med(data_laboratory_pregnant$Lymphocyte_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Lymphocyte_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Lymphocyte_pregnant_overall<-pool.med(data_laboratory_pregnant$Lymphocyte_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Lymphocyte_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Lymphocyte_pregnant_overall<-pool.med(data_laboratory_pregnant$Lymphocyte_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Lymphocyte_pregnant_overall

#---Neutrophils----------------------------------------------------------
#Calculate pooled median of all studies using sample size as weight
median_Neutrophils_pregnant_overall<-pool.med(data_laboratory_pregnant$Neutrophils_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Neutrophils_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Neutrophils_pregnant_overall<-pool.med(data_laboratory_pregnant$Neutrophils_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Neutrophils_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Neutrophils_pregnant_overall<-pool.med(data_laboratory_pregnant$Neutrophils_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Neutrophils_pregnant_overall

#---Platelet--------------------------------------------------------
#Calculate pooled median of all studies using sample size as weight
median_Platelet_pregnant_overall<-pool.med(data_laboratory_pregnant$Platelet_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Platelet_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Platelet_pregnant_overall<-pool.med(data_laboratory_pregnant$Platelet_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Platelet_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Platelet_pregnant_overall<-pool.med(data_laboratory_pregnant$Platelet_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Platelet_pregnant_overall

#----------------------------------------------------------------------
###Hemoglobin
#Calculate pooled median of all studies using sample size as weight
median_Hemoglobin_pregnant_overall<-pool.med(data_laboratory_pregnant$Hemoglobin_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Hemoglobin_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Hemoglobin_pregnant_overall<-pool.med(data_laboratory_pregnant$Hemoglobin_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Hemoglobin_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Hemoglobin_pregnant_overall<-pool.med(data_laboratory_pregnant$Hemoglobin_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Hemoglobin_pregnant_overall

#----------------------------------------------------------------------
###Prothrombin_time
#Calculate pooled median of all studies using sample size as weight
median_Prothrombin_time_pregnant_overall<-pool.med(data_laboratory_pregnant$Prothrombin_time_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Prothrombin_time_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Prothrombin_time_pregnant_overall<-pool.med(data_laboratory_pregnant$Prothrombin_time_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Prothrombin_time_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Prothrombin_time_pregnant_overall<-pool.med(data_laboratory_pregnant$Prothrombin_time_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Prothrombin_time_pregnant_overall

#----------------------------------------------------------------------
###Activated_PTT
#Calculate pooled median of all studies using sample size as weight
median_Activated_PTT_pregnant_overall<-pool.med(data_laboratory_pregnant$Activated_PTT_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Activated_PTT_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Activated_PTT_pregnant_overall<-pool.med(data_laboratory_pregnant$Activated_PTT_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Activated_PTT_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Activated_PTT_pregnant_overall<-pool.med(data_laboratory_pregnant$Activated_PTT_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Activated_PTT_pregnant_overall

#----------------------------------------------------------------------
###Fibrinogen
#Calculate pooled median of all studies using sample size as weight
median_Fibrinogen_pregnant_overall<-pool.med(data_laboratory_pregnant$Fibrinogen_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Fibrinogen_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Fibrinogen_pregnant_overall<-pool.med(data_laboratory_pregnant$Fibrinogen_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Fibrinogen_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Fibrinogen_pregnant_overall<-pool.med(data_laboratory_pregnant$Fibrinogen_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Fibrinogen_pregnant_overall

#----------------------------------------------------------------------
###Sodium
#Calculate pooled median of all studies using sample size as weight
median_Sodium_pregnant_overall<-pool.med(data_laboratory_pregnant$Sodium_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Sodium_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Sodium_pregnant_overall<-pool.med(data_laboratory_pregnant$Sodium_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Sodium_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Sodium_pregnant_overall<-pool.med(data_laboratory_pregnant$Sodium_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Sodium_pregnant_overall

#----------------------------------------------------------------------
###Potassium
#Calculate pooled median of all studies using sample size as weight
median_Potassium_pregnant_overall<-pool.med(data_laboratory_pregnant$Potassium_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Potassium_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Potassium_pregnant_overall<-pool.med(data_laboratory_pregnant$Potassium_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Potassium_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Potassium_pregnant_overall<-pool.med(data_laboratory_pregnant$Potassium_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Potassium_pregnant_overall

#----------------------------------------------------------------------
###Albumin
#Calculate pooled median of all studies using sample size as weight
median_Albumin_pregnant_overall<-pool.med(data_laboratory_pregnant$Albumin_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Albumin_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Albumin_pregnant_overall<-pool.med(data_laboratory_pregnant$Albumin_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Albumin_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Albumin_pregnant_overall<-pool.med(data_laboratory_pregnant$Albumin_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Albumin_pregnant_overall


#----------------------------------------------------------------------
###ALT
#Calculate pooled median of all studies using sample size as weight
median_ALT_pregnant_overall<-pool.med(data_laboratory_pregnant$ALT_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_ALT_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_ALT_pregnant_overall<-pool.med(data_laboratory_pregnant$ALT_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_ALT_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_ALT_pregnant_overall<-pool.med(data_laboratory_pregnant$ALT_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_ALT_pregnant_overall


#----------------------------------------------------------------------
###AST
#Calculate pooled median of all studies using sample size as weight
median_AST_pregnant_overall<-pool.med(data_laboratory_pregnant$AST_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_AST_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_AST_pregnant_overall<-pool.med(data_laboratory_pregnant$AST_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_AST_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_AST_pregnant_overall<-pool.med(data_laboratory_pregnant$AST_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_AST_pregnant_overall


#----------------------------------------------------------------------
###bilirubin
#Calculate pooled median of all studies using sample size as weight
median_bilirubin_pregnant_overall<-pool.med(data_laboratory_pregnant$bilirubin_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_bilirubin_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_bilirubin_pregnant_overall<-pool.med(data_laboratory_pregnant$bilirubin_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_bilirubin_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_bilirubin_pregnant_overall<-pool.med(data_laboratory_pregnant$bilirubin_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_bilirubin_pregnant_overall


#----------------------------------------------------------------------
###LDH
#Calculate pooled median of all studies using sample size as weight
median_LDH_pregnant_overall<-pool.med(data_laboratory_pregnant$LDH_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_LDH_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_LDH_pregnant_overall<-pool.med(data_laboratory_pregnant$LDH_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_LDH_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_LDH_pregnant_overall<-pool.med(data_laboratory_pregnant$LDH_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_LDH_pregnant_overall


#----------------------------------------------------------------------
###CK
#Calculate pooled median of all studies using sample size as weight
median_CK_pregnant_overall<-pool.med(data_laboratory_pregnant$CK_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_CK_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_CK_pregnant_overall<-pool.med(data_laboratory_pregnant$CK_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_CK_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_CK_pregnant_overall<-pool.med(data_laboratory_pregnant$CK_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_CK_pregnant_overall


#----------------------------------------------------------------------
###D_Dimer
#Calculate pooled median of all studies using sample size as weight
median_D_Dimer_pregnant_overall<-pool.med(data_laboratory_pregnant$D_Dimer_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_D_Dimer_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_D_Dimer_pregnant_overall<-pool.med(data_laboratory_pregnant$D_Dimer_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_D_Dimer_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_D_Dimer_pregnant_overall<-pool.med(data_laboratory_pregnant$D_Dimer_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_D_Dimer_pregnant_overall

#----------------------------------------------------------------------
###creatinin
#Calculate pooled median of all studies using sample size as weight
median_creatinin_pregnant_overall<-pool.med(data_laboratory_pregnant$creatinin_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_creatinin_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_creatinin_pregnant_overall<-pool.med(data_laboratory_pregnant$creatinin_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_creatinin_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_creatinin_pregnant_overall<-pool.med(data_laboratory_pregnant$creatinin_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_creatinin_pregnant_overall


#----------------------------------------------------------------------
###BUN
#Calculate pooled median of all studies using sample size as weight
median_BUN_pregnant_overall<-pool.med(data_laboratory_pregnant$BUN_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_BUN_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_BUN_pregnant_overall<-pool.med(data_laboratory_pregnant$BUN_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_BUN_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_BUN_pregnant_overall<-pool.med(data_laboratory_pregnant$BUN_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_BUN_pregnant_overall


#----------------------------------------------------------------------
###CRP
#Calculate pooled median of all studies using sample size as weight
median_CRP_pregnant_overall<-pool.med(data_laboratory_pregnant$CRP_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_CRP_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_CRP_pregnant_overall<-pool.med(data_laboratory_pregnant$CRP_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_CRP_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_CRP_pregnant_overall<-pool.med(data_laboratory_pregnant$CRP_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_CRP_pregnant_overall


#----------------------------------------------------------------------
###Procalcitonin
#Calculate pooled median of all studies using sample size as weight
median_Procalcitonin_pregnant_overall<-pool.med(data_laboratory_pregnant$Procalcitonin_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Procalcitonin_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Procalcitonin_pregnant_overall<-pool.med(data_laboratory_pregnant$Procalcitonin_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Procalcitonin_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Procalcitonin_pregnant_overall<-pool.med(data_laboratory_pregnant$Procalcitonin_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Procalcitonin_pregnant_overall

#----------------------------------------------------------------------
###Erythrocyte_sed_rate
#Calculate pooled median of all studies using sample size as weight
median_Erythrocyte_sed_rate_pregnant_overall<-pool.med(data_laboratory_pregnant$Erythrocyte_sed_rate_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Erythrocyte_sed_rate_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Erythrocyte_sed_rate_pregnant_overall<-pool.med(data_laboratory_pregnant$Erythrocyte_sed_rate_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Erythrocyte_sed_rate_pregnant_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Erythrocyte_sed_rate_pregnant_overall<-pool.med(data_laboratory_pregnant$Erythrocyte_sed_rate_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Erythrocyte_sed_rate_pregnant_overall

#----------------------------------------------------------------------
###IL6
#Calculate pooled median of all studies using sample size as weight
median_IL6_pregnant_overall<-pool.med(data_laboratory_pregnant$IL6_Median, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_IL6_pregnant_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_IL6_pregnant_overall<-pool.med(data_laboratory_pregnant$IL6_Q1, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_IL6_pregnant_overall


#Calculate pooled Q3 of all studies using sample size as weight
Q3_IL6_pregnant_overall<-pool.med(data_laboratory_pregnant$IL6_Q3, data_laboratory_pregnant$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_IL6_pregnant_overall

