rm(list=ls())

data_laboratory_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_labor_all_patients.csv", sep =',', header = TRUE)


#########ADULTS##########
#Subset data: Adult patients and reported laboratory: yes
data_laboratory_adult <- subset(data_laboratory_all, Study_Population =='Adult' &laboratory_reported=='Yes')
names(data_laboratory_adult)

#count number of adult patients
sum_adults_laboratory <-sum(data_laboratory_adult$Total_number_of_patients)
sum_adults_laboratory

#Remove all empty columns
emptycols <- sapply(data_laboratory_adult, function (k) all(is.na(k)))
data_laboratory_adult <- data_laboratory_adult[!emptycols]
names(data_laboratory_adult)

#----------------------------------------------------------------------
###Leucocytes
#Calculate pooled median of all studies using sample size as weight
median_Leucocytes_adult_overall<-pool.med(data_laboratory_adult$Leucocytes_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Leucocytes_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Leucocytes_adult_overall<-pool.med(data_laboratory_adult$Leucocytes_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Leucocytes_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Leucocytes_adult_overall<-pool.med(data_laboratory_adult$Leucocytes_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Leucocytes_adult_overall

#----------------------------------------------------------------------
###Lymphocyte
#Calculate pooled median of all studies using sample size as weight
median_Lymphocyte_adult_overall<-pool.med(data_laboratory_adult$Lymphocyte_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Lymphocyte_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Lymphocyte_adult_overall<-pool.med(data_laboratory_adult$Lymphocyte_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Lymphocyte_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Lymphocyte_adult_overall<-pool.med(data_laboratory_adult$Lymphocyte_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Lymphocyte_adult_overall

#----------------------------------------------------------------------
###Neutrophils
#Calculate pooled median of all studies using sample size as weight
median_Neutrophils_adult_overall<-pool.med(data_laboratory_adult$Neutrophils_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Neutrophils_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Neutrophils_adult_overall<-pool.med(data_laboratory_adult$Neutrophils_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Neutrophils_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Neutrophils_adult_overall<-pool.med(data_laboratory_adult$Neutrophils_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Neutrophils_adult_overall

#----------------------------------------------------------------------
###Platelet
#Calculate pooled median of all studies using sample size as weight
median_Platelet_adult_overall<-pool.med(data_laboratory_adult$Platelet_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Platelet_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Platelet_adult_overall<-pool.med(data_laboratory_adult$Platelet_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Platelet_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Platelet_adult_overall<-pool.med(data_laboratory_adult$Platelet_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Platelet_adult_overall

#----------------------------------------------------------------------
###Hemoglobin
#Calculate pooled median of all studies using sample size as weight
median_Hemoglobin_adult_overall<-pool.med(data_laboratory_adult$Hemoglobin_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Hemoglobin_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Hemoglobin_adult_overall<-pool.med(data_laboratory_adult$Hemoglobin_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Hemoglobin_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Hemoglobin_adult_overall<-pool.med(data_laboratory_adult$Hemoglobin_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Hemoglobin_adult_overall

#----------------------------------------------------------------------
###Prothrombin_time
#Calculate pooled median of all studies using sample size as weight
median_Prothrombin_time_adult_overall<-pool.med(data_laboratory_adult$Prothrombin_time_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Prothrombin_time_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Prothrombin_time_adult_overall<-pool.med(data_laboratory_adult$Prothrombin_time_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Prothrombin_time_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Prothrombin_time_adult_overall<-pool.med(data_laboratory_adult$Prothrombin_time_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Prothrombin_time_adult_overall

#----------------------------------------------------------------------
###Activated_PTT
#Calculate pooled median of all studies using sample size as weight
median_Activated_PTT_adult_overall<-pool.med(data_laboratory_adult$Activated_PTT_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Activated_PTT_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Activated_PTT_adult_overall<-pool.med(data_laboratory_adult$Activated_PTT_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Activated_PTT_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Activated_PTT_adult_overall<-pool.med(data_laboratory_adult$Activated_PTT_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Activated_PTT_adult_overall

#----------------------------------------------------------------------
###Fibrinogen
#Calculate pooled median of all studies using sample size as weight
median_Fibrinogen_adult_overall<-pool.med(data_laboratory_adult$Fibrinogen_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Fibrinogen_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Fibrinogen_adult_overall<-pool.med(data_laboratory_adult$Fibrinogen_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Fibrinogen_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Fibrinogen_adult_overall<-pool.med(data_laboratory_adult$Fibrinogen_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Fibrinogen_adult_overall

#----------------------------------------------------------------------
###Sodium
#Calculate pooled median of all studies using sample size as weight
median_Sodium_adult_overall<-pool.med(data_laboratory_adult$Sodium_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Sodium_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Sodium_adult_overall<-pool.med(data_laboratory_adult$Sodium_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Sodium_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Sodium_adult_overall<-pool.med(data_laboratory_adult$Sodium_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Sodium_adult_overall

#----------------------------------------------------------------------
###Potassium
#Calculate pooled median of all studies using sample size as weight
median_Potassium_adult_overall<-pool.med(data_laboratory_adult$Potassium_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Potassium_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Potassium_adult_overall<-pool.med(data_laboratory_adult$Potassium_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Potassium_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Potassium_adult_overall<-pool.med(data_laboratory_adult$Potassium_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Potassium_adult_overall

#----------------------------------------------------------------------
###Albumin
#Calculate pooled median of all studies using sample size as weight
median_Albumin_adult_overall<-pool.med(data_laboratory_adult$Albumin_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Albumin_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Albumin_adult_overall<-pool.med(data_laboratory_adult$Albumin_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Albumin_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Albumin_adult_overall<-pool.med(data_laboratory_adult$Albumin_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Albumin_adult_overall


#----------------------------------------------------------------------
###ALT
#Calculate pooled median of all studies using sample size as weight
median_ALT_adult_overall<-pool.med(data_laboratory_adult$ALT_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_ALT_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_ALT_adult_overall<-pool.med(data_laboratory_adult$ALT_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_ALT_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_ALT_adult_overall<-pool.med(data_laboratory_adult$ALT_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_ALT_adult_overall


#----------------------------------------------------------------------
###AST
#Calculate pooled median of all studies using sample size as weight
median_AST_adult_overall<-pool.med(data_laboratory_adult$AST_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_AST_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_AST_adult_overall<-pool.med(data_laboratory_adult$AST_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_AST_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_AST_adult_overall<-pool.med(data_laboratory_adult$AST_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_AST_adult_overall


#----------------------------------------------------------------------
###bilirubin
#Calculate pooled median of all studies using sample size as weight
median_bilirubin_adult_overall<-pool.med(data_laboratory_adult$bilirubin_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_bilirubin_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_bilirubin_adult_overall<-pool.med(data_laboratory_adult$bilirubin_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_bilirubin_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_bilirubin_adult_overall<-pool.med(data_laboratory_adult$bilirubin_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_bilirubin_adult_overall


#----------------------------------------------------------------------
###LDH
#Calculate pooled median of all studies using sample size as weight
median_LDH_adult_overall<-pool.med(data_laboratory_adult$LDH_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_LDH_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_LDH_adult_overall<-pool.med(data_laboratory_adult$LDH_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_LDH_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_LDH_adult_overall<-pool.med(data_laboratory_adult$LDH_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_LDH_adult_overall


#----------------------------------------------------------------------
###CK
#Calculate pooled median of all studies using sample size as weight
median_CK_adult_overall<-pool.med(data_laboratory_adult$CK_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_CK_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_CK_adult_overall<-pool.med(data_laboratory_adult$CK_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_CK_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_CK_adult_overall<-pool.med(data_laboratory_adult$CK_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_CK_adult_overall


#----------------------------------------------------------------------
###D_Dimer
#Calculate pooled median of all studies using sample size as weight
median_D_Dimer_adult_overall<-pool.med(data_laboratory_adult$D_Dimer_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_D_Dimer_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_D_Dimer_adult_overall<-pool.med(data_laboratory_adult$D_Dimer_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_D_Dimer_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_D_Dimer_adult_overall<-pool.med(data_laboratory_adult$D_Dimer_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_D_Dimer_adult_overall

#----------------------------------------------------------------------
###creatinin
#Calculate pooled median of all studies using sample size as weight
median_creatinin_adult_overall<-pool.med(data_laboratory_adult$creatinin_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_creatinin_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_creatinin_adult_overall<-pool.med(data_laboratory_adult$creatinin_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_creatinin_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_creatinin_adult_overall<-pool.med(data_laboratory_adult$creatinin_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_creatinin_adult_overall


#----------------------------------------------------------------------
###BUN
#Calculate pooled median of all studies using sample size as weight
median_BUN_adult_overall<-pool.med(data_laboratory_adult$BUN_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_BUN_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_BUN_adult_overall<-pool.med(data_laboratory_adult$BUN_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_BUN_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_BUN_adult_overall<-pool.med(data_laboratory_adult$BUN_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_BUN_adult_overall


#----------------------------------------------------------------------
###CRP
#Calculate pooled median of all studies using sample size as weight
median_CRP_adult_overall<-pool.med(data_laboratory_adult$CRP_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_CRP_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_CRP_adult_overall<-pool.med(data_laboratory_adult$CRP_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_CRP_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_CRP_adult_overall<-pool.med(data_laboratory_adult$CRP_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_CRP_adult_overall


#----------------------------------------------------------------------
###Procalcitonin
#Calculate pooled median of all studies using sample size as weight
median_Procalcitonin_adult_overall<-pool.med(data_laboratory_adult$Procalcitonin_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Procalcitonin_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Procalcitonin_adult_overall<-pool.med(data_laboratory_adult$Procalcitonin_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Procalcitonin_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Procalcitonin_adult_overall<-pool.med(data_laboratory_adult$Procalcitonin_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Procalcitonin_adult_overall

#----------------------------------------------------------------------
###Erythrocyte_sed_rate
#Calculate pooled median of all studies using sample size as weight
median_Erythrocyte_sed_rate_adult_overall<-pool.med(data_laboratory_adult$Erythrocyte_sed_rate_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Erythrocyte_sed_rate_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Erythrocyte_sed_rate_adult_overall<-pool.med(data_laboratory_adult$Erythrocyte_sed_rate_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Erythrocyte_sed_rate_adult_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Erythrocyte_sed_rate_adult_overall<-pool.med(data_laboratory_adult$Erythrocyte_sed_rate_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Erythrocyte_sed_rate_adult_overall

#----------------------------------------------------------------------
###IL6
#Calculate pooled median of all studies using sample size as weight
median_IL6_adult_overall<-pool.med(data_laboratory_adult$IL6_Median, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_IL6_adult_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_IL6_adult_overall<-pool.med(data_laboratory_adult$IL6_Q1, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_IL6_adult_overall


#Calculate pooled Q3 of all studies using sample size as weight
Q3_IL6_adult_overall<-pool.med(data_laboratory_adult$IL6_Q3, data_laboratory_adult$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_IL6_adult_overall
