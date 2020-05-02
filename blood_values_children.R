rm(list=ls())

data_laboratory_all <- read.csv("/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/Corona_review_labor_all_patients.csv", sep =',', header = TRUE)


#########childrenS##########
#Subset data: children patients and reported laboratory: yes
data_laboratory_children <- subset(data_laboratory_all, (Study_Population == 'Pediatric' |Study_Population == 'Neonatal') &laboratory_reported=='Yes')
names(data_laboratory_children)

#count number of children patients
sum_childrens_laboratory <-sum(data_laboratory_children$Total_number_of_patients)
sum_childrens_laboratory

#Remove all empty columns
emptycols <- sapply(data_laboratory_children, function (k) all(is.na(k)))
data_laboratory_children <- data_laboratory_children[!emptycols]
names(data_laboratory_children)

#---Leucocytes-------------------------------------------------------------------
#Calculate pooled median of all studies using sample size as weight
median_Leucocytes_children_overall<-pool.med(data_laboratory_children$Leucocytes_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Leucocytes_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Leucocytes_children_overall<-pool.med(data_laboratory_children$Leucocytes_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Leucocytes_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Leucocytes_children_overall<-pool.med(data_laboratory_children$Leucocytes_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Leucocytes_children_overall

#---Lymphocyte----------------------------------------------------------
#Calculate pooled median of all studies using sample size as weight
median_Lymphocyte_children_overall<-pool.med(data_laboratory_children$Lymphocyte_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Lymphocyte_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Lymphocyte_children_overall<-pool.med(data_laboratory_children$Lymphocyte_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Lymphocyte_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Lymphocyte_children_overall<-pool.med(data_laboratory_children$Lymphocyte_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Lymphocyte_children_overall

#---Neutrophils----------------------------------------------------------
#Calculate pooled median of all studies using sample size as weight
median_Neutrophils_children_overall<-pool.med(data_laboratory_children$Neutrophils_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Neutrophils_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Neutrophils_children_overall<-pool.med(data_laboratory_children$Neutrophils_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Neutrophils_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Neutrophils_children_overall<-pool.med(data_laboratory_children$Neutrophils_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Neutrophils_children_overall

#---Platelet--------------------------------------------------------
#Calculate pooled median of all studies using sample size as weight
median_Platelet_children_overall<-pool.med(data_laboratory_children$Platelet_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Platelet_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Platelet_children_overall<-pool.med(data_laboratory_children$Platelet_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Platelet_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Platelet_children_overall<-pool.med(data_laboratory_children$Platelet_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Platelet_children_overall

#----------------------------------------------------------------------
###Hemoglobin
#Calculate pooled median of all studies using sample size as weight
median_Hemoglobin_children_overall<-pool.med(data_laboratory_children$Hemoglobin_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Hemoglobin_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Hemoglobin_children_overall<-pool.med(data_laboratory_children$Hemoglobin_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Hemoglobin_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Hemoglobin_children_overall<-pool.med(data_laboratory_children$Hemoglobin_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Hemoglobin_children_overall

#----------------------------------------------------------------------
###Prothrombin_time
#Calculate pooled median of all studies using sample size as weight
median_Prothrombin_time_children_overall<-pool.med(data_laboratory_children$Prothrombin_time_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Prothrombin_time_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Prothrombin_time_children_overall<-pool.med(data_laboratory_children$Prothrombin_time_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Prothrombin_time_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Prothrombin_time_children_overall<-pool.med(data_laboratory_children$Prothrombin_time_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Prothrombin_time_children_overall

#----------------------------------------------------------------------
###Activated_PTT
#Calculate pooled median of all studies using sample size as weight
median_Activated_PTT_children_overall<-pool.med(data_laboratory_children$Activated_PTT_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Activated_PTT_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Activated_PTT_children_overall<-pool.med(data_laboratory_children$Activated_PTT_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Activated_PTT_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Activated_PTT_children_overall<-pool.med(data_laboratory_children$Activated_PTT_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Activated_PTT_children_overall

#----------------------------------------------------------------------
###Fibrinogen
#Calculate pooled median of all studies using sample size as weight
median_Fibrinogen_children_overall<-pool.med(data_laboratory_children$Fibrinogen_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Fibrinogen_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Fibrinogen_children_overall<-pool.med(data_laboratory_children$Fibrinogen_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Fibrinogen_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Fibrinogen_children_overall<-pool.med(data_laboratory_children$Fibrinogen_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Fibrinogen_children_overall

#----------------------------------------------------------------------
###Sodium
#Calculate pooled median of all studies using sample size as weight
median_Sodium_children_overall<-pool.med(data_laboratory_children$Sodium_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Sodium_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Sodium_children_overall<-pool.med(data_laboratory_children$Sodium_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Sodium_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Sodium_children_overall<-pool.med(data_laboratory_children$Sodium_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Sodium_children_overall

#----------------------------------------------------------------------
###Potassium
#Calculate pooled median of all studies using sample size as weight
median_Potassium_children_overall<-pool.med(data_laboratory_children$Potassium_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Potassium_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Potassium_children_overall<-pool.med(data_laboratory_children$Potassium_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Potassium_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Potassium_children_overall<-pool.med(data_laboratory_children$Potassium_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Potassium_children_overall

#----------------------------------------------------------------------
###Albumin
#Calculate pooled median of all studies using sample size as weight
median_Albumin_children_overall<-pool.med(data_laboratory_children$Albumin_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Albumin_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Albumin_children_overall<-pool.med(data_laboratory_children$Albumin_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Albumin_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Albumin_children_overall<-pool.med(data_laboratory_children$Albumin_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Albumin_children_overall

#------ALT----------------------------------------------------------------
#Calculate pooled median of all studies using sample size as weight
median_ALT_children_overall<-pool.med(data_laboratory_children$ALT_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_ALT_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_ALT_children_overall<-pool.med(data_laboratory_children$ALT_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_ALT_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_ALT_children_overall<-pool.med(data_laboratory_children$ALT_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_ALT_children_overall

#------AST----------------------------------------------------------------
#Calculate pooled median of all studies using sample size as weight
median_AST_children_overall<-pool.med(data_laboratory_children$AST_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_AST_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_AST_children_overall<-pool.med(data_laboratory_children$AST_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_AST_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_AST_children_overall<-pool.med(data_laboratory_children$AST_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_AST_children_overall


#----------------------------------------------------------------------
###bilirubin
#Calculate pooled median of all studies using sample size as weight
median_bilirubin_children_overall<-pool.med(data_laboratory_children$bilirubin_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_bilirubin_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_bilirubin_children_overall<-pool.med(data_laboratory_children$bilirubin_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_bilirubin_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_bilirubin_children_overall<-pool.med(data_laboratory_children$bilirubin_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_bilirubin_children_overall


#----------------------------------------------------------------------
###LDH
#Calculate pooled median of all studies using sample size as weight
median_LDH_children_overall<-pool.med(data_laboratory_children$LDH_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_LDH_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_LDH_children_overall<-pool.med(data_laboratory_children$LDH_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_LDH_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_LDH_children_overall<-pool.med(data_laboratory_children$LDH_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_LDH_children_overall


#----------------------------------------------------------------------
###CK
#Calculate pooled median of all studies using sample size as weight
median_CK_children_overall<-pool.med(data_laboratory_children$CK_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_CK_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_CK_children_overall<-pool.med(data_laboratory_children$CK_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_CK_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_CK_children_overall<-pool.med(data_laboratory_children$CK_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_CK_children_overall


#----------------------------------------------------------------------
###D_Dimer
#Calculate pooled median of all studies using sample size as weight
median_D_Dimer_children_overall<-pool.med(data_laboratory_children$D_Dimer_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_D_Dimer_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_D_Dimer_children_overall<-pool.med(data_laboratory_children$D_Dimer_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_D_Dimer_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_D_Dimer_children_overall<-pool.med(data_laboratory_children$D_Dimer_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_D_Dimer_children_overall

#----------------------------------------------------------------------
###creatinin
#Calculate pooled median of all studies using sample size as weight
median_creatinin_children_overall<-pool.med(data_laboratory_children$creatinin_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_creatinin_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_creatinin_children_overall<-pool.med(data_laboratory_children$creatinin_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_creatinin_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_creatinin_children_overall<-pool.med(data_laboratory_children$creatinin_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_creatinin_children_overall


#----------------------------------------------------------------------
###BUN
#Calculate pooled median of all studies using sample size as weight
median_BUN_children_overall<-pool.med(data_laboratory_children$BUN_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_BUN_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_BUN_children_overall<-pool.med(data_laboratory_children$BUN_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_BUN_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_BUN_children_overall<-pool.med(data_laboratory_children$BUN_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_BUN_children_overall


#----------------------------------------------------------------------
###CRP
#Calculate pooled median of all studies using sample size as weight
median_CRP_children_overall<-pool.med(data_laboratory_children$CRP_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_CRP_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_CRP_children_overall<-pool.med(data_laboratory_children$CRP_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_CRP_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_CRP_children_overall<-pool.med(data_laboratory_children$CRP_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_CRP_children_overall


#----------------------------------------------------------------------
###Procalcitonin
#Calculate pooled median of all studies using sample size as weight
median_Procalcitonin_children_overall<-pool.med(data_laboratory_children$Procalcitonin_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Procalcitonin_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Procalcitonin_children_overall<-pool.med(data_laboratory_children$Procalcitonin_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Procalcitonin_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Procalcitonin_children_overall<-pool.med(data_laboratory_children$Procalcitonin_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Procalcitonin_children_overall

#----------------------------------------------------------------------
###Erythrocyte_sed_rate
#Calculate pooled median of all studies using sample size as weight
median_Erythrocyte_sed_rate_children_overall<-pool.med(data_laboratory_children$Erythrocyte_sed_rate_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_Erythrocyte_sed_rate_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_Erythrocyte_sed_rate_children_overall<-pool.med(data_laboratory_children$Erythrocyte_sed_rate_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_Erythrocyte_sed_rate_children_overall

#Calculate pooled Q3 of all studies using sample size as weight
Q3_Erythrocyte_sed_rate_children_overall<-pool.med(data_laboratory_children$Erythrocyte_sed_rate_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_Erythrocyte_sed_rate_children_overall

#----------------------------------------------------------------------
###IL6
#Calculate pooled median of all studies using sample size as weight
median_IL6_children_overall<-pool.med(data_laboratory_children$IL6_Median, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
median_IL6_children_overall

#Calculate pooled Q1 of all studies using sample size as weight
Q1_IL6_children_overall<-pool.med(data_laboratory_children$IL6_Q1, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q1_IL6_children_overall


#Calculate pooled Q3 of all studies using sample size as weight
Q3_IL6_children_overall<-pool.med(data_laboratory_children$IL6_Q3, data_laboratory_children$Total_number_of_patients, norm.approx = TRUE, coverage.prob = 1)
Q3_IL6_children_overall

