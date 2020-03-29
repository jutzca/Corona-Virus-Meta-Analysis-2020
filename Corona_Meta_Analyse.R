#----------------R Code for Corona Meta-Analyses----------------#
#Created by C. Jutzeler, March 28th, 2020

#Clear working space
rm(list=ls())

#Install libraries
if(!require(devtools)){install.packages("devtools")}
if(!require(metafor)){install.packages("metafor")}

#Load libraries
library(devtools)
library(metafor)

#-------------------------Data preparation------------------------------------------------------------------------------------------------------

#load dataset
emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/18_Corona_Review/3_Data/corona_data.csv", sep = ',', header = T,  na.strings=c("","NA"))


#-------------------------Data Analysis------------------------------------------------------------------------------------------------------


###How to calculte IQRs from SD: Then simply use mean=median and SD = IQR/1.35.
##https://www.rdocumentation.org/packages/estmeansd/versions/0.2.0/topics/bc.mean.sd
###https://arxiv.org/pdf/1505.05687.pdf


#-------------------------Data visualization------------------------------------------------------------------------------------------------------

##Figure 2A: Map of where studies were conducted







##Figure 2B: Population pyramide of sex distribution across studies






##Figure 2C: Population pyramide of age distribution across studies (Forrest Plot)






#Figure 3: Meta-analysis results of signs and symptoms





#Figure 4: Meta-analysis results of comorbidities




#Figure 5: Meta-analysis results of treatments




#Forest Plot and funnel plot

#https://cran.rstudio.com/web/packages/metaviz/vignettes/metaviz.html






