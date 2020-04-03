#--------------------Forrest Plot for Age Groups------------------------------------------------------------------------------------------------------

#remove list
rm(list=ls())

#Install libraries if required
# if(!require(ggplot2)) install.packages("ggplot2")
# if(!require(plyr)) install.packages("plyr")
# if(!require(dplyr)) install.packages("dplyr")

#Load library
library(ggplot2)
library(plyr)
library(dplyr)

#Load data
age_distr <- read.csv("/Users/jutzelec/Desktop/age.csv", sep = ',', header = TRUE)

#Calculate mean of all studies
mean_age_overall <- age_distr %>% 
  pull(mean) %>% 
  mean() %>%
  signif(9)
  
#define legend title
legend_title <- "Study Size (#)"

#Plot age distribution
age_distr %>% 
  ggplot(aes(x = study, y = as.numeric(mean), ymin = lower_sd, ymax = upper_sd)) +
  geom_point(position = position_dodge(width = 0.2), aes(size = number_of_patients)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  geom_hline(yintercept=mean_age_overall, size= 0.5, color="red", linetype='dotdash')+
  coord_flip() +
  theme_bw()+
  ylab("Mean Age [years]")+
  xlab("")+
  theme(axis.title.x = element_text(size=10))


  




