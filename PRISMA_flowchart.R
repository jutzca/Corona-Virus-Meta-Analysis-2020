#Create PRISMA Flow Chart
#Code created by Catherine Jutzeler, modified version of https://github.com/geritwagner/r-prisma-diagram/blob/master/prisma-diagram.R

#Clear Working Space
rm(list=ls())

#Load libraries
library(tidyverse)
library(ggplot2)

#List Number of Studies Found in Each Database
search_db_1 = 10  #Embase
search_db_2 = 20  #Pubmed/Medline
search_db_3 = 30  #Scopus
search_db_4 = 40  #Web of Science
search_db_5 = 50  #Other Sources (e.g., Reference Lists)

#Caculate total number of studies indentified
total_results_from_search = search_db_1 + search_db_2 + search_db_3 +search_db_4+ search_db_5

#Caculate total number duplicates
duplicates_removed = 10

#Total number of records screened
records_screened = total_results_from_search - duplicates_removed

#Total number of records exluded based on titles and abstracts
exclusion1 = 20
full_text_articles_retrieved = records_screened - exclusion1

#Total number of records exluded based on full text
exclusion2 = 5
articles_indluded_in_synthesis = full_text_articles_retrieved - exclusion2

extraction = 20

format_statistics <- function(statistic){
  return(format(round(statistic), nsmall = 0, big.mark = ","))
}

#Create Flowchart
data <- tibble(x = 1:100, y = 1:100)
p<-data %>%
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  theme_linedraw() 
  
#Create boxes describing the different stages of the PRISMA guidelines
#Identification
p <- p +
  geom_rect(xmin = 0, xmax = 5, ymin = 70, ymax = 100, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 2.5, y = 85, label = 'Identification', size = 4, angle = 90)

#Screening
p <- p +
  geom_rect(xmin = 0, xmax = 5, ymin = 53, ymax = 68, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 2.5, y = 60.5, label = 'Screening', size = 4, angle = 90)

#Eligibility
p <- p +
  geom_rect(xmin = 0, xmax = 5, ymin = 38, ymax = 51, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 2.5, y = 44.5, label = 'Eligibility', size = 4, angle = 90)

#Inclusion
p <- p +
  geom_rect(xmin = 0, xmax = 5, ymin = 10, ymax = 35, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 2.5, y = 22.5, label = 'Inclusion', size = 4, angle = 90)

#Create boxes with data bases searched
#EMBASE
p <- p +
  geom_rect(xmin = 10, xmax = 25, ymin = 90, ymax = 100, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 17.5, y = 95, label = paste('EMBASE\n (n=',
                                                 format_statistics(search_db_1),
                                                 ')'), size = 4)

#Pubmed/Medline
p <- p +
  geom_rect(xmin = 29, xmax = 44, ymin = 90, ymax = 100, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 36.5, y = 95, label = paste('Pubmed/Medline \n (n=',
                                                 format_statistics(search_db_2),
                                                 ')'), size = 4)
#Scopus
p <- p +
  geom_rect(xmin = 49, xmax = 64, ymin = 90, ymax = 100, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 56.5, y = 95, label = paste('Scopus\n (n=',
                                                 format_statistics(search_db_3),
                                                 ')'), size = 4)
#Web of Science
p <- p +
  geom_rect(xmin = 69, xmax = 84, ymin = 90, ymax = 100, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 76.5, y = 95, label = paste('Web of Science\n (n=',
                                                 format_statistics(search_db_4),
                                                 ')'),size = 4)
#Other sources
p <- p +
  geom_rect(xmin = 89, xmax = 104, ymin = 90, ymax = 100, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 96.5, y = 95, label = paste('Identified by\n other sources\n (n=',
                                                   format_statistics(search_db_5),')'), size = 4)



#Draw connecting lines

p <- p +
  geom_segment(
    x = 17.5, xend = 17.5, y = 90, yend = 85,
    size = 0.15) +
  geom_segment(
    x = 36.5, xend = 36.5, y = 90, yend = 85,
    size = 0.15) +
  geom_segment(
    x = 56.5, xend = 56.5, y = 90, yend = 85,
    size = 0.15) +
  geom_segment(
    x = 76.5, xend = 76.5, y = 90, yend = 85,
    size = 0.15) +
  geom_segment(
    x = 96.5, xend = 96.5, y = 90, yend = 85,
    size = 0.15) +
  geom_segment(
    x = 17.5, xend = 96.5, y = 85, yend = 85,
    size = 0.15) +
  geom_segment(
    x = 35, xend = 35, y = 85, yend = 80,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"))


#Create boxes for flowchart
#Total results from search
p <- p +
  geom_rect(xmin = 20, xmax = 50, ymin = 70, ymax = 80, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 35, y = 75, label = paste('Total results from search\n (n=',
                                                 format_statistics(total_results_from_search),
                                                 ')'), size = 4)

#Duplicates removed
p <- p +
  geom_segment(
    x = 35, xend = 55, y = 67.5, yend = 67.5,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed")) +
  geom_segment(
    x = 35, xend = 35, y = 70, yend = 65,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"))

p <- p +
  geom_rect(xmin = 55, xmax = 85, ymin = 72.5, ymax = 62.5, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 70, y = 67.5, label = paste('Duplicates removed\n (n=',
                                                   format_statistics(duplicates_removed),
                                                   ')'), size = 4)
#Records screened
p <- p +
  geom_rect(xmin = 20, xmax = 50, ymin = 55, ymax = 65, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 35, y = 60, label = paste('Records screened\n (n=',
                                                 format_statistics(records_screened),
                                                 ')'),size = 4)
#Articles excluded (Title or Abstract)
p <- p +
  geom_segment(
    x = 35, xend = 55, y = 52.5, yend = 52.5,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed")) +
  geom_segment(
    x = 35, xend = 35, y = 55, yend = 50,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"))

p <- p +
  geom_rect(xmin = 55, xmax = 85, ymin = 57.5, ymax = 47.5, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 70, y = 52.5, label = paste('Articles excluded (Title or Abstract)\n (n=',
                                                   format_statistics(exclusion1),
                                                   ')'),size = 4)
#Full-text articles retrieved
p <- p +
  geom_rect(xmin = 20, xmax = 50, ymin = 40, ymax = 50, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 35, y = 45, label = paste('Full-text articles retrieved\n (n=',
                                                 format_statistics(full_text_articles_retrieved),
                                                 ')'), size = 4)

#Articles excluded (Full-text)
p <- p +
  geom_segment(
    x = 35, xend = 55, y = 37.5, yend = 37.5,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed")) +
  geom_segment(
    x = 35, xend = 35, y = 40, yend = 35,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"))

p <- p +
  geom_rect(xmin = 55, xmax = 85, ymin = 42.5, ymax = 32.5, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 70, y = 37.5, label = paste('Articles excluded (Full-text)\n (n=',
                                                   format_statistics(exclusion2),
                                                   ')'),size = 4)

#Articles included in the synthesis
p <- p +
  geom_rect(xmin = 20, xmax = 50, ymin = 25, ymax = 35, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 35, y = 30, label = paste('Articles included in the synthesis\n (n=',
                                                 format_statistics(articles_indluded_in_synthesis),
                                                 ')'),size = 4)
#Articles included in the classification
p <- p +
  geom_segment(
    x = 35, xend = 35, y = 25, yend = 20,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"))

p <- p +
  geom_rect(xmin = 20, xmax = 50, ymin = 10, ymax = 20, color = 'black',
            fill = 'white', size = 0.25) +
  annotate('text', x = 35, y = 15, label = paste('Articles included in the classification\n (n=',
                                                 format_statistics(extraction),
                                                 ')'),size = 4)

#Define theme
p <- p + theme_void()

#Print plot as pdf
pdf('/Users/jutzelec/Documents/GitHub/Corona-Virus-Meta-Analysis-2020/⁨Corona-Virus-Meta-Analysis-2020⁩prisma-diagram.pdf',width = 12, height=10)
plot(p)
dev.off()

