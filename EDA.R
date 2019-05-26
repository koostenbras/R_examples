#install.packages("devtools")
library(devtools)

#devtools::install_github("alastairrushworth/inspectdf")
library(inspectdf)
library(tidyverse)
library(readr)

# reference: https://www.r-bloggers.com/part-2-simple-eda-in-r-with-inspectdf/

#### IMPORTING DATA ====
df <-  read_csv('https://raw.githubusercontent.com/lgellis/STEM/master/DATA-ART-1/Data/FinalData.csv', col_names = TRUE)

#Creating three data frames.
allGrades <- df

oldGrades <- allGrades %>% 
  filter(Grade > 5)

youngGrades <- allGrades %>% 
  filter(Grade < 6)

#View the distribution of grade to ensure it was split properly
ggplot(oldGrades, aes(x=Grade)) + geom_histogram()
ggplot(youngGrades, aes(x=Grade)) + geom_histogram()

#### RUN THE PACKAGE FUNCTIONS

inspect_types(allGrades, show_plot = TRUE)
inspect_types(youngGrades, oldGrades, show_plot = TRUE)

inspect_mem(allGrades, show_plot = TRUE)
inspect_mem(youngGrades, oldGrades, show_plot = TRUE)

inspect_na(allGrades, show_plot = TRUE)
inspect_na(youngGrades, oldGrades, show_plot = TRUE)

inspect_num(allGrades, show_plot = TRUE)
inspect_num(youngGrades, oldGrades, show_plot = TRUE)

inspect_imb(allGrades, show_plot = TRUE)
inspect_imb(youngGrades, oldGrades, show_plot = TRUE)

inspect_cat(allGrades, show_plot = TRUE)
inspect_cat(youngGrades, oldGrades, show_plot = TRUE)

inspect_cor(allGrades, show_plot = TRUE)
inspect_cor(youngGrades, oldGrades, show_plot = TRUE)


