#loading libraries
library("dplyr")
library("ggplot2")



# Read in CSV file
seattle <- read.csv('data/Seattle_Crime_Stats_2008_To_Present.csv', stringsAsFactors = FALSE)

#created a year column
seattle.crime <- mutate(seattle, year = substr(REPORT_DATE, 7, 10)) %>% 
  select(Police.Beat, CRIME_TYPE, STAT_VALUE, REPORT_DATE, Sector, year)
View(seattle.crime)

colnames(seattle.crime)
