#loading libraries
library("dplyr")
library("ggplot2")

# Read in CSV file
seattle <- read.csv('data/Seattle_Crime_Stats_2008_To_Present.csv', stringsAsFactors = FALSE)

# Original Data frame we will be working with
seattle.crime <- mutate(seattle, year = substr(REPORT_DATE, 7, 10)) %>% 
  mutate(month = substr(REPORT_DATE, 1, 2)) %>% 
  select(CRIME_TYPE, STAT_VALUE, REPORT_DATE, Precinct, month, year)
names(seattle.crime) <- c("crime.type", "stat.value", "report.date", "precinct", "month", "year")
View(seattle.crime)

#Crime Vs Years Data Frame
crime.years <- group_by(seattle.crime, year, crime.type) %>% summarise(count = sum(stat.value))
#View(crime.years)

#Crime Vs Months
#NOTE: in the shiny app we will make it that the filtered year can be manipulated by the user
crime.months <- filter(seattle.crime, year == 2008) %>% 
group_by(month, crime.type) %>% summarise(test = sum(stat.value))
#View(crime.months)

#Crime Vs Precincts
#NOTE: in the shiny app we will make it that the filtered year can be manipulated by the user
crime.precincts <- filter(seattle.crime, year == 2008) %>% 
  group_by(precinct, crime.type) %>% summarise(test = sum(stat.value))
View(crime.precincts)


