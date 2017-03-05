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

#Created a function that takes an integer year as a parameter and returns the total for specific crimes that were reported that year
CrimeYearFunction <- function(specefic.year){
  crime.year <- filter(seattle.crime, year== specefic.year) %>% 
    group_by(crime.type) %>% 
    summarise(counts = sum(stat.value)) %>%  
    mutate(year = specefic.year)
  return(crime.year)
}

#set variable names to the returns of the function
crime.2008 <- CrimeYearFunction(2008)
crime.2009 <- CrimeYearFunction(2009)
crime.2010 <- CrimeYearFunction(2010)
crime.2011 <- CrimeYearFunction(2011)
crime.2012 <- CrimeYearFunction(2012)
crime.2013 <- CrimeYearFunction(2013)
crime.2014 <- CrimeYearFunction(2014)

#combined all the years(if you guys know a more efficient way to do this let me know haha)
crime.years <- rbind(crime.2008, crime.2009)
crime.years <- rbind(crime.years, crime.2010)
crime.years <- rbind(crime.years, crime.2011)
crime.years <- rbind(crime.years, crime.2012)
crime.years <- rbind(crime.years, crime.2013)
crime.years <- rbind(crime.years, crime.2014)

View(crime.years)

