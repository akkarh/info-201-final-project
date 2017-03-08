library("shiny")
library("dplyr")
library("ggplot2")

server <- function(input, output) {
  seattle <- read.csv('data/Seattle_Crime_Stats_2008_To_Present.csv', stringsAsFactors = FALSE)
  seattle.crime <- mutate(seattle, year = substr(REPORT_DATE, 7, 10)) %>% 
    mutate(month = substr(REPORT_DATE, 1, 2)) %>% 
    select(CRIME_TYPE, STAT_VALUE, REPORT_DATE, Precinct, month, year)
  names(seattle.crime) <- c("crime.type", "stat.value", "report.date", "precinct", "month", "year")
  
  output$plot <- renderPlot({
    data <- filter(seattle.crime, year == input$year) %>% 
      group_by(month, crime.type) %>% summarise(count = sum(stat.value))
    
    if(input$xaxis == "Years"){
      #Crime Vs Years Data Frame
      crime.years <- group_by(seattle.crime, year, crime.type) %>% summarise(count = sum(stat.value))
      
      # Crime v. Years Plot
      # If we are going to have an option to filter the data by crime type, we need the following line of code:
      # crime.years <- crime.years %>% filter(crime.type == "crime type")
      ggplot(crime.years, aes(x = year, y = count, fill = crime.type)) +
        geom_col() +
        labs(x = "Years", y = "Number of Occurences of the Crime", title = "Frequencies of Crime Types Over Time") +
        scale_fill_brewer(type = "qual", palette = "Set2")
    }
    
    else if(input$xaxis == "Months"){
      crime.months <- filter(seattle.crime, year == input$year) %>% 
        group_by(month, crime.type) %>% summarise(count = sum(stat.value))
      # Crime v. Months
      # Month numbers to month names: to make the graph more readable
      months <- c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr",
                  "05" = "May", "06" = "Jun", "07" = "Jul", "08" = "Aug",
                  "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")
      months.as.numbers <- crime.months$month
      months.names <- months[months.as.numbers]
      crime.months$month <- months.names
      # Prevents alphabetical sorting of the x axis.
      crime.months$month <- factor(crime.months$month, levels = unique(crime.months$month))
      
      ggplot(crime.months, aes(x = month, y = count, fill = crime.type)) +
        scale_x_discrete("month") +
        geom_col() +
        labs(x = "Months", y = paste("Number of occurences of the Crime in", input$year), title = paste("Frequencies of Crime Types in", input$year)) +
        scale_fill_brewer(type = "qual", palette = "Set2")
    }
    
    else if(input$xaxis == "Precincts"){
      #Crime Vs Precincts
      #NOTE: in the shiny app we will make it that the filtered year can be manipulated by the user
      crime.precincts <- filter(seattle.crime, year == input$year) %>% 
        group_by(precinct, crime.type) %>% summarise(count = sum(stat.value))
      
      # Crime v. Precincts
      ggplot(crime.precincts, aes(x = precinct, y = count, fill = crime.type)) +
        geom_col() +
        labs(x = "Precincts", y = paste("Number of occurences of the Crime in", input$year), title = paste("Frequencies of Crime Types by Precincts in", input$year)) +
        scale_fill_brewer(type = "qual", palette = "Set2")
    }
    
  })
  output$xtitle = renderText({
    return(input$xaxis)
  })
  
  #outputs data about the most common occuring crime
  output$maxCrime = renderText({
    if(input$xaxis == "Years"){
      crime.years <- group_by(seattle.crime, year, crime.type) %>% summarise(count = sum(stat.value))
      maxData <- filter(crime.years, count == max(count)) %>% arrange(-count)
      return(paste(maxData$crime.type[1], "in the year", maxData$year[1], "with a count of", maxData$count[1]))
    }
    else if(input$xaxis == "Months"){
      crime.months <- filter(seattle.crime, year == input$year) %>% 
        group_by(month, crime.type) %>% summarise(count = sum(stat.value))
      # Crime v. Months
      # Month numbers to month names: to make the graph more readable
      months <- c("01" = "January", "02" = "February", "03" = "March", "04" = "April",
                  "05" = "May", "06" = "June", "07" = "July", "08" = "August",
                  "09" = "September", "10" = "October", "11" = "November", "12" = "December")
      months.as.numbers <- crime.months$month
      months.names <- months[months.as.numbers]
      crime.months$month <- months.names
      # Prevents alphabetical sorting of the x axis.
      crime.months$month <- factor(crime.months$month, levels = unique(crime.months$month))
      
      maxData <- filter(crime.months, count == max(count)) %>% arrange(-count)
      return(paste(maxData$crime.type[1], "with a count of", maxData$count[1]))
    }
    else if(input$xaxis == "Precincts"){
      crime.precincts <- filter(seattle.crime, year == input$year) %>% 
        group_by(precinct, crime.type) %>% summarise(count = sum(stat.value))
      
      maxData <- filter(crime.precincts, count == max(count)) %>% arrange(-count)
      return(paste(maxData$crime.type[1], "in the", maxData$precinct[1], "precinct, with a count of", maxData$count[1]))
    }
  })
  
  #outputs information about the least common crime
  output$minCrime = renderText({
    if(input$xaxis == "Years"){
      crime.years <- group_by(seattle.crime, year, crime.type) %>% summarise(count = sum(stat.value))
      minData <- filter(crime.years, year < 2014 & count == min(count)) %>% arrange(count)
      return(paste(minData$crime.type[1], "in the year", minData$year[1], "with a count of", minData$count[1]))
    }
    else if(input$xaxis == "Months"){
      crime.months <- filter(seattle.crime, year == input$year) %>% 
        group_by(month, crime.type) %>% summarise(count = sum(stat.value))
      # Crime v. Months
      # Month numbers to month names: to make the graph more readable
      months <- c("01" = "January", "02" = "February", "03" = "March", "04" = "April",
                  "05" = "May", "06" = "June", "07" = "July", "08" = "August",
                  "09" = "September", "10" = "October", "11" = "November", "12" = "December")
      months.as.numbers <- crime.months$month
      months.names <- months[months.as.numbers]
      crime.months$month <- months.names
      # Prevents alphabetical sorting of the x axis.
      crime.months$month <- factor(crime.months$month, levels = unique(crime.months$month))
      
      minData <- filter(crime.months, count == min(count)) %>% arrange(count)
      return(paste(minData$crime.type[1], "in the month of", minData$month[1], "with a count of", minData$count[1]))
    }
    else if(input$xaxis == "Precincts"){
      crime.precincts <- filter(seattle.crime, year == input$year) %>% 
        group_by(precinct, crime.type) %>% summarise(count = sum(stat.value))
      
      minData <- filter(crime.precincts, count == min(count)) %>% arrange(count)
      return(paste(minData$crime.type[1], "in the", minData$precinct[1], "precinct, with a count of", minData$count[1]))
    }
  })
}

shinyServer(server)