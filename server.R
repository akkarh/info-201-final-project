# install.packages("plotly")

library("shiny")
library("dplyr")
library("ggplot2")
library("plotly")


# ggplot code for bar graphs
barGraph <- function(data, xValue, xLab, yLab, chart.title) {
  plot <- ggplot(data, aes(x = xValue, y = count, fill = crime.type))
  plot <- plot + geom_bar(stat = "identity", position = "stack")
  plot <- plot + labs(x = xLab, y = yLab, title = chart.title)
  plot <- plot + scale_fill_brewer(type = "qual", palette = "Set2")
  plot <- ggplotly(plot)
  return(plot)
}

pieChart <- function(data, chart.title) {
  crime.count <- sum(data$count)
  data <- data %>% mutate(percent = round((count / crime.count) * 100, 2))
  colors <- c('rgb(102,194,165)', 'rgb(252,141,98)', 'rgb(141,160,203)', 'rgb(231,138,195)', 'rgb(166,216,84)', 'rgb(255,217,47)', 'rgb(229,196,148)')
  plot_ly(data, labels = ~crime.type, values = ~percent, type = "pie",
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1))) %>% 
    layout(title = chart.title,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}

# Make the months data table easier to graph
processCrimeMonths <- function(months, data) {
  months.as.numbers <- data$month
  months.names <- months[months.as.numbers]
  data$month <- months.names
  # Prevents alphabetical sorting of the x axis.
  data$month <- factor(data$month, levels = unique(data$month))
  return(data)
}

server <- function(input, output) {
  seattle <- read.csv('data/Seattle_Crime_Stats_2008_To_Present.csv', stringsAsFactors = FALSE)
  precinct.info <- read.csv('data/Seattle_Precinct_Information.csv', stringsAsFactors = FALSE)
  months <- c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr",
              "05" = "May", "06" = "Jun", "07" = "Jul", "08" = "Aug",
              "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")
  seattle.crime <- mutate(seattle, year = substr(REPORT_DATE, 7, 10)) %>% 
    mutate(month = substr(REPORT_DATE, 1, 2)) %>% 
    select(CRIME_TYPE, STAT_VALUE, REPORT_DATE, Precinct, month, year)
  names(seattle.crime) <- c("crime.type", "stat.value", "report.date", "precinct", "month", "year")
  # Crime v. Years Data frame
  crime.years <- group_by(seattle.crime, year, crime.type) %>% summarise(count = sum(stat.value))
  
  output$table <- renderTable(width = '200px', {
    precinct.info <- precinct.info %>% filter(Precinct == input$precinct)
    precinct.info[1]
  })
  
  output$plot <- renderPlotly({
    if(input$xaxis == "Years") {
      barGraph(crime.years, crime.years$`year`, "Years", "Number of Occurences of the Crime", "Frequencies of Crime Types Over Time")
    } else if(input$xaxis == "Months") {
      # Crime v. Month Data frame
      crime.months <- filter(seattle.crime, year == input$year) %>% 
        group_by(month, crime.type) %>% summarise(count = sum(stat.value))
      # Month numbers to month names: to make the graph more readable
      crime.months <- processCrimeMonths(months, crime.months)
      barGraph(crime.months, crime.months$`month`, "Years", paste("Number of occurences of the Crime in", input$year), paste("Frequencies of Crime Types in", input$year))
    } else if(input$xaxis == "Precincts") {
      # Crime v. Precinct Data frame
      crime.precincts <- filter(seattle.crime, year == input$year) %>% 
        group_by(precinct, crime.type) %>% summarise(count = sum(stat.value))
      # Crime v. Precincts
      barGraph(crime.precincts, crime.precincts$`precinct`, "Precincts", paste("Number of occurences of the Crime in", input$year), paste("Frequencies of Crime Types by Precincts in", input$year))
    }
  })
  
  # Code to output the chart on the 'Pie Chart' tab.
  output$chart <- renderPlotly({
    if(input$xaxis == "Years") {
      pieChart(crime.years, "Percent of Crime Types Over Time")
    } else if(input$xaxis == "Months") {
      # Crime v. Month Data frame
      crime.months <- filter(seattle.crime, year == input$year) %>% 
        group_by(month, crime.type) %>% summarise(count = sum(stat.value))
      pieChart(crime.months, paste("Percent of Crime Types in ", input$year))
    } else if(input$xaxis == "Precincts") {
      # Crime v. Precinct Data frame
      crime.precincts <- filter(seattle.crime, year == input$year) %>% 
        group_by(precinct, crime.type) %>% summarise(count = sum(stat.value))
      pieChart(crime.precincts, paste("Percent of Crimes in ", input$year))
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
    } else if (input$xaxis == "Months"){
        crime.months <- filter(seattle.crime, year == input$year) %>% 
          group_by(month, crime.type) %>% summarise(count = sum(stat.value))
        # Crime v. Months
        # Month numbers to month names: to make the graph more readable
        crime.months <- processCrimeMonths(months, crime.months)
        maxData <- filter(crime.months, count == max(count)) %>% arrange(-count)
        return(paste(maxData$crime.type[1], "with a count of", maxData$count[1]))
    } else if (input$xaxis == "Precincts"){
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
    } else if(input$xaxis == "Months"){
      crime.months <- filter(seattle.crime, year == input$year) %>% 
        group_by(month, crime.type) %>% summarise(count = sum(stat.value))
      # Crime v. Months
      # Month numbers to month names: to make the graph more readable
      crime.months <- processCrimeMonths(months, crime.months)
      minData <- filter(crime.months, count == min(count)) %>% arrange(count)
      return(paste(minData$crime.type[1], "in the month of", minData$month[1], "with a count of", minData$count[1]))
    } else if(input$xaxis == "Precincts"){
      crime.precincts <- filter(seattle.crime, year == input$year) %>% 
        group_by(precinct, crime.type) %>% summarise(count = sum(stat.value))
      
      minData <- filter(crime.precincts, count == min(count)) %>% arrange(count)
      return(paste(minData$crime.type[1], "in the", minData$precinct[1], "precinct, with a count of", minData$count[1]))
    }
  })
}

shinyServer(server)