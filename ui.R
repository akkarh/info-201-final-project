library("shiny")

ui <- fluidPage(
  titlePanel("Crime Rates in Seattle"),
  sidebarLayout(
    sidebarPanel(
      radioButtons('xaxis', "X-Axis", choices = c("Years", "Months", "Crime Type")),
      conditionalPanel(condition = "input.xaxis === 'Months'", selectInput('year', "Year", choices = c(2008:2017)))
    ), 
    mainPanel(
      
    )
  )
)
shinyUI(ui)