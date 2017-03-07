library("shiny")

ui <- fluidPage(
  titlePanel("Crime Rates in Seattle"),
  sidebarLayout(
    sidebarPanel(
      #selection of x-axis
      radioButtons('xaxis', "X-Axis", choices = c("Years", "Months", "Precincts")),
      #if months is selected as x=axis, allows for selection of year
      conditionalPanel(condition = "input.xaxis === 'Months' || input.xaxis == 'Precincts'", selectInput('year', "Year", choices = c(2008:2014)))
    ), 
    mainPanel(
      #data visualization
      plotOutput("plot", hover = "plot_hover")
    )
  )
)
shinyUI(ui)