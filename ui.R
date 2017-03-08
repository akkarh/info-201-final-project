library("shiny")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),
  headerPanel("Crime Rates in Seattle"),
  sidebarLayout(
    sidebarPanel(
      #selection of x-axis
      radioButtons('xaxis', "X-Axis", choices = c("Years", "Months", "Precincts")),
      #if months is selected as x=axis, allows for selection of year
      conditionalPanel(condition = "input.xaxis === 'Months' || input.xaxis == 'Precincts'", selectInput('year', "Year", choices = c(2008:2014)))
    ),
    mainPanel(
      #data visualization
      tabsetPanel(
        tabPanel("Bar Graph",
                 plotOutput("plot", hover = "plot_hover"),
                 p("This visualization shows the relationship between the occurences of various types of crimes and ", textOutput('xtitle', inline=TRUE),
                   ". The graph shows that the most commonly occuring crime is ", textOutput('maxCrime', inline=TRUE),
                   ". The least commonly occuring crime is ", textOutput('minCrime', inline=TRUE), ".")),
        tabPanel("Pie Chart",
                 plotOutput("chart", hover = "chart_hover"))
      )
    )
  )
)
shinyUI(ui)
