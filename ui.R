library("shiny")
library("plotly")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),
  titlePanel("Crime Rates in Seattle", windowTitle = "Crime Rate Explorer"),
  tags$p("This interactive application is made to explore the crime rates
         data in Seattle from 2008-2014. Our application is intended for
         anyone who might be interested in seeing how the crime rates have
         changed over time in the Seattle area."),
  tags$li("The Occurences tab displays a bar chart of the frequencies of
          various crimes over years, months, or precincts."),
  tags$li("The Percent Breakdown tab displays a pie chart percent breakdowns of the
          crime types."),
  tags$br(),
  tags$p("Use the control widgets to select a variable to view by!"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Neighborhoods'",
        selectInput("precinct", "Select a Precinct", choices = c("North", "South", "East", "West", "South West"))
      ),
      #selection of x-axis
      conditionalPanel(
        condition = "input.tabs == 'Occurences' || input.tabs == 'Percent Breakdown'",
        conditionalPanel(condition = "input.tabs == 'Occurences'",
                         radioButtons('xaxis', "Select a variable", choices = c("Years", "Months", "Precincts"))),
        conditionalPanel(condition = "input.xaxis == 'Months' || input.xaxis == 'Precincts' || input.tabs == 'Percent Breakdown'", selectInput('year', "Year", choices = c(2008:2014)))
      )
    ),
    mainPanel(
      #data visualization
      tabsetPanel(id = "tabs",
                  tabPanel("Occurences",
                           plotlyOutput("plot"),
                           p("This bar graph shows the relationship between the occurences of various types of crimes and ", textOutput('xtitle', inline=TRUE),
                             ". The graph shows that the most commonly occuring crime is ", textOutput('maxCrime', inline=TRUE),
                             ". The least commonly occuring crime is ", textOutput('minCrime', inline=TRUE), ".")),
                  tabPanel("Percent Breakdown",
                           plotlyOutput("chart")),
                  tabPanel("Neighborhoods",
                           tableOutput("table"))
      )
    )
  )
)
shinyUI(ui)