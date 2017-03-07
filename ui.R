library("shiny")

ui <- fluidPage(
  titlePanel("Crime Rates in Seattle"),
  sidebarLayout(
    sidebarPanel(
      #all vs Specefic crime choice
      selectInput("crime.selection", label = ("Select Crime"), 
                  choices = list("All (Default)" = "All", "Homocide" = "Homocide", "Rape" = "Rape", "Robbery" = "Robbery",
                                 "Assault" = "Assault",  "Larcency-Theft" = "Larcency-Theft", 
                                 "Motor Vehicle Theft" = "Motor Vehicle Theft", "Burglary" = "Burglary"), 
                  selected = "All"),
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