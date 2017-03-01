library("shiny")

ui <- fluidPage(
  titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
      textInput('region', label = h3("Region"), value = "Enter text...")
    ), 
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Tab 1"),
                  tabPanel("Tab 2")
      )
    )
  )
)
shinyUI(ui)