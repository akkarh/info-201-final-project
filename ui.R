library("shiny")

ui <- fluidPage(
  titlePanel("Global Melody"),
  sidebarLayout(
    sidebarPanel(
      textInput('country', label = h3("Country"), value = "Enter text..."),
      sliderInput('popularity', label = h3("Popularity (100 being most popular)"), min = 0, max = 100, value = 50),
      sliderInput('loudness', label = h3("Loudness (dB)"), min = -60, max = 0, value = -30),
      sliderInput('danceability', label = h3("Danceability"), min = 0.0, max = 1.0, value = 0.5),
      sliderInput('energy', label = h3("Energy"), min = 0.0, max = 1.0, value = 0.5),
      sliderInput('instrumentalness', label = h3("Instrumentalness (it's a thing!)"), min = 0.0, max = 1.0, value = 0.5)
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