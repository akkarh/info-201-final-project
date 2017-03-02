library("httr")
library("jsonlite")
library("dplyr")
library("ggplot2")

source("apikey.R")

#base.uri <- "https://api.spotify.com/v1"
#resource <- "/recommendations"
#query.params <- list("api-key"=nyt.apikey, query=movie)


response <- GET("https://api.spotify.com/v1/audio-analysis/3JIxjvbbDrA9ztYlNcp3yL",api.key )
body <- fromJSON(content(response, "text"))

View(body)