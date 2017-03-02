library("httr")
library("jsonlite")
library("dplyr")
library("ggplot2")

source("apikey.R")


response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(client.id, client.secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)
mytoken = content(response)$access_token
HeaderValue = paste0('Bearer ', mytoken)



URI = "https://api.spotify.com/v1/search?q=popularity=100&type=track&market=US"
response2 = GET(url = URI, add_headers(Authorization = HeaderValue))
rec = content(response2)
print(rec)
