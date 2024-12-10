library(plumber)
r <- plumb("weather_api.R")
r$run(port = 8001)






