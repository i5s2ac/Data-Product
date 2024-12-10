library(plumber)
r <- plumb("model_api.R")
r$run(host = "0.0.0.0", port = 8001)
