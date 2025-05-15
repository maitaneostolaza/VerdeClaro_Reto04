library(plumber)
r <- plumb("Api.R")
r$run(port=8000)

