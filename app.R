# Launch the ShinyApp

library(sf)
library(geojsonsf)
library(dplyr)
library(d3poindia)

options("golem.app.prod" = TRUE)
d3poindia::run_app() # add parameters here (if any)
