# Deploy
install_github("rahulsh97/d3poindia", force = TRUE)
install_github("pachadotdev/d3po", force = TRUE)
install_github("pachadotdev/tabler", force = TRUE)
rsconnect::deployApp(appFiles = "app.R")
