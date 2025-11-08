# Deploy
install_github("rahulsh97/d3poindia", force = TRUE)
rsconnect::deployApp(appFiles = "app.R")
