required_cran <- c("renv", "rsconnect", "assertthat", "dplyr", "purrr", "htmlwidgets")

for (p in required_cran) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

# Replace these with the actual GitHub repos/refs for the packages you need
# e.g. list(d3po = "pachadotdev/d3po", tabler = "pachadotdev/tabler")
github_deps <- list(
  d3po = "pachadotdev/d3po",
  tabler = "pachadotdev/tabler"
)

# Install GitHub dependencies (if not already installed)
for (pkg in names(github_deps)) {
  repo <- github_deps[[pkg]]
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Installing %s from GitHub (%s)", pkg, repo))
    remotes::install_github(repo, force = T)
    # install.packages(repo, repos = "https://pachadotdev.r-universe.dev")
  } else {
    message(sprintf("%s already installed, skipping GitHub install", pkg))
  }
}

# (Optional) snapshot project libraries with renv so deployment uses the same package versions
# if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
# message("Initializing renv (if not already) and snapshotting library state...")
# renv::init(bare = TRUE)
# renv::snapshot()

# Deploy to shinyapps.io using rsconnect
if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

# You may need to run rsconnect::setAccountInfo(...) once interactively before using automated deploy.
message("Deploying app via rsconnect::deployApp()...")
rsconnect::deployApp(appDir = ".", appTitle = "mapsofindia", appPrimaryDoc = "app.R")

message("Done. If deploy failed due to missing package binaries, try building locally with renv::restore() and deploy again.")
