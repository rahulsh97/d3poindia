library(shiny)
library(tabler)
library(d3po)
library(viridisLite)
library(scales)
library(magrittr)
library(RColorBrewer)
library(dplyr)
library(sf)

# silence R CMD check notes for dplyr NSE variables used in the app
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("country", "region", "geometry", "color", "avg_expenditure", "mwdays", "state_name"))
}

ui <- page(
  theme = "light",
  color = "blue",
  title = "D3po demo",
  layout = "boxed",
  show_theme_button = FALSE,
  body = div(
    class = "page-body",
    div(
      class = "container-xl",
      fluidRow(
        column(12, h1("Expenditure and Manufacturing Maps of India")),
        column(
          3,
          card(
            shiny::selectInput("which_plot", "Choose plot:",
              choices = c(
                "Average expenditure" = "expenditure",
                "Average manufacturing days" = "manufacture"
              ),
              selected = "expenditure"
            ),
            # new: gradient selector
            shiny::selectInput("gradient", "Select gradient:",
              choices = c(
                "Blue-Yellow" = "blue_yellow",
                "Viridis" = "viridis",
                "Plasma" = "plasma",
                "Inferno" = "inferno",
                "Magma" = "magma"
              ),
              selected = "blue_yellow"
            )
          )
        ),
        column(
          9,
          card(
            footer = "Based on the Periodic Labour Force Survey (PLFS) and the Annual Survey of Industries (ASI).",
            d3po_output("plot", width = "100%", height = "700px")
          )
        )
      )
    )
  ),
  footer = footer(
    left = "Tabler",
    right = shiny::tags$span("v1.4.0")
  )
)

server <- function(input, output, session) {
  output$plot <- render_d3po({
    # try to load precomputed data files saved by the dashboard Rmd
    axis_color <- "#000"
    tooltip_color <- "#fff"

    # helper: return a vector of hex colors for n values based on selected gradient
    get_palette <- function(name, n) {
      if (name == "blue_yellow") {
        return(colorRampPalette(c("#0571b0", "#f7fcb9"))(n))
      }
      if (name == "viridis") {
        return(viridisLite::viridis(n))
      }
      if (name == "plasma") {
        return(viridisLite::plasma(n))
      }
      if (name == "inferno") {
        return(viridisLite::inferno(n))
      }
      if (name == "magma") {
        return(viridisLite::magma(n))
      }
      # fallback
      return(viridisLite::viridis(n))
    }

    # Determine which dataset to use and normalize to `mean_data` with a `value` column
    if (is.null(input$which_plot) || input$which_plot == "expenditure") {
      mean_data <- readRDS("mean_expenditure.rds")
      value_col <- "avg_expenditure"
      size_mapping <- rlang::sym("avg_expenditure")
      manufacture_mode <- FALSE
    } else {
      mean_data <- readRDS("mean_manufacture.rds")
      # ensure the manufacturing dataset uses a consistent name
      value_col <- "mwdays"
      size_mapping <- rlang::sym("mwdays")
      manufacture_mode <- TRUE
    }

    map_india <- d3po::subnational %>%
      filter(country == "India") %>%
      select(region, geometry) %>%
      filter(region != "Ladakh") %>%
      mutate(
        region = case_when(
          region == "Dadra and Nagar Haveli and Daman and Diu" ~ "Daman and Diu",
          region == "Odisha" ~ "Orissa",
          region == "Uttarakhand" ~ "Uttaranchal",
          TRUE ~ region
        )
      ) %>%
      mutate(region = as.character(region))

    # apply selected gradient to color column if the dataset has the target metric
    if (!is.null(input$gradient) && value_col %in% names(mean_data)) {
      vals <- mean_data[[value_col]]
      pal <- get_palette(input$gradient, 5)
    }

    # join the metric data onto the map
    map_india <- map_india %>%
      left_join(mean_data, by = c("region" = "state_name"))

    # build the d3po map. use the same color mapping; size depends on chosen plot
    d3po(map_india) %>%
      po_geomap(daes(group = region, color = pal, size = !!size_mapping, gradient = TRUE, tooltip = region)) %>%
      po_theme(axis = axis_color, tooltips = tooltip_color)
  })
}

shinyApp(ui, server)
