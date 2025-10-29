library(shiny)
library(tabler)
library(d3po)
library(viridisLite)
library(scales)
library(magrittr)
library(RColorBrewer)

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

    if (is.null(input$which_plot) || input$which_plot == "expenditure") {
      # expenditure map
        mean_expenditure <- readRDS("mean_expenditure.rds")
        map_india <- d3po::maps$asia$india

        # apply selected gradient to color column
        if (!is.null(input$gradient) && "color" %in% names(mean_expenditure)) {
          vals <- mean_expenditure$avg_expenditure
          pal <- get_palette(input$gradient, 100)
          mean_expenditure$color <- ifelse(is.na(vals), "#e0e0e0", scales::col_numeric(domain = range(vals, na.rm = TRUE), palette = pal)(vals))
        }

        d3po(mean_expenditure) %>%
          po_geomap(daes(group = id, color = color, size = avg_expenditure, tooltip = state_name), map = map_india) %>%
          po_background("transparent") %>%
          po_theme(axis = axis_color, tooltips = tooltip_color)
    } else if (input$which_plot == "manufacture") {
      # manufacturing map
        mean_manufacturing <- readRDS("mean_manufacture.rds")
        map_india <- d3po::maps$asia$india

        # apply selected gradient to color column
        if (!is.null(input$gradient) && "color" %in% names(mean_manufacturing)) {
          vals <- mean_manufacturing$mwdays
          pal <- get_palette(input$gradient, 100)
          mean_manufacturing$color <- ifelse(is.na(vals), "#e0e0e0", scales::col_numeric(domain = range(vals, na.rm = TRUE), palette = pal)(vals))
        }

        d3po(mean_manufacturing) %>%
          po_geomap(daes(group = id, color = color, size = mwdays, tooltip = state_name), map = map_india) %>%
          po_background("transparent") %>%
          po_theme(axis = axis_color, tooltips = tooltip_color)
    }
  })
}

shinyApp(ui, server)
