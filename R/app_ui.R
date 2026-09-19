#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page(
      theme = "light",
      color = "blue",
      title = "BharatViz",
      layout = "boxed",
      show_theme_button = FALSE,
      body = div(
        class = "page-body",
        div(
          class = "container-xl",
          # ---- Header: research question + honest framing -------------------
          fluidRow(
            column(
              12,
              h1("BharatViz - India state context (NFHS-5)"),
              tags$p(
                class = "text-muted",
                "Part of the Growth & Work Atlas question: ",
                tags$em("when an industry produces more value per worker, do pay and jobs improve too, and where do gains fail to reach people?"),
                " This India panel shows ", tags$strong("descriptive regional context"),
                " from the National Family Health Survey (NFHS-5, 2019-21). ",
                "It is ", tags$strong("not"), " an industrial outcome and is ",
                tags$strong("not"), " joined to any industry, wage, or productivity measure."
              ),
              tags$p(
                class = "text-muted",
                tags$strong("Read the map as context only:"),
                " co-location of a health/education indicator with economic activity is descriptive, not causal."
              )
            )
          ),
          fluidRow(
            column(
              3,
              card(
                shiny::selectInput(
                  "indicator",
                  "Indicator (NFHS-5, state total):",
                  choices = c(
                    "Women 15-49 who are anaemic (%)" = "women_anaemia",
                    "Women with 10+ years of schooling (%)" = "women_schooling10"
                  ),
                  selected = "women_schooling10"
                ),
                shiny::selectInput(
                  "gradient",
                  "Colour scale (sequential):",
                  choices = c(
                    "Viridis (colourblind-safe)" = "viridis",
                    "Plasma" = "plasma",
                    "Inferno" = "inferno",
                    "Magma" = "magma",
                    "Blue-Yellow" = "blue_yellow"
                  ),
                  selected = "viridis"
                ),
                shiny::downloadButton("download_data", "Download plotted data (CSV)"),
                tags$hr(),
                tags$div(
                  class = "text-muted", style = "font-size:0.85em;",
                  shiny::htmlOutput("legend_caption")
                )
              )
            ),
            column(
              9,
              card(
                d3po_output("plot", width = "100%", height = "560px")
              )
            )
          ),
          # ---- Ranked view (legible without colour) + methods ---------------
          fluidRow(
            column(
              7,
              card(
                tags$h3("Ranked by state"),
                tags$p(class = "text-muted", style = "font-size:0.9em;",
                       "The same values as the map, ranked, so differences are legible without relying on colour."),
                shiny::uiOutput("ranked")
              )
            ),
            column(
              5,
              card(
                tags$h3("Method & sources"),
                shiny::htmlOutput("methods")
              )
            )
          )
        )
      ),
      footer = footer(
        left = "NFHS-5 (2019-21), IIPS/MoHFW | boundaries: SimpleMaps, CC BY 4.0",
        right = shiny::tags$span("BharatViz v0.2.0")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "d3poindia"
    )
  )
}
