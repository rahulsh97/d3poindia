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
              h1("BharatViz - India state context"),
              tags$p(
                class = "text-muted",
                "Part of the Growth & Work Atlas question: ",
                tags$em("when an industry produces more value per worker, do pay and jobs improve too, and where do gains fail to reach people?"),
                " Two ", tags$strong("separate"), " descriptive layers, selected below: ",
                tags$strong("health & education context"), " (National Family Health Survey, NFHS-5, 2019-21) and ",
                tags$strong("factory-sector productivity & pay"),
                " (Annual Survey of Industries, ASI 2023-24, current prices)."
              ),
              tags$p(
                class = "text-muted",
                tags$strong("Read as descriptive only:"),
                " the two layers are ", tags$strong("not joined to each other"),
                "; ASI covers the ", tags$strong("registered factory sector only"),
                " (predominantly manufacturing, also covered electricity/gas/water/repair units; not the whole economy and not the unorganised sector) ",
                "and industry composition differs across states; ",
                "current-price levels are not real growth and no relationship shown here is causal."
              )
            )
          ),
          fluidRow(
            column(
              3,
              card(
                shiny::selectInput(
                  "indicator",
                  "Indicator (state level):",
                  choices = list(
                    "Health & education context (NFHS-5, 2019-21)" = c(
                      "Women 15-49 who are anaemic (%)" = "women_anaemia",
                      "Women with 10+ years of schooling (%)" = "women_schooling10"
                    ),
                    "Factory-sector productivity & pay (ASI 2023-24, current prices)" = c(
                      "Net value added per worker (Rs lakh/worker)" = "nva_per_worker",
                      "Wages per worker (Rs lakh/worker)" = "wages_per_worker"
                    )
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
          ),
          # ---- ASI productivity vs pay scatter (current prices) -------------
          fluidRow(
            column(
              12,
              card(
                tags$h3("Factory-sector productivity vs pay (ASI 2023-24)"),
                tags$p(class = "text-muted", style = "font-size:0.9em;",
                       "Each point is a State/UT: average ",
                       tags$strong("wages per worker"), " (x) against ",
                       tags$strong("net value added per worker"), " (y), both in Rs lakh/worker at ",
                       tags$strong("current prices"), ". ASI registered factory sector, all industries (not the whole economy). ",
                       "The left panel shows the full range (Sikkim is a small-workforce, high-value-added outlier); ",
                       "the right panel zooms to the main cluster so the other states are readable. ",
                       "A descriptive cross-section - not real growth, and not a causal relationship."),
                shiny::plotOutput("asi_scatter", height = "460px")
              )
            )
          )
        )
      ),
      footer = footer(
        left = "NFHS-5 (2019-21), IIPS/MoHFW | ASI 2023-24, MoSPI (current prices) | boundaries: SimpleMaps, CC BY 4.0",
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
