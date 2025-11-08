#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page(
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
                # persistent gradient selector
                shiny::selectInput(
                  "gradient",
                  "Select gradient:",
                  choices = c(
                    "Blue-Yellow" = "blue_yellow",
                    "Viridis" = "viridis",
                    "Plasma" = "plasma",
                    "Inferno" = "inferno",
                    "Magma" = "magma"
                  ),
                  selected = "blue_yellow"
                ),
                # persistent employment filters
                shiny::uiOutput("emp_year"),
                shiny::uiOutput("emp_gender"),
                shiny::uiOutput("emp_age"),
                shiny::uiOutput("emp_sector"),
                shiny::uiOutput("emp_education"),
                shiny::uiOutput("emp_employment_type")
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
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
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
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
