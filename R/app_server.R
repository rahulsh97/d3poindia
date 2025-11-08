#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import tabler
#' @import d3po
#' @importFrom dplyr count group_by left_join mutate n summarise filter
#' @noRd
app_server <- function(input, output, session) {
  state_totals <- d3poindia::emp_data %>%
    group_by(region) %>%
    summarise(total = n(), .groups = "drop")

  output$emp_year <- renderUI({
    years <- levels(d3poindia::emp_data$year)
    selectInput(
      "emp_year",
      "Year:",
      choices = years,
      selected = "2021-22",
      multiple = TRUE
    )
  })
  output$emp_gender <- renderUI({
    genders <- levels(d3poindia::emp_data$gender)
    selectInput(
      "emp_gender",
      "Gender:",
      choices = genders,
      selected = "female",
      multiple = TRUE
    )
  })
  output$emp_age <- renderUI({
    ages <- c("All" = "__all__", levels(d3poindia::emp_data$age))
    selectInput(
      "emp_age",
      "Age:",
      choices = ages,
      selected = "__all__",
      multiple = TRUE
    )
  })
  output$emp_sector <- renderUI({
    sectors <- c("All" = "__all__", levels(d3poindia::emp_data$sector))
    selectInput(
      "emp_sector",
      "Sector:",
      choices = sectors,
      selected = "__all__",
      multiple = TRUE
    )
  })
  output$emp_education <- renderUI({
    eds <- c("All" = "__all__", levels(d3poindia::emp_data$education))
    selectInput(
      "emp_education",
      "Education:",
      choices = eds,
      selected = "__all__",
      multiple = TRUE
    )
  })
  output$emp_employment_type <- renderUI({
    types <- c("All" = "__all__", levels(d3poindia::emp_data$employment_type))
    selectInput(
      "emp_employment_type",
      "Employment type:",
      choices = types,
      selected = "__all__",
      multiple = TRUE
    )
  })

  output$plot <- render_d3po({
    axis_color <- "#000"
    tooltip_color <- "#fff"

    get_palette <- function(name, n) {
      if (name == "blue_yellow") {
        return(grDevices::colorRampPalette(c("#0571b0", "#f7fcb9"))(n))
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

    # apply filters based on inputs (supporting multiple selections and '__all__')
    emp_filtered <- d3poindia::emp_data %>%
      mutate(region = as.character(region))

    if (!is.null(input$emp_year) && length(input$emp_year) > 0) {
      emp_filtered <- emp_filtered %>%
        filter(year %in% input$emp_year)
    }

    if (!is.null(input$emp_gender) && length(input$emp_gender) > 0) {
      emp_filtered <- emp_filtered %>%
        filter(gender %in% input$emp_gender)
    }

    if (
      !is.null(input$emp_age) &&
        length(input$emp_age) > 0 &&
        !("__all__" %in% input$emp_age)
    ) {
      emp_filtered <- emp_filtered %>%
        filter(age %in% input$emp_age)
    }

    if (
      !is.null(input$emp_sector) &&
        length(input$emp_sector) > 0 &&
        !("__all__" %in% input$emp_sector)
    ) {
      emp_filtered <- emp_filtered %>%
        filter(sector %in% input$emp_sector)
    }

    if (
      !is.null(input$emp_education) &&
        length(input$emp_education) > 0 &&
        !("__all__" %in% input$emp_education)
    ) {
      emp_filtered <- emp_filtered %>%
        filter(education %in% input$emp_education)
    }

    if (
      !is.null(input$emp_employment_type) &&
        length(input$emp_employment_type) > 0 &&
        !("__all__" %in% input$emp_employment_type)
    ) {
      emp_filtered <- emp_filtered %>%
        filter(employment_type %in% input$emp_employment_type)
    }

    emp_summary <- emp_filtered %>%
      group_by(region) %>%
      summarise(count = n(), .groups = "drop")

    emp_summary <- emp_summary %>%
      left_join(state_totals, by = "region") %>%
      mutate(proportion = count / total)

    value_col <- "proportion"

    d <- d3poindia::map_of_india %>%
      left_join(emp_summary, by = "region")

    pal <- get_palette(input$gradient, 5)

    d <- sf::st_as_sf(d)

    d$proportion <- round(d$proportion * 100, 2)

    print(d)

    d3po(d) %>%
      po_geomap(daes(
        group = region,
        color = pal,
        size = proportion,
        gradient = TRUE,
        tooltip = region
      )) %>%
      po_theme(axis = axis_color, tooltips = tooltip_color)
  })
}
