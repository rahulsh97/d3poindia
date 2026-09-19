#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import tabler
#' @import d3po
#' @importFrom rlang sym
#' @noRd
app_server <- function(input, output, session) {

  # Sequential, colourblind-safe by default. All options are sequential ramps.
  get_palette <- function(name, n) {
    switch(
      name,
      viridis = viridisLite::viridis(n),
      plasma  = viridisLite::plasma(n),
      inferno = viridisLite::inferno(n),
      magma   = viridisLite::magma(n),
      blue_yellow = grDevices::colorRampPalette(c("#0571b0", "#f7fcb9"))(n),
      viridisLite::viridis(n)
    )
  }

  # Reactive: the currently selected indicator's aggregate rows (one per state).
  selected <- reactive({
    ind <- input$indicator
    if (is.null(ind)) ind <- "women_schooling10"
    d <- d3poindia::nfhs_state
    d[d$indicator == ind, , drop = FALSE]
  })

  # ---- Map ------------------------------------------------------------------
  output$plot <- render_d3po({
    ind <- input$indicator
    if (is.null(ind)) ind <- "women_schooling10"
    d <- nfhs_map_data(ind)          # states with no value keep NA -> grey, never 0

    pal <- get_palette(input$gradient, 5)

    d3po(d) %>%
      po_geomap(daes(
        group = !!sym("region"),
        color = pal,
        size = !!sym("value"),   # states with NA value render in the base fill (grey = no data)
        gradient = TRUE,
        tooltip = !!sym("label")
      )) %>%
      po_theme(axis = "#000", tooltips = "#fff")
  })

  # ---- Legend / caption (units, round, source, missing rule) ---------------
  output$legend_caption <- renderUI({
    sel <- selected()
    n_missing <- sum(!(d3poindia::india_map$region %in% sel$region))
    HTML(sprintf(
      paste0(
        "<strong>%s</strong><br/>Unit: %s &middot; Round: %s<br/>",
        "Source: %s<br/>",
        "Grey = no data (never zero). States shown: %d of %d.<br/>",
        "Higher value = <strong>%s</strong>."
      ),
      sel$indicator_lab[1], sel$unit[1], sel$round[1], sel$source[1],
      nrow(sel), nrow(d3poindia::india_map),
      ifelse(sel$higher_is[1] == "worse", "worse outcome", "better outcome")
    ))
  })

  # ---- Ranked view (accessible; legible without colour) ---------------------
  output$ranked <- renderUI({
    sel <- selected()
    unit <- sel$unit[1]
    o <- order(sel$value, decreasing = TRUE)
    sel <- sel[o, ]
    mx <- max(sel$value, na.rm = TRUE)
    rows <- vapply(seq_len(nrow(sel)), function(i) {
      w <- round(100 * sel$value[i] / mx, 1)
      sprintf(
        paste0(
          "<tr><td style='text-align:right;color:#666;padding-right:6px;'>%d</td>",
          "<td style='padding-right:8px;'>%s</td>",
          "<td style='width:55%%;'><div role='img' aria-label='%s %s%s' ",
          "style='background:#0f5c6b;height:12px;width:%s%%;border-radius:2px;display:inline-block;'></div></td>",
          "<td style='text-align:right;font-variant-numeric:tabular-nums;padding-left:8px;'>%s%s</td></tr>"
        ),
        i, sel$region[i], sel$region[i], sel$value[i], unit, w, sel$value[i], unit
      )
    }, character(1))
    HTML(paste0(
      "<table style='width:100%;border-collapse:collapse;font-size:0.86rem;'>",
      "<caption style='text-align:left;color:#666;font-size:0.85em;'>",
      sel$indicator_lab[1], " (", unit, "), NFHS-5 (2019-21), ranked high to low</caption>",
      "<tbody>", paste0(rows, collapse = ""), "</tbody></table>"
    ))
  })

  # ---- Methods & sources ----------------------------------------------------
  output$methods <- renderUI({
    sel <- selected()
    HTML(sprintf(
      paste0(
        "<ul style='font-size:0.88rem;padding-left:18px;'>",
        "<li><strong>Indicator:</strong> %s (%s).</li>",
        "<li><strong>Universe:</strong> %s.</li>",
        "<li><strong>Source:</strong> %s. Round: %s.</li>",
        "<li><strong>Denominator:</strong> published state percentage from the NFHS-5 fact sheet; no re-weighting is applied.</li>",
        "<li><strong>Missing:</strong> any state without a value is shown grey, never zero.</li>",
        "<li><strong>Boundaries:</strong> SimpleMaps India state layer, CC BY 4.0 (see data-raw/MAP_SOURCE.md). A boundary depiction is not an official Government of India map.</li>",
        "<li><strong>Not causal:</strong> a descriptive regional context measure; not an industrial outcome and not attributable to wages or productivity.</li>",
        "</ul>"
      ),
      sel$indicator_lab[1], sel$unit[1], sel$universe[1], sel$source[1], sel$round[1]
    ))
  })

  # ---- Download the plotted aggregate --------------------------------------
  output$download_data <- downloadHandler(
    filename = function() paste0("nfhs5_", input$indicator, ".csv"),
    content = function(file) {
      sel <- selected()
      utils::write.csv(
        sel[, c("region", "indicator", "indicator_lab", "value", "unit", "universe", "round", "source")],
        file, row.names = FALSE
      )
    }
  )
}
