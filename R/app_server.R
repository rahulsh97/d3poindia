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

  is_asi <- function(ind) isTRUE(ind %in% d3poindia::asi_state$indicator)

  # Reactive: the currently selected indicator's aggregate rows (one per state),
  # routed to whichever layer (NFHS or ASI) defines it.
  selected <- reactive({
    ind <- input$indicator
    if (is.null(ind)) ind <- "women_schooling10"
    indicator_rows(ind)
  })

  # ---- Map ------------------------------------------------------------------
  output$plot <- render_d3po({
    ind <- input$indicator
    if (is.null(ind)) ind <- "women_schooling10"
    d <- map_data_for(ind)           # states with no value keep NA -> grey, never 0

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
    pct  <- identical(unit, "%")
    disp <- function(v) if (pct) paste0(v, "%") else paste0(round(v, 2), " ", unit)
    o <- order(sel$value, decreasing = TRUE)
    sel <- sel[o, ]
    mx <- max(sel$value, na.rm = TRUE)
    rows <- vapply(seq_len(nrow(sel)), function(i) {
      w <- round(100 * sel$value[i] / mx, 1)
      sprintf(
        paste0(
          "<tr><td style='text-align:right;color:#666;padding-right:6px;'>%d</td>",
          "<td style='padding-right:8px;'>%s</td>",
          "<td style='width:55%%;'><div role='img' aria-label='%s %s' ",
          "style='background:#0f5c6b;height:12px;width:%s%%;border-radius:2px;display:inline-block;'></div></td>",
          "<td style='text-align:right;font-variant-numeric:tabular-nums;padding-left:8px;'>%s</td></tr>"
        ),
        i, sel$region[i], sel$region[i], disp(sel$value[i]), w, disp(sel$value[i])
      )
    }, character(1))
    HTML(paste0(
      "<table style='width:100%;border-collapse:collapse;font-size:0.86rem;'>",
      "<caption style='text-align:left;color:#666;font-size:0.85em;'>",
      sel$indicator_lab[1], " (", unit, "), ", sel$round[1], ", ranked high to low</caption>",
      "<tbody>", paste0(rows, collapse = ""), "</tbody></table>"
    ))
  })

  # ---- Methods & sources (layer-aware) -------------------------------------
  output$methods <- renderUI({
    ind <- input$indicator
    if (is.null(ind)) ind <- "women_schooling10"
    sel <- selected()
    if (is_asi(ind)) {
      HTML(sprintf(
        paste0(
          "<ul style='font-size:0.88rem;padding-left:18px;'>",
          "<li><strong>Indicator:</strong> %s (%s), %s.</li>",
          "<li><strong>Universe:</strong> %s.</li>",
          "<li><strong>Source:</strong> %s.</li>",
          "<li><strong>Definitions (matched):</strong> pay = <em>Wages to Workers &divide; Number of Workers</em> ",
          "(numerator and denominator refer to the same workers); productivity = ",
          "<em>Net Value Added &divide; Number of Workers</em> (NVA is the directly-published ",
          "value-added measure). Both from ASI Table 4, All Industries.</li>",
          "<li><strong>Current prices:</strong> nominal Rupees for one year (2023-24); these are ",
          "<strong>level</strong> differences, not real or inflation-adjusted, and not a trend.</li>",
          "<li><strong>Coverage:</strong> \"All Industries\" is the ASI <strong>registered factory sector</strong> ",
          "(Factories Act units + bidi/cigar establishments + non-CEA electricity undertakings) across all covered NIC groups - ",
          "predominantly manufacturing, but its statutory scope also includes electricity generation/transmission, gas &amp; water/cold storage and repair. ",
          "It is <strong>not the whole economy</strong> and <strong>excludes the unorganised sector</strong>. ",
          "Industry composition differs across states, so cross-state differences partly reflect what each state makes.</li>",
          "<li><strong>Missing:</strong> any state without a value is shown grey, never zero.</li>",
          "<li><strong>Boundaries:</strong> SimpleMaps India state layer, CC BY 4.0 (see data-raw/MAP_SOURCE.md). ",
          "A boundary depiction is not an official Government of India map.</li>",
          "<li><strong>Not causal:</strong> a descriptive cross-section; higher productivity is not shown to ",
          "cause higher pay.</li>",
          "</ul>"
        ),
        sel$indicator_lab[1], sel$unit[1], sel$round[1], sel$universe[1], sel$source[1]
      ))
    } else {
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
    }
  })

  # ---- Scatter: ASI productivity vs pay by state (current prices) ----------
  # Two panels: (left) full range, so Sikkim's true value is shown; (right) a
  # zoom to the main cluster so the other 35 states stay readable. Sikkim is not
  # hidden and its value is unchanged - it is marked off-scale on the right.
  output$asi_scatter <- renderPlot({
    w <- d3poindia::asi_state[d3poindia::asi_state$indicator == "wages_per_worker",
                              c("region", "value")]
    p <- d3poindia::asi_state[d3poindia::asi_state$indicator == "nva_per_worker",
                              c("region", "value")]
    d <- merge(w, p, by = "region", suffixes = c("_wages", "_nva"))
    sk <- d$region == "Sikkim"
    xlab <- "Wages per worker (Rs lakh/worker, current prices)"
    ylab <- "Net value added per worker (Rs lakh/worker, current prices)"
    accent <- "#b5651d"

    # Stack the two panels on narrow (mobile) viewports; side-by-side otherwise.
    w_px <- session$clientData[["output_asi_scatter_width"]]
    narrow <- is.numeric(w_px) && length(w_px) == 1 && w_px < 640
    op <- graphics::par(mfrow = if (isTRUE(narrow)) c(2, 1) else c(1, 2),
                        mar = c(4.4, 4.4, 3.0, 1.0),
                        oma = c(if (isTRUE(narrow)) 3.4 else 2.4, 0, 0, 0))
    on.exit(graphics::par(op), add = TRUE)

    # Panel 1 - full range (Sikkim visible with its true value)
    graphics::plot(d$value_wages, d$value_nva, pch = 19, col = "#0f5c6b",
                   cex = 1.1, xlab = "", ylab = "",
                   main = "All States/UTs (full range)")
    graphics::title(xlab = xlab, ylab = ylab)
    graphics::grid(col = "#e6e6e6")
    graphics::points(d$value_wages[sk], d$value_nva[sk], pch = 1, cex = 2.4,
                     lwd = 2, col = accent)
    graphics::text(d$value_wages[sk], d$value_nva[sk],
                   labels = sprintf("Sikkim (%.1f)", d$value_nva[sk]),
                   pos = 2, cex = 0.82, col = accent)

    # Panel 2 - zoom to the main cluster (Sikkim off scale, marked, value shown)
    ymax <- 27
    graphics::plot(d$value_wages, d$value_nva, pch = 19, col = "#0f5c6b",
                   cex = 1.2, xlab = "", ylab = "", ylim = c(0, ymax),
                   xpd = FALSE,
                   main = if (isTRUE(narrow)) "Main cluster (zoom)" else
                     "Main cluster (zoom; Sikkim off scale)")
    graphics::title(xlab = xlab, ylab = ylab)
    graphics::grid(col = "#e6e6e6")
    lab_states <- c("Maharashtra", "Gujarat", "Tamil Nadu", "Karnataka",
                    "Kerala", "Bihar", "Uttarakhand", "Punjab")
    li <- d$region %in% lab_states
    graphics::text(d$value_wages[li], d$value_nva[li], labels = d$region[li],
                   pos = 3, cex = 0.72, col = "#333")
    # Sikkim is off the top of this zoom: mark its true x with an arrow, and put
    # the value note in the clear top-left corner (no true value changed/hidden).
    xs <- d$value_wages[sk]
    graphics::arrows(xs, ymax * 0.80, xs, ymax * 0.98, length = 0.08,
                     lwd = 2, col = accent, xpd = FALSE)
    graphics::text(min(d$value_wages), ymax * 0.96, adj = c(0, 1),
                   labels = sprintf("Sikkim off scale: %.1f (wages %.2f)",
                                    d$value_nva[sk], d$value_wages[sk]),
                   cex = 0.76, col = accent)

    if (isTRUE(narrow)) {
      # side=1 outer: smaller line = higher, so the lead clause takes the smaller line.
      graphics::mtext("ASI 2023-24 (current prices). Descriptive cross-section;",
                      side = 1, outer = TRUE, line = 1.5, cex = 0.62, col = "#666")
      graphics::mtext("registered factory sector (not whole economy); not real growth, not causal.",
                      side = 1, outer = TRUE, line = 2.4, cex = 0.62, col = "#666")
    } else {
      graphics::mtext(
        paste("ASI 2023-24 (current prices). Descriptive cross-section;",
              "ASI = registered factory sector, all industries (not the whole economy);",
              "not real growth and not causal."),
        side = 1, outer = TRUE, line = 0.8, cex = 0.76, col = "#666"
      )
    }
  })

  # ---- Download the plotted aggregate --------------------------------------
  output$download_data <- downloadHandler(
    filename = function() {
      ind <- input$indicator
      if (is.null(ind)) ind <- "women_schooling10"
      prefix <- if (is_asi(ind)) "asi2023_24_" else "nfhs5_"
      paste0(prefix, ind, ".csv")
    },
    content = function(file) {
      sel <- selected()
      utils::write.csv(
        sel[, c("region", "indicator", "indicator_lab", "value", "unit", "universe", "round", "source")],
        file, row.names = FALSE
      )
    }
  )
}
