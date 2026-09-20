#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    tags$style(HTML(atlas_styles)),
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
          div(class = "atlas-hero",
              div(class = "atlas-kicker", "THE GROWTH & WORK ATLAS / INDIA"),
              h1("What does factory output mean for workers?"),
              tags$p("Explore value added and wages per worker across India's registered factory sector. Pick two states, compare the same measures, and inspect each measure on the map."),
              div(class = "atlas-hero-foot", tags$span("ASI 2023-24"),
                  tags$span("36 states and UTs"), tags$span("Published aggregates, current prices"))
          ),
          fluidRow(column(12, card(
            div(class = "atlas-section-intro",
                div(class = "atlas-kicker", "01 / ASK A QUESTION"),
                h2("Choose the states you want to understand"),
                tags$p("Search for one or two states. The profile, ranked list and scatter respond to your selection.")),
            shiny::selectizeInput("focus_states", "States or Union Territories",
                choices = sort(unique(as.character(d3poindia::india_map$region))),
                selected = c("Maharashtra", "Tamil Nadu"), multiple = TRUE,
                options = list(maxItems = 2, placeholder = "Search for up to two states")),
            shiny::uiOutput("focus_story")
          ))),
          fluidRow(column(12, card(
            div(class = "atlas-section-intro",
                div(class = "atlas-kicker", "02 / FACTORY VALUE & PAY"),
                h2("See every state, including the outlier"),
                tags$p("Each dot compares wages per worker with net value added per worker. Your chosen states are circled in amber. The full view keeps Sikkim visible; the second view makes the main cluster legible.")),
            shiny::plotOutput("asi_scatter", height = "540px"),
            tags$p(class = "atlas-fineprint", "ASI covers registered factories, all covered industries. State industry mix differs. Nominal levels show neither growth nor a causal effect of productivity on pay.")
          ))),
          fluidRow(column(12, card(
            div(class = "atlas-section-intro",
                div(class = "atlas-kicker", "03 / EXPLORE THE GEOGRAPHY"),
                h2("Change the measure on the map"),
                tags$p("Pick an ASI measure or switch to the separate NFHS health and education context. Grey always means no data."))
          ))),
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
                  selected = "nva_per_worker"
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
                tags$h3("Every state, ranked"),
                tags$p(class = "text-muted", style = "font-size:0.9em;",
                       "The same values as the map, ranked, so differences are legible without relying on colour."),
                shiny::uiOutput("ranked")
              )
            ),
            column(
              5,
              card(
                tags$h3("How to read this"),
                shiny::htmlOutput("methods")
              )
            )
          ),
          fluidRow(column(12, card(
            div(class = "atlas-section-intro",
                div(class = "atlas-kicker", "04 / HUMAN DEVELOPMENT CONTEXT"),
                h2("What the health survey says about these states"),
                tags$p("NFHS-5 describes women in a different survey and period (2019-21). These percentages do not explain or measure factory wages or productivity.")),
            shiny::uiOutput("context_story")
          )))
        )
      ),
      footer = footer(
        left = "NFHS-5 (2019-21), IIPS/MoHFW | ASI 2023-24, MoSPI (current prices) | boundaries: SimpleMaps, CC BY 4.0",
        right = shiny::tags$span("Growth & Work Atlas / research preview")
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

atlas_styles <- "
body, .page-body { background: #f2f5f5; color: #173244; }
.page-body .container-xl { max-width: 1320px; }
.page-body .row { margin-bottom: 1rem; }
.page-body .card { border: 1px solid #dce6e6; border-radius: 16px;
  box-shadow: 0 8px 28px rgba(17,55,68,.055); padding: 1.1rem; background: #fff; }
.atlas-hero { background: #102e3b; color: #fff; border-radius: 20px;
  padding: clamp(1.5rem, 4vw, 3rem); margin: 1.2rem 0 1.35rem;
  background-image: radial-gradient(circle at 85% 20%, rgba(40,176,157,.30), transparent 38%); }
.atlas-kicker { color: #188b7e; font-size: .72rem; font-weight: 800;
  letter-spacing: .14em; text-transform: uppercase; margin-bottom: .5rem; }
.atlas-hero .atlas-kicker { color: #76e1c5; }
.atlas-hero h1 { color: #fff; max-width: 800px; font-size: clamp(2rem,4vw,3.4rem);
  line-height: 1.12; letter-spacing: -.035em; margin: .4rem 0 1rem; }
.atlas-hero p { color: #d9e9e7; max-width: 740px; font-size: 1.07rem; line-height: 1.6; }
.atlas-hero-foot { display: flex; flex-wrap: wrap; gap: .6rem; margin-top: 1.8rem; }
.atlas-hero-foot span { border: 1px solid #72939a; border-radius: 100px;
  padding: .3rem .75rem; font-size: .76rem; }
.atlas-section-intro h2 { color: #102e3b; font-size: clamp(1.25rem,2vw,1.65rem);
  letter-spacing: -.02em; margin: 0 0 .35rem; }
.atlas-section-intro p { color: #536d75; margin-bottom: 1rem; max-width: 820px; }
.atlas-state-grid, .atlas-context-grid { display: grid; grid-template-columns:
  repeat(auto-fit,minmax(min(100%,270px),1fr)); gap: .9rem; margin-top: .8rem; }
.atlas-state-card { background: #f3f8f7; border: 1px solid #dce9e5;
  border-radius: 13px; padding: 1.1rem; }
.atlas-state-name { font-size: 1.2rem; font-weight: 800; color: #133747; margin-bottom: 1rem; }
.atlas-metric { margin: .75rem 0; }
.atlas-metric-head { display: flex; justify-content: space-between;
  gap: .6rem; font-size: .84rem; color: #34515a; }
.atlas-metric-head strong { white-space: nowrap; color: #173244; }
.atlas-bar-track { height: 8px; border-radius: 10px; background: #dfebeb;
  overflow: hidden; margin-top: .4rem; }
.atlas-bar-fill { height: 100%; border-radius: 10px; }
.atlas-comparison { border-left: 3px solid #e5a448; background: #fffbf1;
  color: #4a4235; padding: .75rem 1rem; margin: 1rem 0 .6rem; }
.atlas-fineprint { color: #5d727a; font-size: .82rem; line-height: 1.5; margin: .8rem 0 .1rem; }
.atlas-context-card { border-left: 3px solid #7188a3; padding: .7rem 1rem;
  background: #f3f6f9; border-radius: 0 10px 10px 0; }
.atlas-context-card strong { display: block; font-size: 1.1rem; margin-bottom: .5rem; }
.atlas-context-card > div { display: flex; justify-content: space-between;
  gap: .7rem; font-size: .83rem; padding: .3rem 0; }
.atlas-context-card b { white-space: nowrap; }
.atlas-row-active { background: #fff4da; border-left: 3px solid #e5a448; }
@media (max-width:640px) {
  .page-body .card { padding: .75rem; }
  .atlas-metric-head { flex-wrap: wrap; }
  .atlas-hero { margin-top: .5rem; }
}
"
