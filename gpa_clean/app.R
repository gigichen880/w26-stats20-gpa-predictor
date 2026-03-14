# Load everything into the global environment explicitly
sys.source("globals.R",          envir=globalenv())
sys.source("modules/ui_overview.R", envir=globalenv())
sys.source("modules/ui_models.R",   envir=globalenv())
sys.source("modules/ui_predict.R",  envir=globalenv())

# Navigation Bar
ui <- navbarPage(
  title = tags$span(tags$span("GPA", style="color:#2ec4b6"), " Lifestyle Analyzer"),
  id    = "nav",
  header = tags$head(
    tags$link(rel="stylesheet", href="styles.css"),
    tags$link(rel="stylesheet",
      href="https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&family=Playfair+Display:wght@600;700&display=swap")
  ),
  collapsible = TRUE,
  tab_overview,
  tab_models,
  tab_predict
)

server <- function(input, output, session) {
  source("modules/server_overview.R", local=TRUE)
  source("modules/server_models.R",   local=TRUE)
  source("modules/server_predict.R",  local=TRUE)
}

shinyApp(ui, server)