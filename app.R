library(shiny)

# -------------------------
# UI
# -------------------------

ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  titlePanel("GPA Predictor"),

  sidebarLayout(

    sidebarPanel(

      h3("Tell us about your daily routine"),

      numericInput(
        "study",
        "Study Hours Per Day",
        value = 4,
        min = 0,
        max = 24
      ),

      numericInput(
        "sleep",
        "Sleep Hours Per Day",
        value = 7,
        min = 0,
        max = 24
      ),

      numericInput(
        "social",
        "Social Hours Per Day",
        value = 2,
        min = 0,
        max = 24
      ),

      numericInput(
        "exercise",
        "Physical Activity Hours Per Day",
        value = 1,
        min = 0,
        max = 24
      ),

      numericInput(
        "extra",
        "Extracurricular Hours Per Day",
        value = 1,
        min = 0,
        max = 24
      ),

      br(),

      actionButton(
        "predict",
        "Predict My GPA",
        class = "btn-primary"
      )

    ),

    mainPanel(

      h2("Welcome!"),

      p("This app predicts GPA based on student lifestyle factors."),

      p("Fill in your daily habits and see what GPA our model predicts!"),

      br(),

      uiOutput("prediction_ui"),

      br(),

      uiOutput("feedback_ui")

    )
  )
)

# -------------------------
# SERVER
# -------------------------

server <- function(input, output, session) {

  predicted_gpa <- eventReactive(input$predict, {

    # -------------------------
    # Dummy prediction logic
    # -------------------------

    gpa <- 2.5 +
      0.15 * input$study +
      0.05 * input$sleep -
      0.03 * input$social +
      0.04 * input$exercise +
      0.02 * input$extra

    gpa <- max(min(gpa, 4), 0)

    return(round(gpa, 2))

  })

  # -------------------------
  # Show prediction
  # -------------------------

  output$prediction_ui <- renderUI({

    req(predicted_gpa())

    tagList(

      h3("Predicted GPA"),

      div(
        class = "gpa-box",
        predicted_gpa()
      ),

      p("How close are we? Tell us your real GPA!")

    )

  })

  # -------------------------
  # User feedback
  # -------------------------

  output$feedback_ui <- renderUI({

    req(predicted_gpa())

    tagList(

      numericInput(
        "real_gpa",
        "What is your actual GPA?",
        value = NA,
        min = 0,
        max = 4,
        step = 0.01
      ),

      actionButton(
        "submit_real",
        "Submit GPA"
      ),

      br(),
      br(),

      textOutput("error_text")

    )

  })

  observeEvent(input$submit_real, {

    req(input$real_gpa)

    error <- abs(input$real_gpa - predicted_gpa())

    output$error_text <- renderText({

      paste(
        "Prediction Error:",
        round(error, 2)
      )

    })

  })
}

# -------------------------
# Run app
# -------------------------

shinyApp(ui = ui, server = server)