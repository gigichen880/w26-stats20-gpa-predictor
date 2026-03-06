library(shiny)
library(ggplot2)
library(dplyr)
library(GGally)

# ---------------------------
# Load dataset (TODO)
# ---------------------------
# Placeholder dataset
set.seed(1)

data <- data.frame(
  GPA = runif(200, 2.0, 4.0),
  study = rnorm(200, 4, 1.5),
  sleep = rnorm(200, 7, 1),
  social = rnorm(200, 2, 1),
  exercise = rnorm(200, 1, 0.5),
  extra = rnorm(200, 1, 0.5)
)

# ---------------------------
# Fit statistical models (TODO)
# ---------------------------

model1 <- lm(GPA ~ study + sleep + social, data=data)

model2 <- lm(GPA ~ study + sleep + social + exercise + extra, data=data)

model3 <- lm(GPA ~ study + I(study^2) + sleep + social, data=data)

# ---------------------------
# UI
# ---------------------------

ui <- navbarPage("GPA Lifestyle Analyzer",

  # ---------------------------
  # Tab 1: Introduction
  # ---------------------------
  tabPanel("Introduction",

    h2("Welcome to the GPA Lifestyle Analyzer"),

    p("This app explores how student lifestyle factors relate to GPA."),

    p("We analyze relationships between:"),

    tags$ul(
      tags$li("Study time"),
      tags$li("Sleep habits"),
      tags$li("Social life"),
      tags$li("Physical activity"),
      tags$li("Extracurricular activities")
    ),

    p("Use the tabs above to explore the data, compare models, and predict GPA.")

  ),

  # ---------------------------
  # Tab 2: Data Exploration
  # ---------------------------
  tabPanel("Data Exploration",

    sidebarLayout(

      sidebarPanel(

        selectInput(
          "xvar",
          "Select X variable",
          choices=c("study","sleep","social","exercise","extra")
        )

      ),

      mainPanel(

        h3("Scatter Plot"),

        plotOutput("scatter_plot"),

        h3("Correlation Matrix"),

        plotOutput("correlation_plot")

      )
    )
  ),

  # ---------------------------
  # Tab 3: Model Comparison
  # ---------------------------
  tabPanel("Model Comparison",

    h3("Regression Model Comparison"),

    tableOutput("model_table")

  ),

  # ---------------------------
  # Tab 4: GPA Predictor
  # ---------------------------
  tabPanel("Predict Your GPA",

    sidebarLayout(

      sidebarPanel(

        numericInput("study", "Study Hours Per Day", 4, 0, 12),
        numericInput("sleep", "Sleep Hours Per Day", 7, 0, 12),
        numericInput("social", "Social Hours Per Day", 2, 0, 12),
        numericInput("exercise", "Exercise Hours Per Day", 1, 0, 6),
        numericInput("extra", "Extracurricular Hours Per Day", 1, 0, 6),

        actionButton("predict","Predict GPA")

      ),

      mainPanel(

        uiOutput("prediction_ui"),

        br(),

        uiOutput("feedback_ui")


      )
    )
  ),

  # ---------------------------
  # Tab 5: Model Diagnostics
  # ---------------------------
  tabPanel("Diagnostics",

    h3("Residual Plot"),

    plotOutput("residual_plot"),

    h3("QQ Plot"),

    plotOutput("qq_plot")

  )
)

# ---------------------------
# SERVER
# ---------------------------

server <- function(input, output, session) {

  # ---------------------------
  # Scatter plot
  # ---------------------------
  output$scatter_plot <- renderPlot({

    ggplot(data, aes_string(x=input$xvar, y="GPA")) +
      geom_point(alpha=0.6) +
      geom_smooth(method="lm", se=FALSE, color="blue") +
      theme_minimal()

  })

  # ---------------------------
  # Correlation matrix
  # ---------------------------
  output$correlation_plot <- renderPlot({

    ggpairs(data)

  })

  # ---------------------------
  # Model comparison table (TODO)
  # ---------------------------
  output$model_table <- renderTable({

    data.frame(
      Model=c("Model 1","Model 2","Model 3"),
      AIC=c(AIC(model1), AIC(model2), AIC(model3)),
      R_squared=c(summary(model1)$r.squared,
                  summary(model2)$r.squared,
                  summary(model3)$r.squared)
    )

  })

  # ---------------------------
  # GPA prediction
  # ---------------------------
  predicted_gpa <- eventReactive(input$predict, {

    newdata <- data.frame(
      study=input$study,
      sleep=input$sleep,
      social=input$social,
      exercise=input$exercise,
      extra=input$extra
    )

    predict(model2,newdata)

  })

  output$prediction_ui <- renderUI({

    req(predicted_gpa())

    tagList(

      h3("Predicted GPA"),

      div(
        style="font-size:40px;font-weight:bold;",
        round(predicted_gpa(),2)
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



  # ---------------------------
  # Residual plot (TODO)
  # ---------------------------
  output$residual_plot <- renderPlot({

    plot(model2$fitted.values,
         model2$residuals,
         xlab="Fitted",
         ylab="Residuals")

    abline(h=0,col="red")

  })

  # ---------------------------
  # QQ plot (TODO - for all models / the best model)
  # ---------------------------
  output$qq_plot <- renderPlot({

    qqnorm(model2$residuals)
    qqline(model2$residuals)

  })

}

shinyApp(ui, server)