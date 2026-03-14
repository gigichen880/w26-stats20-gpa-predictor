# Overview tab UI
tab_overview <- tabPanel("Overview", div(class="tab-content",
  div(class="hero-band",
    h2("Student Lifestyle & GPA"),
    p("Explore how daily hour distribution relate to academic performance. Customize models, compare them, and predict your own GPA."),
    div(lapply(c("OLS","Decision Tree","Random Forest","OLS + Regularization", "KNN","GPA Prediction"),
               function(x) span(class="stat-pill", x)))
  ),
  fluidRow(
    column(5, div(class="gpa-card", tags$h4("Dataset Summary"), uiOutput("overview_stats"))),
    column(7, div(class="gpa-card", tags$h4("GPA Distribution"),
                  div(class="plot-wrap", plotOutput("overview_hist", height="190px"))))
  ),
  div(class="gpa-card",
    tags$h4("GPA vs Each Numerical Lifestyle Variable"),
    div(class="section-sub", style="margin-top:-.4rem;margin-bottom:.8rem;",
        "Scatter with LOESS (red) and linear (dashed) smooths per variable"),
    div(class="plot-wrap", plotOutput("overview_scatter", height="320px"))
  ),
  fluidRow(
    column(6, div(class="gpa-card", tags$h4("Predictor Correlation Heatmap"),
                  div(class="plot-wrap", plotOutput("overview_heatmap", height="290px")))),
    column(6, div(class="gpa-card", tags$h4("GPA by Stress Level"),
                  plotOutput("overview_stress")))
  )
))
