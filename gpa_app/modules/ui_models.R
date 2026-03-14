# Models tab UI: OLS, Decision Tree, Random Forest, Regularized OLS, KNN, Compare All Models

tab_models <- tabPanel("Models", div(class="tab-content",
  tabsetPanel(id="model_tabs", type="tabs",

    # ── OLS ───────────────────────────────────────────────────────────────────
    tabPanel("OLS", br(),
      sidebarLayout(
        sidebarPanel(width=4,
          tags$h3("Linear Regression (OLS)"),
          tags$p(class="section-sub", "Least squares with optional polynomial and interaction terms."),
          div(style="margin-bottom:.6rem",
              term_badge("Linear"), term_badge("Quadratic"), term_badge("Interactions")),
          hr(),
          tags$h4("Variables"),
          var_grid_ui("ols", show_quadratic=TRUE, default_checked="study"),
          hr(),
          tags$h4("Interaction Terms"),
          tags$small(class="control-hint", "Numeric predictors only: stress is not eligible for interactions."),
          br(),
          interaction_grid_ui("ols"),
          hr(),
          actionButton("run_ols", "Fit OLS Model", class="btn btn-primary", style="width:100%")
        ),
        mainPanel(width=8,
          uiOutput("ols_metrics_ui"),
          fluidRow(
            column(6, div(class="plot-wrap",
              div(class="plot-label", "Coefficient Estimates"),
              div(class="plot-desc", " * Points show coefficient estimates with 95% confidence intervals. 
  Bars crossing zero indicate weak or insignificant effects."),
              plotOutput("ols_coef_plot", height="250px")
            )),
            column(6, div(class="plot-wrap",
              div(class="plot-label", "Residuals vs Fitted"),
              div(class="plot-desc", " * Residuals should scatter randomly around zero. 
  Patterns or curves suggest model misspecification."),
              plotOutput("ols_resid_plot", height="250px")
            ))
          ),
          fluidRow(
            column(6, div(class="plot-wrap",
              div(class="plot-label", "Normal Q-Q"),
              div(class="plot-desc", " * Points following the diagonal indicate residuals are approximately normal."),
              plotOutput("ols_qq_plot", height="210px")
            )),
            column(6, div(class="plot-wrap",
              div(class="plot-label", "Scale-Location"),
              div(class="plot-desc", " * A flat trend indicates constant residual variance; upward trends suggest heteroscedasticity."),
              plotOutput("ols_scale_plot", height="210px")
            ))
          ),
          div(class="gpa-card", tags$h4("Full Summary"), verbatimTextOutput("ols_summary"))
        )
      )
    ),

    # ── Decision Tree ─────────────────────────────────────────────────────────
    tabPanel("Decision Tree", br(),
      sidebarLayout(
        sidebarPanel(width=4,
          tags$h3("Decision Tree"),
          tags$p(class="section-sub", "CART regression tree: interpretable splits on selected variables."),
          div(style="margin-bottom:.6rem",
              term_badge("Linear terms only", "#7a7d8e"),
              term_badge("No quadratic / interactions", "#7a7d8e")),
          hr(),
          tags$h4("Variables"),
          var_grid_ui("tree", show_quadratic=FALSE, default_checked="study"),
          hr(),
          div(class="control-row",
              tags$label(class="control-label", "Max Depth"),
              sliderInput("tree_depth", NULL, min=1, max=10, value=3, step=1)),
          hr(),
          actionButton("run_tree", "Fit Decision Tree", class="btn btn-primary", style="width:100%")
        ),
        mainPanel(width=8,
          uiOutput("tree_metrics_ui"),
          fluidRow(
            column(7, div(class="plot-wrap", div(class="plot-label", "Tree Diagram"),
                          plotOutput("tree_plot", height="320px"))),
            column(5, div(class="plot-wrap", div(class="plot-label", "Variable Importance"),
                          plotOutput("tree_importance", height="320px")))
          )
        )
      )
    ),

    # ── Random Forest ─────────────────────────────────────────────────────────
    tabPanel("Random Forest", br(),
      sidebarLayout(
        sidebarPanel(width=4,
          tags$h3("Random Forest"),
          tags$p(class="section-sub", "Ensemble of trees: lower variance, built-in OOB error estimate."),
          div(style="margin-bottom:.6rem",
              term_badge("Linear terms only", "#7a7d8e"),
              term_badge("No quadratic / interactions", "#7a7d8e")),
          hr(),
          tags$h4("Variables"),
          var_grid_ui("rf", show_quadratic=FALSE, default_checked="study"),
          hr(),
          div(class="control-row",
              tags$label(class="control-label", "Number of Trees"),
              sliderInput("rf_ntree", NULL, min=50, max=500, value=200, step=50)),
          hr(),
          actionButton("run_rf", "Fit Random Forest", class="btn btn-primary", style="width:100%")
        ),
        mainPanel(width=8,
          uiOutput("rf_metrics_ui"),
          fluidRow(
            column(6, div(class="plot-wrap", div(class="plot-label", "Variable Importance"),
                      div(class="plot-desc", " * Importance shows how much prediction MSE increases when a variable is randomly shuffled. Larger increases indicate more influential predictors."),
                          plotOutput("rf_importance", height="280px"))),
            column(6, div(class="plot-wrap", div(class="plot-label", "OOB Error vs Number of Trees"),
                          plotOutput("rf_error_plot", height="280px")))
          )
        )
      )
    ),

    # ── Ridge / Lasso ─────────────────────────────────────────────────────────
    tabPanel("Regularized OLS", br(),
      sidebarLayout(
        sidebarPanel(width=4,
          tags$h3("Regularized Regression"),
          tags$p(class="section-sub", "Ridge (alpha=0), Lasso (alpha=1), or Elastic Net."),
          div(style="margin-bottom:.6rem",
              term_badge("Linear terms only", "#7a7d8e"),
              term_badge("No quadratic / interactions", "#7a7d8e")),
          hr(),
          tags$h4("Variables"),
          br(),
          var_grid_ui("glmnet", show_quadratic=FALSE, default_checked=c("study")),
          hr(),
          div(class="control-row",
              tags$label(class="control-label", "Alpha (0 = Ridge | 1 = Lasso)"),
              sliderInput("glmnet_alpha", NULL, min=0, max=1, value=0, step=0.1)),
          hr(),
          actionButton("run_glmnet", "Fit Regularized Model", class="btn btn-primary", style="width:100%"),
          br(), br(),
          uiOutput("glmnet_warning")
        ),
        mainPanel(width=8,
          uiOutput("glmnet_metrics_ui"),
          fluidRow(
            column(6, div(class="plot-wrap", div(class="plot-label", "Coefficient Path (log lambda)"),
                      div(class="plot-desc", "* Each line shows how a predictor’s coefficient changes as the regularization strength lambda varies. Larger lambda values shrink coefficients toward zero."),
                          plotOutput("glmnet_path", height="280px"))),
            column(6, div(class="plot-wrap", div(class="plot-label", "CV Error vs log lambda"),
                      div(class="plot-desc", "* Cross-validation error for different lambda values. The dashed lines mark the optimal lambda and the largest lambda with error within 1 SE."),
                          plotOutput("glmnet_cv_plot", height="280px")))
          )
        )
      )
    ),

    # ── KNN ───────────────────────────────────────────────────────────────────
    tabPanel("KNN", br(),
      sidebarLayout(
        sidebarPanel(width=4,
          tags$h3("K-Nearest Neighbours"),
          tags$p(class="section-sub", "Non-parametric: average of k closest training points."),
          div(style="margin-bottom:.6rem",
              term_badge("Linear terms only", "#7a7d8e"),
              term_badge("No quadratic / interactions", "#7a7d8e")),
          hr(),
          tags$h4("Variables"),
          br(),
          var_grid_ui("knn", show_quadratic=FALSE, default_checked="study"),
          hr(),
          div(class="control-row",
              tags$label(class="control-label", "Max k to search"),
              sliderInput("knn_k_max", NULL, min=5, max=40, value=25, step=1)),
          hr(),
          actionButton("run_knn", "Fit KNN Model", class="btn btn-primary", style="width:100%")
        ),
        mainPanel(width=8,
          uiOutput("knn_metrics_ui"),
          div(class="plot-wrap",
              div(class="plot-label", "CV RMSE vs k (best k in red)"),
              plotOutput("knn_k_plot", height="280px"))
        )
      )
    ),

    # ── Compare All ───────────────────────────────────────────────────────────
    tabPanel("Compare All", br(),
      div(class="model-section",
        div(class="model-section-header",
          div(class="model-dot", style="background:#e84855"),
          div(
            div(class="section-title", "Model Comparison"),
            div(class="section-sub", "5-fold CV reusing each customized model fitted. Fit models first in their respective tabs.")
          )
        ),
        uiOutput("cmp_status_ui"),
        div(style="margin-bottom:1.2rem",
            actionButton("compare_models", "Run Comparison", class="btn btn-primary", style="min-width:200px")),
        fluidRow(
          column(4, div(class="plot-wrap", div(class="plot-label", "CV RMSE (lower = better)"), plotOutput("cmp_rmse", height="240px"))),
          column(4, div(class="plot-wrap", div(class="plot-label", "CV MAE (lower = better)"), plotOutput("cmp_mae", height="240px"))),
          column(4, div(class="plot-wrap", div(class="plot-label", "CV R-Squared (higher = better)"), plotOutput("cmp_r2", height="240px")))
        ),
        div(class="gpa-card", tags$h4("Summary Table"), tableOutput("cmp_table")),
        div(class="plot-wrap",
            div(class="plot-label", "Predicted vs Actual GPA"),
            div(class="plot-desc",
                      "Points show cross-validated out-of-fold predictions. Each prediction is made on data not used to train that fold."),
            plotOutput("cmp_pred_actual", height="300px"))
      )
    )
  )
))
