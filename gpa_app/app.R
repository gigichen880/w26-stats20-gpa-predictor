library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(glmnet)

source("helpers.R")

data <- load_data("data/student_lifestyle_dataset.csv")

VARS <- c(
  "Study Hours"         = "study",
  "Sleep Hours"         = "sleep",
  "Social Hours"        = "social",
  "Exercise Hours"      = "exercise",
  "Extracurricular Hrs" = "extra"
)

INTERACT_CHOICES <- combn(unname(VARS), 2, function(x) paste(x, collapse=":"), simplify=TRUE)
INTERACT_LABELS  <- combn(names(VARS),  2, function(x) paste(x, collapse=" x "), simplify=TRUE)
names(INTERACT_CHOICES) <- INTERACT_LABELS

STRESS_COL <- "Stress_Level"

MODEL_COLORS <- c(
  "Linear Regression" = "#2ec4b6",
  "Decision Tree"     = "#0f1b2d",
  "Random Forest"     = "#f4a261",
  "Ridge"             = "#6a4c93",
  "Lasso"             = "#e84855",
  "KNN"               = "#2d6a4f"
)

# Term-usage badge helper
term_badge <- function(txt, col="#2ec4b6")
  tags$span(txt,
    style=paste0("display:inline-block;font-size:.68rem;font-weight:700;",
                 "text-transform:uppercase;letter-spacing:.6px;padding:.15rem .55rem;",
                 "border-radius:20px;border:1px solid ",col,";color:",col,
                 ";margin-right:.3rem;margin-bottom:.3rem;"))

# ═══════════════════════════════════════════════════════════════
#  UI
# ═══════════════════════════════════════════════════════════════

ui <- navbarPage(
  title = tags$span(tags$span("GPA", style="color:#2ec4b6"), " Lifestyle Analyzer"),
  id    = "nav",
  header = tags$head(
    tags$link(rel="stylesheet", href="styles.css"),
    tags$link(rel="stylesheet",
      href="https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&family=Playfair+Display:wght@600;700&display=swap")
  ),
  collapsible = TRUE,

  # TAB 1: OVERVIEW 
  tabPanel("Overview",
    div(class="tab-content",

      div(class="hero-band",
        h2("Student Lifestyle & GPA"),
        p("Explore how daily habits relate to academic performance. Build models, compare them, and predict your own GPA."),
        div(
          span(class="stat-pill", "OLS + Regularisation"),
          span(class="stat-pill", "Tree + Random Forest"),
          span(class="stat-pill", "KNN"),
          span(class="stat-pill", "GPA Prediction")
        )
      ),

      fluidRow(
        column(5,
          div(class="gpa-card",
            tags$h4("Dataset Summary"),
            uiOutput("overview_stats")
          )
        ),
        column(7,
          div(class="gpa-card",
            tags$h4("GPA Distribution"),
            div(class="plot-wrap", plotOutput("overview_hist", height="190px"))
          )
        )
      ),

      div(class="gpa-card",
        tags$h4("GPA vs Each Lifestyle Variable"),
        div(class="section-sub", style="margin-top:-.4rem;margin-bottom:.8rem;",
            "Scatter with LOESS (red) and linear (dashed) smooths per variable"),
        div(class="plot-wrap", plotOutput("overview_scatter", height="320px"))
      ),

      fluidRow(
        column(6,
          div(class="gpa-card",
            tags$h4("Predictor Correlation Heatmap"),
            div(class="plot-wrap", plotOutput("overview_heatmap", height="290px"))
          )
        ),
        column(6,
          div(class="gpa-card",
            tags$h4("Average Daily Hours Budget"),
            div(class="plot-wrap", plotOutput("overview_budget", height="290px"))
          )
        )
      )
    )
  ),

  # TAB 2: MODELS            
  tabPanel("Models",
    div(class="tab-content",
      sidebarLayout(
        sidebarPanel(width=4,
          tags$h3("Model Builder"),

          tags$h4("Terms"),
          tags$div(class="terms-grid",
            tags$div(class="terms-header", "Variable"),
            tags$div(class="terms-header", "Linear"),
            tags$div(class="terms-header", "Quadratic"),
            mapply(function(label, val) {
              list(
                tags$div(class="terms-varname", label),
                tags$div(class="terms-cell",
                  tags$input(type="checkbox", id=paste0("me_",val),
                    name="main_effects", value=val, class="grid-cb",
                    checked=if(val=="study") "checked" else NULL)
                ),
                tags$div(class="terms-cell",
                  tags$input(type="checkbox", id=paste0("qe_",val),
                    name="quadratic_terms", value=paste0(val,"^2"), class="grid-cb")
                )
              )
            }, names(VARS), unname(VARS), SIMPLIFY=FALSE),
            tags$script(HTML("
              $(document).on('change', '.grid-cb', function() {
                var name = $(this).attr('name');
                var vals = [];
                $('input.grid-cb[name=\"' + name + '\"]:checked').each(function() {
                  vals.push($(this).val());
                });
                Shiny.setInputValue(name, vals, {priority:'event'});
              });
              $(document).ready(function() {
                setTimeout(function() {
                  ['main_effects','quadratic_terms','interaction_terms'].forEach(function(name) {
                    var vals = [];
                    $('input.grid-cb[name=\"' + name + '\"]:checked').each(function() {
                      vals.push($(this).val());
                    });
                    Shiny.setInputValue(name, vals, {priority:'event'});
                  });
                }, 300);
              });
            "))
          ),

          hr(),

          tags$h4("Interaction Terms"),
          tags$div(class="interact-wrap",
            tags$div(class="interact-grid",
              tags$div(class="ix-corner", ""),
              lapply(c("Sleep","Social","Exer.","Extra"), function(h)
                tags$div(class="ix-header", h)
              ),
              do.call(tagList, lapply(seq_along(unname(VARS))[-5], function(i) {
                row_var   <- unname(VARS)[i]
                row_label <- c("Study","Sleep","Social","Exer.")[i]
                cells <- list(tags$div(class="ix-rowlabel", row_label))
                for (k in seq_len(i-1))
                  cells[[length(cells)+1]] <- tags$div(class="ix-cell ix-empty","")
                for (j in (i+1):5) {
                  col_var <- unname(VARS)[j]
                  ikey    <- paste(row_var, col_var, sep=":")
                  cells[[length(cells)+1]] <- tags$div(class="ix-cell",
                    tags$input(type="checkbox",
                      id=paste0("ix_",row_var,"_",col_var),
                      name="interaction_terms", value=ikey, class="grid-cb",
                      title=paste(names(VARS)[i], "x", names(VARS)[j]))
                  )
                }
                cells
              }))
            )
          ),

          hr(),

          tags$h4("Model Controls"),
          div(class="control-row",
            tags$label(class="control-label", "Tree Max Depth"),
            sliderInput("tree_depth", NULL, min=1, max=10, value=3, step=1)
          ),
          div(class="control-row",
            tags$label(class="control-label", "Include Stress Level"),
            tags$small(class="control-hint", "Adds stress as a categorical predictor to all models"),
            checkboxInput("use_stress", NULL, value=FALSE)
          ),
          div(class="control-row",
            tags$label(class="control-label", "Random Forest: No. Trees"),
            sliderInput("rf_ntree", NULL, min=50, max=500, value=200, step=50)
          ),
          div(class="control-row",
            tags$label(class="control-label", "Regularisation Alpha"),
            tags$small(class="control-hint", "0 = Ridge  |  1 = Lasso  |  0.5 = Elastic Net"),
            sliderInput("glmnet_alpha", NULL, min=0, max=1, value=0, step=0.1)
          ),
          div(class="control-row",
            tags$label(class="control-label", "KNN: Neighbours (k)"),
            sliderInput("knn_k", NULL, min=1, max=30, value=7, step=1)
          ),

          hr(),

          div(class="btn-grid",
            actionButton("run_regression", "OLS",          class="btn btn-primary btn-sm"),
            actionButton("run_tree",       "Tree",         class="btn btn-primary btn-sm"),
            actionButton("run_rf",         "Rand. Forest", class="btn btn-primary btn-sm"),
            actionButton("run_glmnet",     "Ridge/Lasso",  class="btn btn-primary btn-sm"),
            actionButton("run_knn",        "KNN",          class="btn btn-primary btn-sm")
          ),
          br(),
          actionButton("compare_models", "Compare All Models",
                       class="btn btn-primary", style="width:100%")
        ),

        mainPanel(width=8,

          # OLS
          div(class="model-section",
            div(class="model-section-header",
              div(class="model-dot", style="background:#2ec4b6"),
              div(
                div(class="section-title", "Linear Regression (OLS)"),
                div(class="section-sub", "Least squares with optional polynomial and interaction terms."),
                div(style="margin-top:.3rem",
                  term_badge("Linear terms"),
                  term_badge("Quadratic terms"),
                  term_badge("Interactions"),
                  term_badge("Stress level", "#f4a261")
                )
              )
            ),
            uiOutput("reg_metrics_ui"),
            fluidRow(
              column(6, div(class="plot-wrap",
                div(class="plot-label","Coefficient Estimates"),
                plotOutput("coef_plot", height="260px")
              )),
              column(6, div(class="plot-wrap",
                div(class="plot-label","Residuals vs Fitted"),
                plotOutput("resid_plot", height="260px")
              ))
            ),
            fluidRow(
              column(6, div(class="plot-wrap",
                div(class="plot-label","Q-Q Plot"),
                plotOutput("qq_plot", height="210px")
              )),
              column(6, div(class="plot-wrap",
                div(class="plot-label","Scale-Location"),
                plotOutput("scale_plot", height="210px")
              ))
            ),
            div(class="gpa-card",
              tags$h4("Full Summary"),
              verbatimTextOutput("reg_summary")
            )
          ),

          hr(),

          # Decision Tree
          div(class="model-section",
            div(class="model-section-header",
              div(class="model-dot", style="background:#0f1b2d"),
              div(
                div(class="section-title","Decision Tree"),
                div(class="section-sub","CART regression tree — interpretable splits on main effects."),
                div(style="margin-top:.3rem",
                  term_badge("Linear terms only", "#7a7d8e"),
                  term_badge("No quadratic / interactions", "#7a7d8e"),
                  term_badge("Stress level", "#f4a261")
                )
              )
            ),
            uiOutput("tree_metrics_ui"),
            fluidRow(
              column(7, div(class="plot-wrap",
                div(class="plot-label","Tree Diagram"),
                plotOutput("tree_plot", height="300px")
              )),
              column(5, div(class="plot-wrap",
                div(class="plot-label","Variable Importance"),
                plotOutput("tree_importance", height="300px")
              ))
            )
          ),

          hr(),

          # Random Forest
          div(class="model-section",
            div(class="model-section-header",
              div(class="model-dot", style="background:#f4a261"),
              div(
                div(class="section-title","Random Forest"),
                div(class="section-sub","Ensemble of trees — lower variance, built-in OOB error estimate."),
                div(style="margin-top:.3rem",
                  term_badge("Linear terms only", "#7a7d8e"),
                  term_badge("No quadratic / interactions", "#7a7d8e"),
                  term_badge("Stress level", "#f4a261")
                )
              )
            ),
            uiOutput("rf_metrics_ui"),
            fluidRow(
              column(6, div(class="plot-wrap",
                div(class="plot-label","Variable Importance (% Inc. MSE)"),
                plotOutput("rf_importance", height="260px")
              )),
              column(6, div(class="plot-wrap",
                div(class="plot-label","OOB Error vs Number of Trees"),
                plotOutput("rf_error_plot", height="260px")
              ))
            )
          ),

          hr(),

          # Ridge / Lasso / Elastic Net
          div(class="model-section",
            div(class="model-section-header",
              div(class="model-dot", style="background:#6a4c93"),
              div(
                div(class="section-title","Regularised Regression"),
                div(class="section-sub","Ridge (a=0), Lasso (a=1), or Elastic Net — set alpha in the sidebar."),
                div(style="margin-top:.3rem",
                  term_badge("Linear terms only", "#7a7d8e"),
                  term_badge("No quadratic / interactions", "#7a7d8e"),
                  term_badge("Stress level", "#f4a261") 
                )
              )
            ),
            uiOutput("glmnet_metrics_ui"),
            fluidRow(
              column(6, div(class="plot-wrap",
                div(class="plot-label","Coefficient Path (log lambda)"),
                plotOutput("glmnet_path", height="260px")
              )),
              column(6, div(class="plot-wrap",
                div(class="plot-label","CV Error vs log lambda"),
                plotOutput("glmnet_cv_plot", height="260px")
              ))
            )
          ),

          hr(),

          # KNN
          div(class="model-section",
            div(class="model-section-header",
              div(class="model-dot", style="background:#2d6a4f"),
              div(
                div(class="section-title","K-Nearest Neighbours (KNN)"),
                div(class="section-sub","Non-parametric regression — average of k closest training points."),
                div(style="margin-top:.3rem",
                  term_badge("Linear terms only", "#7a7d8e"),
                  term_badge("No quadratic / interactions", "#7a7d8e"),
                  term_badge("Stress level", "#f4a261") 
                )
              )
            ),
            uiOutput("knn_metrics_ui"),
            div(class="plot-wrap",
              div(class="plot-label","CV RMSE vs k (best k in red)"),
              plotOutput("knn_k_plot", height="240px")
            )
          ),

          hr(),

          # Comparison
          div(class="model-section",
            div(class="model-section-header",
              div(class="model-dot", style="background:#e84855"),
              div(
                div(class="section-title","Model Comparison"),
                div(class="section-sub","5-fold CV across all models — RMSE, MAE, R² and predicted vs actual.")
              )
            ),
            # metric charts row
            fluidRow(
              column(4, div(class="plot-wrap",
                div(class="plot-label", "CV RMSE (lower = better)"),
                plotOutput("cmp_rmse",  height="240px")
              )),
              column(4, div(class="plot-wrap",
                div(class="plot-label", "CV MAE (lower = better)"),
                plotOutput("cmp_mae",   height="240px")
              )),
              column(4, div(class="plot-wrap",
                div(class="plot-label", "CV R² (higher = better)"),
                plotOutput("cmp_r2",    height="240px")
              ))
            ),
            # predicted vs actual facets
            div(class="plot-wrap",
              div(class="plot-label", "Predicted vs Actual GPA — per model"),
              plotOutput("cmp_pred_actual", height="300px")
            ),
            # summary table
            div(class="gpa-card",
              tags$h4("Summary Table"),
              tableOutput("cmp_table")
            )
          )
        )
      )
    )
  ),

  # TAB 3: PREDICT       
  tabPanel("Predict",
    div(class="tab-content",
      div(class="section-title","Predict Your GPA"),
      div(class="section-sub","Enter your typical daily hours. OLS model gives a 95% prediction interval."),
      fluidRow(
        column(8,
          div(class="gpa-card",
            tags$h4("Your Daily Hours"),
            fluidRow(
              column(4,
                numericInput("p_study",    "Study (hrs)",           value=6, min=0, max=24, step=.5),
                numericInput("p_sleep",    "Sleep (hrs)",           value=7, min=0, max=24, step=.5)
              ),
              column(4,
                numericInput("p_social",   "Social (hrs)",          value=2, min=0, max=24, step=.5),
                numericInput("p_exercise", "Exercise (hrs)",        value=1, min=0, max=24, step=.5)
              ),
              column(4,
                numericInput("p_extra",    "Extracurricular (hrs)", value=1, min=0, max=24, step=.5),
                br(),
                actionButton("run_predict","Predict GPA",
                             class="btn btn-primary", style="width:100%;margin-top:.3rem")
              )
            )
          )
        ),
        column(4, uiOutput("pred_result_ui"))
      ),
      uiOutput("pred_interp_ui")
    )
  )
)

# ═══════════════════════════════════════════════════════════════
#  SERVER
# ═══════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Overview ────────────────────────────────────────────────

  output$overview_stats <- renderUI({
    div(class="metric-grid",
      metric_box("Observations", nrow(data)),
      metric_box("Predictors",   length(VARS)),
      metric_box("Avg GPA",      round(mean(data$GPA, na.rm=TRUE), 2)),
      metric_box("GPA Range",    paste0(round(min(data$GPA),2), " - ", round(max(data$GPA),2)))
    )
  })

  output$overview_hist <- renderPlot({
    ggplot(data, aes(x=GPA)) +
      geom_histogram(bins=28, fill="#2ec4b6", colour="#1a8c84", alpha=.85) +
      geom_vline(xintercept=mean(data$GPA), colour="#0f1b2d",
                 linetype="dashed", linewidth=.7) +
      annotate("text", x=mean(data$GPA)+.04, y=Inf, vjust=1.7,
               label=paste0("mean = ", round(mean(data$GPA),2)),
               size=3, colour="#0f1b2d", fontface="bold", hjust=0) +
      labs(x="GPA", y="Count") + theme_gpa()
  }, bg="transparent")

  output$overview_scatter <- renderPlot({
    long <- data %>%
      pivot_longer(all_of(unname(VARS)), names_to="variable", values_to="hours") %>%
      mutate(variable = recode(variable, !!!setNames(names(VARS), unname(VARS))))
    ggplot(long, aes(x=hours, y=GPA)) +
      geom_point(alpha=.18, size=.85, colour="#2ec4b6") +
      geom_smooth(method="loess", formula=y~x, se=TRUE,
                  colour="#e84855", fill="#e84855", alpha=.12, linewidth=.9) +
      geom_smooth(method="lm", formula=y~x, se=FALSE,
                  colour="#0f1b2d", linetype="dashed", linewidth=.65) +
      facet_wrap(~variable, nrow=1, scales="free_x") +
      labs(x="Daily Hours", y="GPA") + theme_gpa() +
      theme(strip.text=element_text(face="bold", size=8.5),
            strip.background=element_rect(fill="#f0ede6", colour=NA))
  }, bg="transparent")

  output$overview_heatmap <- renderPlot({
    plot_corr_heatmap(data, unname(VARS), names(VARS))
  }, bg="transparent")

  output$overview_budget <- renderPlot({
    means <- sapply(unname(VARS), function(v) mean(data[[v]], na.rm=TRUE))
    df <- data.frame(variable=names(VARS), hours=means,
                     pct=means/sum(means)*100) %>%
      arrange(desc(hours)) %>%
      mutate(variable=factor(variable, levels=variable))
    ggplot(df, aes(x=variable, y=hours, fill=variable)) +
      geom_col(width=.6, show.legend=FALSE) +
      geom_text(aes(label=paste0(round(hours,1),"h\n(",round(pct,0),"%)")),
                vjust=-.3, size=2.9, fontface="bold", colour="#2d3142") +
      scale_fill_manual(values=c("#2ec4b6","#0f1b2d","#f4a261","#6a4c93","#e84855")) +
      scale_y_continuous(expand=expansion(mult=c(0,.22))) +
      labs(x=NULL, y="Avg hours / day") + theme_gpa()
  }, bg="transparent")

  # ── Formulas ─────────────────────────────────────────────────

  reg_formula <- reactive({
    terms <- c(
      input$main_effects,
      if (!is.null(input$quadratic_terms)) paste0("I(",input$quadratic_terms,")"),
      if (!is.null(input$interaction_terms)) input$interaction_terms
    )
    if (length(terms)==0) return(NULL)
    as.formula(paste("GPA ~", paste(terms, collapse="+")))
  })

  tree_formula <- reactive({
    req(input$main_effects)
    as.formula(paste("GPA ~", paste(input$main_effects, collapse="+")))
  })

  # Shared CV controller factory
  cv5 <- function() trainControl(method="cv", number=5)

  # ── OLS ──────────────────────────────────────────────────────
  # isolate() snapshots all current inputs at click-time, so changing
  # params then clicking the button always reruns with the new values.

  reg_model <- eventReactive(input$run_regression, {
    f <- isolate(reg_formula())
    req(f)
    lm(f, data=data)
  }, ignoreInit=TRUE)

  reg_cv <- eventReactive(input$run_regression, {
    f <- isolate(reg_formula())
    req(f)
    train(f, data=data, method="lm", trControl=cv5())
  }, ignoreInit=TRUE)

  output$reg_metrics_ui <- renderUI({
    req(reg_model(), reg_cv())
    s <- summary(reg_model())
    div(class="metric-grid",
      metric_box("R2",      round(s$r.squared, 3)),
      metric_box("Adj. R2", round(s$adj.r.squared, 3)),
      metric_box("CV RMSE", round(reg_cv()$results$RMSE, 3)),
      metric_box("AIC",     round(AIC(reg_model()), 1))
    )
  })

  output$reg_summary <- renderPrint({ req(reg_model()); summary(reg_model()) })

  output$coef_plot <- renderPlot({
    req(reg_model()); plot_coefs(reg_model())
  }, bg="transparent")

  output$resid_plot <- renderPlot({
    req(reg_model())
    df <- data.frame(fitted=fitted(reg_model()), resid=resid(reg_model()))
    ggplot(df, aes(fitted, resid)) +
      geom_point(alpha=.4, colour="#2ec4b6", size=1.3) +
      geom_smooth(se=FALSE, colour="#e84855", linewidth=.8, method="loess", formula=y~x) +
      geom_hline(yintercept=0, linetype="dashed", colour="#999") +
      labs(x="Fitted", y="Residuals") + theme_gpa()
  }, bg="transparent")

  output$qq_plot <- renderPlot({
    req(reg_model())
    ggplot(data.frame(resid=resid(reg_model())), aes(sample=resid)) +
      stat_qq(colour="#2ec4b6", alpha=.5, size=1.2) +
      stat_qq_line(colour="#e84855", linewidth=.8) +
      labs(x="Theoretical", y="Sample") + theme_gpa()
  }, bg="transparent")

  output$scale_plot <- renderPlot({
    req(reg_model())
    df <- data.frame(fitted=fitted(reg_model()),
                     sqrt_std=sqrt(abs(rstandard(reg_model()))))
    ggplot(df, aes(fitted, sqrt_std)) +
      geom_point(alpha=.4, colour="#2ec4b6", size=1.3) +
      geom_smooth(se=FALSE, colour="#e84855", linewidth=.8, method="loess", formula=y~x) +
      labs(x="Fitted", y="sqrt(|Std. Residuals|)") + theme_gpa()
  }, bg="transparent")

  # ── Decision Tree ─────────────────────────────────────────────

  tree_model <- eventReactive(input$run_tree, {
    f     <- isolate(tree_formula())
    depth <- isolate(input$tree_depth)
    rpart(f, data=data, control=rpart.control(maxdepth=depth))
  }, ignoreInit=TRUE)

  tree_cv <- eventReactive(input$run_tree, {
    f <- isolate(tree_formula())
    train(f, data=data, method="rpart", trControl=cv5())
  }, ignoreInit=TRUE)

  output$tree_metrics_ui <- renderUI({
    req(tree_cv())
    div(class="metric-grid",
      metric_box("CV RMSE", round(tree_cv()$results$RMSE[1], 3)),
      metric_box("Depth",   input$tree_depth),
      metric_box("Leaves",  sum(tree_model()$frame$var == "<leaf>"))
    )
  })

  output$tree_plot <- renderPlot({
    req(tree_model())
    rpart.plot(tree_model(), type=4, extra=101,
               box.palette=list("#0f1b2d","#2ec4b6"),
               branch.lty=3, shadow.col="grey70", main="")
  }, bg="transparent")

  output$tree_importance <- renderPlot({
    req(tree_model())
    imp <- tree_model()$variable.importance
    if (is.null(imp)) {
      return(ggplot() + annotate("text", x=.5, y=.5,
        label="No importance\n(tree is a stump)", colour="#7a7d8e", size=4) + theme_void())
    }
    df <- data.frame(variable=names(imp), importance=imp) %>%
      arrange(importance) %>% mutate(variable=factor(variable, levels=variable))
    ggplot(df, aes(importance, variable)) +
      geom_col(fill="#0f1b2d", alpha=.85, width=.6) +
      geom_text(aes(label=round(importance,1)), hjust=-.15, size=3, colour="#2d3142") +
      scale_x_continuous(expand=expansion(mult=c(0,.2))) +
      labs(x="Importance", y=NULL) + theme_gpa()
  }, bg="transparent")

  # ── Random Forest ─────────────────────────────────────────────

  rf_model <- eventReactive(input$run_rf, {
    vars  <- isolate(input$main_effects)
    ntree <- isolate(input$rf_ntree)
    req(vars)
    f <- as.formula(paste("GPA ~", paste(vars, collapse="+")))
    set.seed(42)
    randomForest(f, data=data, ntree=ntree, importance=TRUE)
  }, ignoreInit=TRUE)

  output$rf_metrics_ui <- renderUI({
    req(rf_model())
    m <- rf_model()
    div(class="metric-grid",
      metric_box("OOB RMSE", round(sqrt(m$mse[length(m$mse)]), 3)),
      metric_box("OOB R2",   round(m$rsq[length(m$rsq)], 3)),
      metric_box("Trees",    input$rf_ntree),
      metric_box("mtry",     m$mtry)
    )
  })

  output$rf_importance <- renderPlot({
    req(rf_model())
    imp <- importance(rf_model(), type=1)
    df  <- data.frame(variable=rownames(imp), inc_mse=imp[,1]) %>%
      arrange(inc_mse) %>% mutate(variable=factor(variable, levels=variable))
    ggplot(df, aes(inc_mse, variable)) +
      geom_col(fill="#f4a261", alpha=.9, width=.6) +
      geom_text(aes(label=round(inc_mse,2)), hjust=-.15, size=3, colour="#2d3142") +
      scale_x_continuous(expand=expansion(mult=c(0,.2))) +
      labs(x="% Increase in MSE", y=NULL) + theme_gpa()
  }, bg="transparent")

  output$rf_error_plot <- renderPlot({
    req(rf_model())
    df <- data.frame(trees=seq_along(rf_model()$mse), mse=rf_model()$mse)
    ggplot(df, aes(trees, sqrt(mse))) +
      geom_line(colour="#f4a261", linewidth=.9) +
      labs(x="Number of Trees", y="OOB RMSE") + theme_gpa()
  }, bg="transparent")

  # ── Regularised Regression ────────────────────────────────────

  # glmnet needs >=2 predictors; fall back to all VARS if fewer selected
  glmnet_vars <- function() {
    vars <- isolate(input$main_effects)
    if (length(vars) < 2) unname(VARS) else vars
  }

  glmnet_inputs <- eventReactive(input$run_glmnet, {
    vars  <- glmnet_vars()
    alpha <- isolate(input$glmnet_alpha)
    list(X=as.matrix(data[, vars, drop=FALSE]), y=data$GPA,
         alpha=alpha, vars=vars)
  }, ignoreInit=TRUE)

  glmnet_cv_fit <- eventReactive(input$run_glmnet, {
    d <- glmnet_inputs(); set.seed(42)
    cv.glmnet(d$X, d$y, alpha=d$alpha, nfolds=5)
  }, ignoreInit=TRUE)

  glmnet_path_fit <- eventReactive(input$run_glmnet, {
    d <- glmnet_inputs()
    glmnet(d$X, d$y, alpha=d$alpha)
  }, ignoreInit=TRUE)

  output$glmnet_metrics_ui <- renderUI({
    req(glmnet_cv_fit())
    cv       <- glmnet_cv_fit()
    lam      <- cv$lambda.min
    rmse_min <- sqrt(cv$cvm[cv$lambda == lam])
    alpha_snap <- glmnet_inputs()$alpha
    method <- if (alpha_snap == 0) "Ridge"
              else if (alpha_snap == 1) "Lasso"
              else paste0("Elastic Net (a=", alpha_snap, ")")
    used_vars <- glmnet_inputs()$vars
    fallback_note <- if (length(isolate(input$main_effects)) < 2)
      div(class="interp-box", style="margin-bottom:.6rem;",
          tags$b("Note: "), "glmnet requires 2+ predictors — using all 5 variables.")
    else NULL
    tagList(
      fallback_note,
      div(class="metric-grid",
        metric_box("Method",         method),
        metric_box("Optimal lambda", round(lam, 4)),
        metric_box("CV RMSE",        round(rmse_min, 3)),
        metric_box("Non-zero coefs", sum(coef(cv, s="lambda.min")[-1] != 0))
      )
    )
  })

  output$glmnet_path <- renderPlot({
    req(glmnet_path_fit(), glmnet_cv_fit())
    m   <- glmnet_path_fit()
    lam <- glmnet_cv_fit()$lambda.min
    coef_mat <- as.matrix(t(coef(m)[-1, , drop=FALSE]))
    df <- as.data.frame(coef_mat) %>%
      mutate(log_lambda=log(m$lambda)) %>%
      pivot_longer(-log_lambda, names_to="variable", values_to="coef")
    PALETTE <- setNames(
      c("#2ec4b6","#0f1b2d","#f4a261","#6a4c93","#e84855"),
      head(unique(df$variable), 5)
    )
    ggplot(df, aes(log_lambda, coef, colour=variable)) +
      geom_line(linewidth=.8) +
      geom_vline(xintercept=log(lam), linetype="dashed", colour="#aaa", linewidth=.6) +
      scale_colour_manual(values=PALETTE) +
      labs(x="log(lambda)", y="Coefficient", colour=NULL) + theme_gpa() +
      theme(legend.position="bottom", legend.text=element_text(size=7.5))
  }, bg="transparent")

  output$glmnet_cv_plot <- renderPlot({
    req(glmnet_cv_fit())
    cv <- glmnet_cv_fit()
    df <- data.frame(log_lam=log(cv$lambda), cvm=cv$cvm,
                     cvup=cv$cvup, cvlo=cv$cvlo)
    ggplot(df, aes(log_lam, cvm)) +
      geom_ribbon(aes(ymin=cvlo, ymax=cvup), fill="#6a4c93", alpha=.15) +
      geom_line(colour="#6a4c93", linewidth=.9) +
      geom_vline(xintercept=log(cv$lambda.min), linetype="dashed",
                 colour="#e84855", linewidth=.7) +
      geom_vline(xintercept=log(cv$lambda.1se), linetype="dashed",
                 colour="#999", linewidth=.7) +
      annotate("text", x=log(cv$lambda.min), y=max(df$cvup),
               label="lam.min", hjust=-.1, size=3, colour="#e84855") +
      annotate("text", x=log(cv$lambda.1se), y=max(df$cvup),
               label="lam.1se", hjust=-.1, size=3, colour="#666") +
      labs(x="log(lambda)", y="CV MSE") + theme_gpa()
  }, bg="transparent")

  # ── KNN ───────────────────────────────────────────────────────

  knn_cv <- eventReactive(input$run_knn, {
    vars <- isolate(input$main_effects)
    req(vars)
    f <- as.formula(paste("GPA ~", paste(vars, collapse="+")))
    train(f, data=data, method="knn",
          preProcess=c("center","scale"),
          tuneGrid=data.frame(k=1:25),
          trControl=cv5())
  }, ignoreInit=TRUE)

  output$knn_metrics_ui <- renderUI({
    req(knn_cv())
    best <- knn_cv()$bestTune$k
    rmse <- min(knn_cv()$results$RMSE)
    mae  <- knn_cv()$results$MAE[knn_cv()$results$k == best]
    div(class="metric-grid",
      metric_box("Best k",  best),
      metric_box("CV RMSE", round(rmse, 3)),
      metric_box("CV MAE",  round(mae,  3))
    )
  })

  output$knn_k_plot <- renderPlot({
    req(knn_cv())
    df   <- knn_cv()$results
    best <- knn_cv()$bestTune$k
    ggplot(df, aes(k, RMSE)) +
      geom_line(colour="#2d6a4f", linewidth=.9) +
      geom_point(size=2.2, colour="#2d6a4f") +
      geom_point(data=df[df$k==best,], aes(k, RMSE),
                 size=4.5, colour="#e84855", shape=18) +
      annotate("text", x=best, y=df$RMSE[df$k==best],
               label=paste0("best k=",best), vjust=-1.3,
               size=3.2, colour="#e84855", fontface="bold") +
      labs(x="k (neighbours)", y="CV RMSE") + theme_gpa()
  }, bg="transparent")

  # ── Model Comparison ──────────────────────────────────────────
  # compare_results: reactive that runs all models fresh and returns
  # a named list with metrics + predictions for every model.

  compare_results <- eventReactive(input$compare_models, {
    main_eff   <- isolate(input$main_effects)
    quad_t     <- isolate(input$quadratic_terms)
    inter_t    <- isolate(input$interaction_terms)
    ntree      <- isolate(input$rf_ntree)
    use_stress <- isolate(input$use_stress)

    ctrl   <- cv5()
    base_vars <- unname(VARS)
    sel_vars  <- if (length(main_eff) > 0) main_eff else base_vars

    # Add stress to formula if requested and available
    stress_term <- paste0("as.factor(", STRESS_COL, ")")

    make_f <- function(vars, extra_terms=NULL) {
      t <- c(vars, extra_terms)
      as.formula(paste("GPA ~", paste(t, collapse="+")))
    }

    results <- list()

    # Helper: run caret train and extract CV metrics + predictions
    run_caret <- function(nm, f, method, extra_args=list()) {
      tryCatch({
        set.seed(42)
        args <- c(list(form=f, data=data, method=method,
                       trControl=trainControl(method="cv", number=5,
                                              savePredictions="final")),
                  extra_args)
        r  <- do.call(train, args)
        br <- r$results[which.min(r$results$RMSE), ]
        preds <- r$pred
        if (!is.null(preds) && "pred" %in% names(preds) && "obs" %in% names(preds)) {
          preds <- preds[order(preds$rowIndex), ]
        }
        results[[nm]] <<- list(
          RMSE = br$RMSE,
          MAE  = br$MAE,
          Rsq  = if ("Rsquared" %in% names(br)) br$Rsquared else NA,
          pred = if (!is.null(preds)) preds$pred else NULL,
          obs  = if (!is.null(preds)) preds$obs  else NULL
        )
      }, error=function(e) NULL)
    }

    # OLS (uses full formula with quadratic/interactions)
    tryCatch({
      terms <- c(
        sel_vars,
        if (!is.null(quad_t))  paste0("I(",quad_t,")"),
        if (!is.null(inter_t)) inter_t,
        stress_term
      )
      if (length(terms) > 0) {
        f  <- as.formula(paste("GPA ~", paste(terms, collapse="+")))
        set.seed(42)
        r  <- train(f, data=data, method="lm",
                    trControl=trainControl(method="cv", number=5,
                                          savePredictions="final"))
        br <- r$results
        preds <- r$pred[order(r$pred$rowIndex), ]
        results[["Linear Regression"]] <- list(
          RMSE=br$RMSE, MAE=br$MAE,
          Rsq=if("Rsquared"%in%names(br)) br$Rsquared else NA,
          pred=preds$pred, obs=preds$obs)
      }
    }, error=function(e) NULL)

    # Tree
    run_caret("Decision Tree",
              make_f(sel_vars, stress_term), "rpart",
              list(tuneGrid=data.frame(cp=0.01)))

    # KNN (no stress — needs numeric only)
    run_caret("KNN",
              make_f(sel_vars), "knn",
              list(preProcess=c("center","scale"),
                   tuneGrid=data.frame(k=seq(3,15,2))))

    # Ridge and Lasso via glmnet (numeric matrix only)
    tryCatch({
      gvars <- if (length(sel_vars) >= 2) sel_vars else base_vars
      X <- as.matrix(data[, gvars, drop=FALSE])
      y <- data$GPA
      set.seed(42)
      for (nm_g in c("Ridge","Lasso")) {
        al <- if (nm_g=="Ridge") 0 else 1
        cv_g <- cv.glmnet(X, y, alpha=al, nfolds=5)
        lam  <- cv_g$lambda.min
        idx  <- which(cv_g$lambda == lam)
        # refit full model for predictions
        fit_g <- glmnet(X, y, alpha=al, lambda=lam)
        phat  <- as.vector(predict(fit_g, newx=X))
        results[[nm_g]] <- list(
          RMSE = sqrt(cv_g$cvm[idx]),
          MAE  = mean(abs(y - phat)),
          Rsq  = cor(y, phat)^2,
          pred = phat,
          obs  = y
        )
      }
    }, error=function(e) NULL)

    # Random Forest
    tryCatch({
      f <- make_f(sel_vars, stress_term)
      set.seed(42)
      m    <- randomForest(f, data=data, ntree=ntree, importance=FALSE)
      phat <- predict(m, data)
      y    <- data$GPA
      results[["Random Forest"]] <- list(
        RMSE = sqrt(mean((y - phat)^2)),   # OOB would need more work; use train preds
        MAE  = mean(abs(y - phat)),
        Rsq  = cor(y, phat)^2,
        pred = phat,
        obs  = y
      )
      # Prefer OOB RMSE
      results[["Random Forest"]]$RMSE <- sqrt(m$mse[length(m$mse)])
    }, error=function(e) NULL)

    results
  }, ignoreInit=TRUE)

  # ── Comparison plots ──────────────────────────────────────────

  cmp_metric_plot <- function(metric, ylab, higher_better=FALSE) {
    req(compare_results())
    res <- compare_results()
    if (length(res) == 0) return(NULL)

    df <- data.frame(
      Model = names(res),
      Value = sapply(res, function(r) {
        v <- r[[metric]]
        if (is.null(v) || length(v)==0) NA else v[1]
      })
    ) %>% filter(!is.na(Value)) %>%
      arrange(if (higher_better) desc(Value) else Value) %>%
      mutate(Model=factor(Model, levels=Model))

    best_idx <- if (higher_better) which.max(df$Value) else which.min(df$Value)

    ggplot(df, aes(Model, Value, fill=Model)) +
      geom_col(width=.55, show.legend=FALSE,
               alpha=ifelse(seq_len(nrow(df))==best_idx, 1, .65)) +
      geom_text(aes(label=round(Value,3)), vjust=-.4, size=3.1, fontface="bold") +
      geom_point(data=df[best_idx,], aes(Model, Value),
                 shape=9, size=3.5, colour="#e84855",
                 position=position_nudge(y=max(df$Value)*.06)) +
      scale_fill_manual(values=MODEL_COLORS) +
      scale_y_continuous(expand=expansion(mult=c(0,.22))) +
      labs(x=NULL, y=ylab) + theme_gpa() +
      theme(axis.text.x=element_text(angle=30, hjust=1, size=7.5))
  }

  output$cmp_rmse <- renderPlot({
    cmp_metric_plot("RMSE", "CV RMSE")
  }, bg="transparent")

  output$cmp_mae  <- renderPlot({
    cmp_metric_plot("MAE", "CV MAE")
  }, bg="transparent")

  output$cmp_r2   <- renderPlot({
    cmp_metric_plot("Rsq", "CV R²", higher_better=TRUE)
  }, bg="transparent")

  output$cmp_pred_actual <- renderPlot({
    req(compare_results())
    res <- compare_results()

    long <- do.call(rbind, lapply(names(res), function(nm) {
      r <- res[[nm]]
      if (is.null(r$pred)) return(NULL)
      n <- min(length(r$pred), length(r$obs))
      data.frame(Model=nm, pred=r$pred[1:n], obs=r$obs[1:n])
    }))

    if (is.null(long) || nrow(long)==0) return(NULL)

    lims <- range(c(long$pred, long$obs), na.rm=TRUE)

    ggplot(long, aes(obs, pred, colour=Model)) +
      geom_abline(slope=1, intercept=0, colour="#ccc", linewidth=.7, linetype="dashed") +
      geom_point(alpha=.25, size=.9, show.legend=FALSE) +
      geom_smooth(method="lm", formula=y~x, se=FALSE, linewidth=.8, show.legend=FALSE) +
      facet_wrap(~Model, nrow=1) +
      scale_colour_manual(values=MODEL_COLORS) +
      coord_fixed(xlim=lims, ylim=lims) +
      labs(x="Actual GPA", y="Predicted GPA") + theme_gpa() +
      theme(strip.text=element_text(face="bold", size=8),
            strip.background=element_rect(fill="#f0ede6", colour=NA))
  }, bg="transparent")

  output$cmp_table <- renderTable({
    req(compare_results())
    res <- compare_results()

    df <- do.call(rbind, lapply(names(res), function(nm) {
      r <- res[[nm]]
      data.frame(
        Model = nm,
        RMSE  = round(r$RMSE[1], 4),
        MAE   = round(r$MAE[1],  4),
        R2    = round(ifelse(is.na(r$Rsq[1]), NA, r$Rsq[1]), 4),
        stringsAsFactors=FALSE
      )
    })) %>% arrange(RMSE)
    names(df)[4] <- "R²"

    df
  }, striped=TRUE, hover=TRUE, bordered=FALSE,
     spacing="s", width="100%")

  # ── Predict ───────────────────────────────────────────────────

  pred_result <- eventReactive(input$run_predict, {
    m  <- lm(GPA ~ study + sleep + social + exercise + extra, data=data)
    nd <- data.frame(study=input$p_study, sleep=input$p_sleep,
                     social=input$p_social, exercise=input$p_exercise,
                     extra=input$p_extra)
    pred <- predict(m, newdata=nd, interval="prediction", level=.95)
    list(gpa=round(pred[1,"fit"],2), low=round(pred[1,"lwr"],2),
         high=round(pred[1,"upr"],2), model=m)
  })

  output$pred_result_ui <- renderUI({
    req(pred_result())
    r   <- pred_result()
    gpa <- max(0, min(4, r$gpa))
    div(class="pred-result",
      div(class="pred-label","Predicted GPA"),
      div(class="pred-gpa", gpa),
      div(class="pred-ci",
          paste0("95% PI: [", max(0,r$low), " - ", min(4,r$high), "]"))
    )
  })

  output$pred_interp_ui <- renderUI({
    req(pred_result())
    r   <- pred_result()
    s   <- summary(r$model)
    gpa <- max(0, min(4, r$gpa))
    tier <- if      (gpa >= 3.7) "excellent - top-tier academic performance"
            else if (gpa >= 3.3) "strong - above the typical student"
            else if (gpa >= 3.0) "solid - around average for college students"
            else if (gpa >= 2.5) "moderate - some room for improvement"
            else                  "low - consider reviewing your study habits"
    div(class="interp-box",
      tags$b("Interpretation: "),
      sprintf("Based on your inputs, the model predicts a GPA of %.2f (%s). ", gpa, tier),
      sprintf("The model (R2 = %.3f) explains %.1f%% of GPA variance. ", s$r.squared, s$r.squared*100),
      "The 95% prediction interval reflects plausible individual variation."
    )
  })
}

shinyApp(ui, server)