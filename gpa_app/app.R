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

data_raw <- load_data("data/student_lifestyle_dataset.csv")

# Encode Stress_Level as a single ordered numeric: Low=0, Moderate=1, High=2.
# This treats stress as one predictor with a linear dose-response interpretation:
# each step up in stress changes GPA by a single coefficient β.
data_raw$stress <- as.integer(factor(data_raw$Stress_Level,
                                      levels = c("Low","Moderate","High"))) - 1L
data <- data_raw

# ── Variable definitions ─────────────────────────────────────────
NUMERIC_VARS <- c(
  "Study Hours"         = "study",
  "Sleep Hours"         = "sleep",
  "Social Hours"        = "social",
  "Exercise Hours"      = "exercise",
  "Extracurricular Hrs" = "extra"
)

MODEL_COLORS <- c(
  "Linear Regression" = "#2ec4b6",
  "Decision Tree"     = "#0f1b2d",
  "Random Forest"     = "#f4a261",
  "Ridge"             = "#6a4c93",
  "Lasso"             = "#e84855",
  "KNN"               = "#2d6a4f"
)

# ── UI helpers ───────────────────────────────────────────────────

term_badge <- function(txt, col = "#2ec4b6")
  tags$span(txt, style = paste0(
    "display:inline-block;font-size:.68rem;font-weight:700;",
    "text-transform:uppercase;letter-spacing:.6px;padding:.15rem .55rem;",
    "border-radius:20px;border:1px solid ", col, ";color:", col,
    ";margin-right:.3rem;margin-bottom:.3rem;"
  ))

# Renders a clean HTML <table> for variable selection.
# show_quadratic=TRUE adds a third column (OLS only).
# Stress Level always appears at bottom with a separator.
var_grid_ui <- function(ns, show_quadratic = FALSE,
                        default_checked = "study") {
  cb_name_me <- paste0(ns, "_main_effects")
  cb_name_qe <- paste0(ns, "_quadratic_terms")
  cb_cls     <- paste0("vgcb-", ns)

  col_headers <- if (show_quadratic)
    c("Variable", "Linear", "Quadratic")
  else
    c("Variable", "Include")

  th_style_var  <- "padding:.38rem .7rem;font-size:.67rem;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#2ec4b6;text-align:left;"
  th_style_chk  <- "padding:.38rem .7rem;font-size:.67rem;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#2ec4b6;text-align:center;width:62px;"
  td_style_var  <- "padding:.32rem .7rem;font-size:.83rem;color:#2d3142;border-top:1px solid #eee;"
  td_style_chk  <- "padding:.32rem .7rem;text-align:center;border-top:1px solid #eee;"
  td_style_var_s <- "padding:.32rem .7rem;font-size:.83rem;color:#7a7d8e;font-style:italic;border-top:2px dashed #ddd8ce;background:#fdfaf6;"
  td_style_chk_s <- "padding:.32rem .7rem;text-align:center;border-top:2px dashed #ddd8ce;background:#fdfaf6;"

  make_numeric_row <- function(label, val) {
    is_checked <- val %in% default_checked
    cb_me <- tags$input(type="checkbox", name=cb_name_me, value=val,
                        class=paste("var-grid-cb", cb_cls),
                        style="width:15px;height:15px;accent-color:#2ec4b6;cursor:pointer;",
                        checked=if(is_checked) NA else NULL)
    if (show_quadratic) {
      cb_qe <- tags$input(type="checkbox", name=cb_name_qe, value=paste0(val,"^2"),
                          class=paste("var-grid-cb", cb_cls),
                          style="width:15px;height:15px;accent-color:#2ec4b6;cursor:pointer;")
      tags$tr(
        tags$td(style=td_style_var, label),
        tags$td(style=td_style_chk, cb_me),
        tags$td(style=td_style_chk, cb_qe)
      )
    } else {
      tags$tr(
        tags$td(style=td_style_var, label),
        tags$td(style=td_style_chk, cb_me)
      )
    }
  }

  # Stress row — numeric 0/1/2, so quadratic IS allowed here
  stress_cb <- tags$input(type="checkbox", name=cb_name_me, value="stress",
                           class=paste("var-grid-cb", cb_cls),
                           style="width:15px;height:15px;accent-color:#2ec4b6;cursor:pointer;")
  stress_qe_cb <- tags$input(type="checkbox", name=cb_name_qe, value="stress^2",
                              class=paste("var-grid-cb", cb_cls),
                              style="width:15px;height:15px;accent-color:#2ec4b6;cursor:pointer;")
  stress_row <- if (show_quadratic) {
    tags$tr(
      tags$td(style=td_style_var_s, tagList("Stress Level ", tags$span(style="font-size:.68rem;color:#aaa;font-style:normal;", "(0=Low 1=Mod 2=High)"))),
      tags$td(style=td_style_chk_s, stress_cb),
      tags$td(style=td_style_chk_s, stress_qe_cb)
    )
  } else {
    tags$tr(
      tags$td(style=td_style_var_s, tagList("Stress Level ", tags$span(style="font-size:.68rem;color:#aaa;font-style:normal;", "(0/1/2)"))),
      tags$td(style=td_style_chk_s, stress_cb)
    )
  }

  numeric_rows <- lapply(seq_along(NUMERIC_VARS), function(i)
    make_numeric_row(names(NUMERIC_VARS)[i], unname(NUMERIC_VARS)[i])
  )

  tagList(
    tags$table(
      style = "width:100%;border-collapse:collapse;border:1px solid #e4e0d8;border-radius:8px;overflow:hidden;font-family:'DM Sans',sans-serif;",
      tags$thead(
        tags$tr(style = "background:#0f1b2d;",
          lapply(col_headers, function(h) {
            s <- if (h == "Variable") th_style_var else th_style_chk
            tags$th(style=s, h)
          })
        )
      ),
      tags$tbody(c(numeric_rows, list(stress_row)))
    ),
    tags$script(HTML(sprintf("
      $(document).on('change', '.%s', function() {
        var nm = $(this).attr('name');
        var vals = [];
        $('input.%s[name=\"' + nm + '\"]:checked').each(function() { vals.push($(this).val()); });
        Shiny.setInputValue(nm, vals, {priority:'event'});
      });
      $(document).ready(function() {
        setTimeout(function() {
          ['%s','%s'].forEach(function(nm) {
            var vals = [];
            $('input.%s[name=\"' + nm + '\"]:checked').each(function() { vals.push($(this).val()); });
            Shiny.setInputValue(nm, vals, {priority:'event'});
          });
        }, 350);
      });
    ", cb_cls, cb_cls, cb_name_me, cb_name_qe, cb_cls)))
  )
}

# Interaction grid — numeric vars only (5×5 upper triangle)
interaction_grid_ui <- function(ns) {
  vars   <- unname(NUMERIC_VARS)
  labels <- names(NUMERIC_VARS)
  abbrev <- c("Study","Sleep","Social","Exer.","Extra")
  n      <- length(vars)
  cb_cls <- paste0("vgcb-ix-", ns)
  cb_nm  <- paste0(ns, "_interaction_terms")

  col_ths <- lapply(2:n, function(j)
    tags$th(
      style="text-align:center;width:38px;font-size:.63rem;font-weight:700;text-transform:uppercase;letter-spacing:.5px;color:#1a8c84;padding:.25rem .1rem .35rem;border-bottom:2px solid #2ec4b6;white-space:nowrap;",
      abbrev[j]
    )
  )

  rows <- lapply(1:(n-1), function(i) {
    cells <- list(
      tags$td(style="font-size:.72rem;font-weight:600;color:#2d3142;padding:.22rem .5rem .22rem 0;white-space:nowrap;",
              abbrev[i])
    )
    for (k in seq_len(i-1))
      cells[[length(cells)+1]] <- tags$td(style="width:38px;")
    for (j in (i+1):n) {
      ikey <- paste(vars[i], vars[j], sep=":")
      cells[[length(cells)+1]] <- tags$td(
        style="width:38px;padding:2px;",
        tags$div(
          style="display:flex;justify-content:center;align-items:center;width:30px;height:30px;border-radius:5px;background:#f8f6f0;margin:auto;",
          tags$input(type="checkbox", name=cb_nm, value=ikey,
                     class=paste("var-grid-cb", cb_cls),
                     title=paste(labels[i],"×",labels[j]),
                     style="width:14px;height:14px;accent-color:#2ec4b6;cursor:pointer;margin:0;")
        )
      )
    }
    tags$tr(cells)
  })

  tagList(
    tags$div(style="overflow-x:auto;",
      tags$table(
        style="border-collapse:separate;border-spacing:3px;width:fit-content;font-family:'DM Sans',sans-serif;",
        tags$thead(tags$tr(c(list(tags$th(style="width:44px;")), col_ths))),
        tags$tbody(rows)
      )
    ),
    tags$script(HTML(sprintf("
      $(document).on('change', '.%s', function() {
        var vals = [];
        $('input.%s:checked').each(function() { vals.push($(this).val()); });
        Shiny.setInputValue('%s', vals, {priority:'event'});
      });
    ", cb_cls, cb_cls, cb_nm)))
  )
}

cv5 <- function(save_preds = FALSE)
  trainControl(method="cv", number=5,
               savePredictions=if(save_preds) "final" else "none")

# Build formula — stress is now plain numeric "stress", no special handling needed
build_formula <- function(main_vars, quad=NULL, inter=NULL) {
  if (is.null(main_vars) || length(main_vars)==0) return(NULL)
  terms <- c(main_vars,
             if (!is.null(quad))  paste0("I(",quad,")"),
             if (!is.null(inter)) inter)
  as.formula(paste("GPA ~", paste(terms, collapse="+")))
}

get_inp <- function(input, nm) {
  v <- input[[nm]]; if (is.null(v)||length(v)==0) NULL else v
}

# ═══════════════════════════════════════════════════════════════
#  UI
# ═══════════════════════════════════════════════════════════════
ui <- navbarPage(
  title = tags$span(tags$span("GPA",style="color:#2ec4b6")," Lifestyle Analyzer"),
  id="nav",
  header=tags$head(
    tags$link(rel="stylesheet", href="styles.css"),
    tags$link(rel="stylesheet",
      href="https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&family=Playfair+Display:wght@600;700&display=swap")
  ),
  collapsible=TRUE,

  # ── OVERVIEW ────────────────────────────────────────────────
  tabPanel("Overview", div(class="tab-content",
    div(class="hero-band",
      h2("Student Lifestyle & GPA"),
      p("Explore how daily habits relate to academic performance. Build models, compare them, and predict your own GPA."),
      div(lapply(c("OLS","OLS + Regularisation","Decision Tree","Random Forest","KNN","GPA Prediction"),
                 function(x) span(class="stat-pill",x)))
    ),
    fluidRow(
      column(5, div(class="gpa-card", tags$h4("Dataset Summary"), uiOutput("overview_stats"))),
      column(7, div(class="gpa-card", tags$h4("GPA Distribution"),
                    div(class="plot-wrap", plotOutput("overview_hist",height="190px"))))
    ),
    div(class="gpa-card",
      tags$h4("GPA vs Each Numerical Lifestyle Variable"),
      div(class="section-sub",style="margin-top:-.4rem;margin-bottom:.8rem;",
          "Scatter with LOESS (red) and linear (dashed) smooths per variable"),
      div(class="plot-wrap", plotOutput("overview_scatter",height="320px"))
    ),
    fluidRow(
      column(6, div(class="gpa-card", tags$h4("Predictor Correlation Heatmap"),
                    div(class="plot-wrap",plotOutput("overview_heatmap",height="290px")))),
      column(6, div(class="gpa-card", tags$h4("GPA by Stress Level"),
                    plotOutput("overview_stress")))
    )
  )),

  # ── MODELS ─────────────────────────────────────────────────
  tabPanel("Models", div(class="tab-content",
    tabsetPanel(id="model_tabs", type="tabs",

      # OLS
      tabPanel("OLS", br(),
        sidebarLayout(
          sidebarPanel(width=4,
            tags$h3("Linear Regression (OLS)"),
            tags$p(class="section-sub","Least squares with optional polynomial and interaction terms."),
            div(style="margin-bottom:.6rem",
                term_badge("Linear"), term_badge("Quadratic"), term_badge("Interactions")),
            hr(),
            tags$h4("Variables"),
            var_grid_ui("ols", show_quadratic=TRUE, default_checked="study"),
            hr(),
            tags$h4("Interaction Terms"),
            tags$small(class="control-hint","Numeric predictors only — stress is not eligible for interactions."),
            br(),
            interaction_grid_ui("ols"),
            hr(),
            actionButton("run_ols","Fit OLS Model",class="btn btn-primary",style="width:100%")
          ),
          mainPanel(width=8,
            # uiOutput("ols_metrics_ui"),
            fluidRow(
              column(6,div(class="plot-wrap",
                div(class="plot-label","Coefficient Estimates"),
                div(class="control-hint",
                  " * Point estimate ± 95% CI. Red bars cross zero (not significant) ",
               ),
                plotOutput("ols_coef_plot",height="250px")
              )),
              column(6,div(class="plot-wrap",
                div(class="plot-label","Residuals vs Fitted"),
                div(class="control-hint",
                  " * A curved or funnel shape signals non-linearity or heteroscedasticity."
                ),
                plotOutput("ols_resid_plot",height="250px")
              ))
            ),
            fluidRow(
              column(6,div(class="plot-wrap",
                div(class="plot-label","Normal Q-Q"),
                div(class="control-hint",
                  " * A straight 45-degree diagnal line indicates normal residuals."
                ),
                plotOutput("ols_qq_plot",height="210px")
              )),
              column(6,div(class="plot-wrap",
                div(class="plot-label","Scale-Location"),
                div(class="control-hint",
                  " * A flat red line indicates homoscedasticity."
                ),
                plotOutput("ols_scale_plot",height="210px")
              ))
            ),
            div(class="gpa-card",tags$h4("Full Summary"),verbatimTextOutput("ols_summary"))
          )
        )
      ),

      # Decision Tree
      tabPanel("Decision Tree", br(),
        sidebarLayout(
          sidebarPanel(width=4,
            tags$h3("Decision Tree"),
            tags$p(class="section-sub","CART regression tree — interpretable splits on selected variables."),
            div(style="margin-bottom:.6rem",
                term_badge("Linear terms only","#7a7d8e"),
                term_badge("No quadratic / interactions","#7a7d8e")),
            hr(),
            tags$h4("Variables"),
            var_grid_ui("tree", show_quadratic=FALSE, default_checked="study"),
            hr(),
            div(class="control-row",
                tags$label(class="control-label","Max Depth"),
                sliderInput("tree_depth",NULL,min=1,max=10,value=3,step=1)),
            hr(),
            actionButton("run_tree","Fit Decision Tree",class="btn btn-primary",style="width:100%")
          ),
          mainPanel(width=8,
            uiOutput("tree_metrics_ui"),
            fluidRow(
              column(7,div(class="plot-wrap",div(class="plot-label","Tree Diagram"),
                           plotOutput("tree_plot",height="320px"))),
              column(5,div(class="plot-wrap",div(class="plot-label","Variable Importance"),
                           plotOutput("tree_importance",height="320px")))
            )
          )
        )
      ),

      # Random Forest
      tabPanel("Random Forest", br(),
        sidebarLayout(
          sidebarPanel(width=4,
            tags$h3("Random Forest"),
            tags$p(class="section-sub","Ensemble of trees — lower variance, built-in OOB error estimate."),
            div(style="margin-bottom:.6rem",
                term_badge("Linear terms only","#7a7d8e"),
                term_badge("No quadratic / interactions","#7a7d8e")),
            hr(),
            tags$h4("Variables"),
            var_grid_ui("rf", show_quadratic=FALSE, default_checked="study"),
            hr(),
            div(class="control-row",
                tags$label(class="control-label","Number of Trees"),
                sliderInput("rf_ntree",NULL,min=50,max=500,value=200,step=50)),
            hr(),
            actionButton("run_rf","Fit Random Forest",class="btn btn-primary",style="width:100%")
          ),
          mainPanel(width=8,
            uiOutput("rf_metrics_ui"),
            fluidRow(
              column(6,div(class="plot-wrap",div(class="plot-label","Variable Importance (% Inc. MSE)"),
                           plotOutput("rf_importance",height="280px"))),
              column(6,div(class="plot-wrap",div(class="plot-label","OOB Error vs Number of Trees"),
                           plotOutput("rf_error_plot",height="280px")))
            )
          )
        )
      ),

      # Ridge / Lasso
      tabPanel("Ridge / Lasso", br(),
        sidebarLayout(
          sidebarPanel(width=4,
            tags$h3("Regularised Regression"),
            tags$p(class="section-sub","Ridge (α=0), Lasso (α=1), or Elastic Net."),
            div(style="margin-bottom:.6rem",
                term_badge("Linear terms only","#7a7d8e"),
                term_badge("No quadratic / interactions","#7a7d8e")),
            hr(),
            tags$h4("Variables"),
            tags$small(class="control-hint","Needs ≥ 2 variables. Stress Level expands to 2 numeric dummies."),
            br(),
            var_grid_ui("glmnet", show_quadratic=FALSE, default_checked=c("study","sleep")),
            hr(),
            div(class="control-row",
                tags$label(class="control-label","Alpha  (0 = Ridge · 1 = Lasso)"),
                sliderInput("glmnet_alpha",NULL,min=0,max=1,value=0,step=0.1)),
            hr(),
            actionButton("run_glmnet","Fit Regularised Model",class="btn btn-primary",style="width:100%")
          ),
          mainPanel(width=8,
            uiOutput("glmnet_metrics_ui"),
            fluidRow(
              column(6,div(class="plot-wrap",div(class="plot-label","Coefficient Path (log lambda)"),
                           plotOutput("glmnet_path",height="280px"))),
              column(6,div(class="plot-wrap",div(class="plot-label","CV Error vs log lambda"),
                           plotOutput("glmnet_cv_plot",height="280px")))
            )
          )
        )
      ),

      # KNN
      tabPanel("KNN", br(),
        sidebarLayout(
          sidebarPanel(width=4,
            tags$h3("K-Nearest Neighbours"),
            tags$p(class="section-sub","Non-parametric — average of k closest training points."),
            div(style="margin-bottom:.6rem",
                term_badge("Linear terms only","#7a7d8e"),
                term_badge("No quadratic / interactions","#7a7d8e")),
            hr(),
            tags$h4("Variables"),
            tags$small(class="control-hint","Stress Level expands to 2 numeric dummies for distance calculation."),
            br(),
            var_grid_ui("knn", show_quadratic=FALSE, default_checked="study"),
            hr(),
            div(class="control-row",
                tags$label(class="control-label","Max k to search"),
                sliderInput("knn_k_max",NULL,min=5,max=40,value=25,step=1)),
            hr(),
            actionButton("run_knn","Fit KNN Model",class="btn btn-primary",style="width:100%")
          ),
          mainPanel(width=8,
            uiOutput("knn_metrics_ui"),
            div(class="plot-wrap",
                div(class="plot-label","CV RMSE vs k (best k in red)"),
                plotOutput("knn_k_plot",height="280px"))
          )
        )
      ),

      # Compare
      tabPanel("Compare All", br(),
        div(class="model-section",
          div(class="model-section-header",
            div(class="model-dot",style="background:#e84855"),
            div(div(class="section-title","Model Comparison"),
                div(class="section-sub","5-fold CV across all models using each model's current variable selection."))
          ),
          div(style="margin-bottom:1.2rem",
              actionButton("compare_models","Run Comparison",
                           class="btn btn-primary",style="min-width:200px")),
          fluidRow(
            column(4,div(class="plot-wrap",div(class="plot-label","CV RMSE (lower = better)"),plotOutput("cmp_rmse",height="240px"))),
            column(4,div(class="plot-wrap",div(class="plot-label","CV MAE (lower = better)"), plotOutput("cmp_mae", height="240px"))),
            column(4,div(class="plot-wrap",div(class="plot-label","CV R² (higher = better)"),plotOutput("cmp_r2",  height="240px")))
          ),
          div(class="plot-wrap",
              div(class="plot-label","Predicted vs Actual GPA — per model"),
              plotOutput("cmp_pred_actual",height="300px")),
          div(class="gpa-card",tags$h4("Summary Table"),tableOutput("cmp_table"))
        )
      )
    )
  )),

  # ── PREDICT ────────────────────────────────────────────────
  tabPanel("Predict", div(class="tab-content",
    div(class="section-title","Predict Your GPA"),
    div(class="section-sub","Enter your typical daily hours. OLS model gives a 95% prediction interval."),
    fluidRow(
      column(8, div(class="gpa-card", tags$h4("Your Daily Hours"),
        fluidRow(
          column(4,
            numericInput("p_study","Study (hrs)",   value=6,min=0,max=24,step=.5),
            numericInput("p_sleep","Sleep (hrs)",   value=7,min=0,max=24,step=.5)),
          column(4,
            numericInput("p_social",  "Social (hrs)",         value=2,min=0,max=24,step=.5),
            numericInput("p_exercise","Exercise (hrs)",       value=1,min=0,max=24,step=.5)),
          column(4,
            numericInput("p_extra","Extracurricular (hrs)",   value=1,min=0,max=24,step=.5),
            br(),
            actionButton("run_predict","Predict GPA",
                         class="btn btn-primary",style="width:100%;margin-top:.3rem"))
        )
      )),
      column(4, uiOutput("pred_result_ui"))
    ),
    uiOutput("pred_interp_ui")
  ))
)

# ═══════════════════════════════════════════════════════════════
#  SERVER
# ═══════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Overview ────────────────────────────────────────────────
  output$overview_stats <- renderUI({
    div(class="metric-grid",
      metric_box("Observations", nrow(data)),
      metric_box("Predictors",   length(NUMERIC_VARS)+1L),
      metric_box("Avg GPA",      round(mean(data$GPA,na.rm=TRUE),2)),
      metric_box("GPA Range",    paste0(round(min(data$GPA),2)," – ",round(max(data$GPA),2)))
    )
  })

  output$overview_hist <- renderPlot({
    ggplot(data,aes(x=GPA))+
      geom_histogram(bins=28,fill="#2ec4b6",colour="#1a8c84",alpha=.85)+
      geom_vline(xintercept=mean(data$GPA),colour="#0f1b2d",linetype="dashed",linewidth=.7)+
      annotate("text",x=mean(data$GPA)+.04,y=Inf,vjust=1.7,
               label=paste0("mean = ",round(mean(data$GPA),2)),
               size=3,colour="#0f1b2d",fontface="bold",hjust=0)+
      labs(x="GPA",y="Count")+theme_gpa()
  },bg="transparent")

  output$overview_scatter <- renderPlot({
    nv <- unname(NUMERIC_VARS); nl <- names(NUMERIC_VARS)
    long <- data %>%
      pivot_longer(all_of(nv),names_to="variable",values_to="hours") %>%
      mutate(variable=recode(variable,!!!setNames(nl,nv)))
    ggplot(long,aes(x=hours,y=GPA))+
      geom_point(alpha=.18,size=.85,colour="#2ec4b6")+
      geom_smooth(method="loess",formula=y~x,se=TRUE,
                  colour="#e84855",fill="#e84855",alpha=.12,linewidth=.9)+
      geom_smooth(method="lm",formula=y~x,se=FALSE,
                  colour="#0f1b2d",linetype="dashed",linewidth=.65)+
      facet_wrap(~variable,nrow=1,scales="free_x")+
      labs(x="Daily Hours",y="GPA")+theme_gpa()+
      theme(strip.text=element_text(face="bold",size=8.5),
            strip.background=element_rect(fill="#f0ede6",colour=NA))
  },bg="transparent")

  output$overview_heatmap <- renderPlot({
    plot_corr_heatmap(data,unname(NUMERIC_VARS),names(NUMERIC_VARS))
  },bg="transparent")

  output$overview_stress <- renderPlot({
    df <- data %>% mutate(Stress_Level=factor(Stress_Level,levels=c("Low","Moderate","High")))
    ggplot(df,aes(x=Stress_Level,y=GPA,fill=Stress_Level))+
      geom_boxplot(alpha=.75,width=.6,outlier.shape=NA)+
      geom_jitter(width=.15,alpha=.2,size=.8,colour="#2ec4b6")+
      scale_fill_manual(values=c("#2ec4b6","#f4a261","#e84855"))+
      labs(x="Stress Level",y="GPA")+theme_gpa()+theme(legend.position="none")
  },bg="transparent")

  # ── OLS ─────────────────────────────────────────────────────
  ols_model <- eventReactive(input$run_ols, {
    main  <- isolate(get_inp(input,"ols_main_effects"))
    quad  <- isolate(get_inp(input,"ols_quadratic_terms"))
    inter <- isolate(get_inp(input,"ols_interaction_terms"))
    f <- build_formula(main,quad,inter); req(f)
    # Fit with the formula evaluated in global env so summary() prints it clearly
    m <- lm(f, data=data)
    m$call$formula <- f   # replace `f` token with actual formula in call
    m
  },ignoreInit=TRUE)

  ols_cv <- eventReactive(input$run_ols, {
    main  <- isolate(get_inp(input,"ols_main_effects"))
    quad  <- isolate(get_inp(input,"ols_quadratic_terms"))
    inter <- isolate(get_inp(input,"ols_interaction_terms"))
    f <- build_formula(main,quad,inter); req(f)
    train(f,data=data,method="lm",trControl=cv5())
  },ignoreInit=TRUE)

  output$ols_metrics_ui <- renderUI({
    fitted <- !isTruthy(ols_model()) && !isTruthy(ols_cv())
    if (fitted) {
      s <- summary(ols_model())
      r2   <- round(s$r.squared, 3)
      ar2  <- round(s$adj.r.squared, 3)
      rmse <- round(ols_cv()$results$RMSE, 3)
      aic  <- round(AIC(ols_model()), 1)
    } else {
      r2 <- ar2 <- rmse <- aic <- "—"
    }
    div(class="metric-grid",
      metric_box("R²",      r2),
      metric_box("Adj. R²", ar2),
      metric_box("CV RMSE", rmse),
      metric_box("AIC",     aic)
    )
  })

  output$ols_summary    <- renderPrint({ req(ols_model()); summary(ols_model()) })

  # Coef plot — clean up stress dummy labels for display
  output$ols_coef_plot  <- renderPlot({
    req(ols_model())
    m <- ols_model()
    ci <- confint(m)
    coefs <- coef(m)
    # Drop intercept
    idx <- names(coefs) != "(Intercept)"
    df  <- data.frame(
      term  = names(coefs)[idx],
      est   = coefs[idx],
      lo    = ci[idx, 1],
      hi    = ci[idx, 2]
    )
    # Humanise labels
    df$label <- df$term
    df$label <- gsub("stress",        "Stress", df$label)
    df$label <- gsub("I\\((.+)\\^2\\)", "\\1²",             df$label)
    df$label <- gsub(":",              " × ",                df$label)
    df <- df[order(abs(df$est)), ]
    df$label <- factor(df$label, levels = df$label)
    sig <- ifelse(df$lo > 0 | df$hi < 0, "#2ec4b6", "#e84855")
    ggplot(df, aes(x = est, y = label)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "#bbb", linewidth = .6) +
      geom_errorbarh(aes(xmin = lo, xmax = hi), height = .25,
                     colour = sig, linewidth = .75) +
      geom_point(size = 2.8, colour = sig) +
      labs(x = "Coefficient Estimate (95% CI)", y = NULL) +
      theme_gpa() +
      theme(axis.text.y = element_text(size = 8))
  }, bg = "transparent")
  output$ols_resid_plot <- renderPlot({
    req(ols_model())
    df <- data.frame(fitted=fitted(ols_model()),resid=resid(ols_model()))
    ggplot(df,aes(fitted,resid))+geom_point(alpha=.4,colour="#2ec4b6",size=1.3)+
      geom_smooth(se=FALSE,colour="#e84855",linewidth=.8,method="loess",formula=y~x)+
      geom_hline(yintercept=0,linetype="dashed",colour="#999")+
      labs(x="Fitted",y="Residuals")+theme_gpa()
  },bg="transparent")
  output$ols_qq_plot <- renderPlot({
    req(ols_model())
    ggplot(data.frame(resid=resid(ols_model())),aes(sample=resid))+
      stat_qq(colour="#2ec4b6",alpha=.5,size=1.2)+
      stat_qq_line(colour="#e84855",linewidth=.8)+
      labs(x="Theoretical",y="Sample")+theme_gpa()
  },bg="transparent")
  output$ols_scale_plot <- renderPlot({
    req(ols_model())
    df <- data.frame(fitted=fitted(ols_model()),sqrt_std=sqrt(abs(rstandard(ols_model()))))
    ggplot(df,aes(fitted,sqrt_std))+geom_point(alpha=.4,colour="#2ec4b6",size=1.3)+
      geom_smooth(se=FALSE,colour="#e84855",linewidth=.8,method="loess",formula=y~x)+
      labs(x="Fitted",y="√|Std. Residuals|")+theme_gpa()
  },bg="transparent")

  # ── Decision Tree ────────────────────────────────────────────
  tree_model <- eventReactive(input$run_tree, {
    vars <- isolate(get_inp(input,"tree_main_effects")); req(vars)
    rpart(build_formula(vars),data=data,
          control=rpart.control(maxdepth=isolate(input$tree_depth)))
  },ignoreInit=TRUE)

  tree_cv <- eventReactive(input$run_tree, {
    vars <- isolate(get_inp(input,"tree_main_effects")); req(vars)
    train(build_formula(vars),data=data,method="rpart",trControl=cv5())
  },ignoreInit=TRUE)

  output$tree_metrics_ui <- renderUI({
    req(tree_cv(),tree_model())
    div(class="metric-grid",
      metric_box("CV RMSE",round(tree_cv()$results$RMSE[1],3)),
      metric_box("Depth",  input$tree_depth),
      metric_box("Leaves", sum(tree_model()$frame$var=="<leaf>"))
    )
  })
  output$tree_plot <- renderPlot({
    req(tree_model())
    rpart.plot(tree_model(),type=4,extra=101,
               box.palette=list("#0f1b2d","#2ec4b6"),
               branch.lty=3,shadow.col="grey70",main="")
  },bg="transparent")
  output$tree_importance <- renderPlot({
    req(tree_model())
    imp <- tree_model()$variable.importance
    if(is.null(imp)) return(ggplot()+annotate("text",x=.5,y=.5,
      label="No importance\n(tree is a stump)",colour="#7a7d8e",size=4)+theme_void())
    df <- data.frame(variable=names(imp),importance=imp) %>%
      arrange(importance) %>% mutate(variable=factor(variable,levels=variable))
    ggplot(df,aes(importance,variable))+
      geom_col(fill="#0f1b2d",alpha=.85,width=.6)+
      geom_text(aes(label=round(importance,1)),hjust=-.15,size=3,colour="#2d3142")+
      scale_x_continuous(expand=expansion(mult=c(0,.2)))+
      labs(x="Importance",y=NULL)+theme_gpa()
  },bg="transparent")

  # ── Random Forest ────────────────────────────────────────────
  rf_model <- eventReactive(input$run_rf, {
    vars <- isolate(get_inp(input,"rf_main_effects")); req(vars)
    set.seed(42)
    randomForest(build_formula(vars),data=data,
                 ntree=isolate(input$rf_ntree),importance=TRUE)
  },ignoreInit=TRUE)

  output$rf_metrics_ui <- renderUI({
    req(rf_model()); m <- rf_model()
    div(class="metric-grid",
      metric_box("OOB RMSE",round(sqrt(m$mse[length(m$mse)]),3)),
      metric_box("OOB R²",  round(m$rsq[length(m$rsq)],3)),
      metric_box("Trees",   input$rf_ntree),
      metric_box("mtry",    m$mtry)
    )
  })
  output$rf_importance <- renderPlot({
    req(rf_model())
    imp <- importance(rf_model(),type=1)
    df  <- data.frame(variable=rownames(imp),inc_mse=imp[,1]) %>%
      arrange(inc_mse) %>% mutate(variable=factor(variable,levels=variable))
    ggplot(df,aes(inc_mse,variable))+
      geom_col(fill="#f4a261",alpha=.9,width=.6)+
      geom_text(aes(label=round(inc_mse,2)),hjust=-.15,size=3,colour="#2d3142")+
      scale_x_continuous(expand=expansion(mult=c(0,.2)))+
      labs(x="% Increase in MSE",y=NULL)+theme_gpa()
  },bg="transparent")
  output$rf_error_plot <- renderPlot({
    req(rf_model())
    df <- data.frame(trees=seq_along(rf_model()$mse),mse=rf_model()$mse)
    ggplot(df,aes(trees,sqrt(mse)))+geom_line(colour="#f4a261",linewidth=.9)+
      labs(x="Number of Trees",y="OOB RMSE")+theme_gpa()
  },bg="transparent")

  # ── Ridge / Lasso ────────────────────────────────────────────
  glmnet_inputs <- eventReactive(input$run_glmnet, {
    vars  <- isolate(get_inp(input,"glmnet_main_effects"))
    alpha <- isolate(input$glmnet_alpha)
    if(is.null(vars) || length(vars)<2) vars <- c("study","sleep")
    list(X=as.matrix(data[,vars,drop=FALSE]),y=data$GPA,alpha=alpha,vars=vars)
  },ignoreInit=TRUE)

  glmnet_cv_fit   <- eventReactive(input$run_glmnet, {
    d <- glmnet_inputs(); set.seed(42)
    cv.glmnet(d$X,d$y,alpha=d$alpha,nfolds=5)
  },ignoreInit=TRUE)
  glmnet_path_fit <- eventReactive(input$run_glmnet, {
    d <- glmnet_inputs(); glmnet(d$X,d$y,alpha=d$alpha)
  },ignoreInit=TRUE)

  output$glmnet_metrics_ui <- renderUI({
    req(glmnet_cv_fit())
    cv  <- glmnet_cv_fit(); lam <- cv$lambda.min
    idx <- which(cv$lambda==lam); rmse <- sqrt(cv$cvm[idx])
    av  <- glmnet_inputs()$alpha
    method <- if(av==0)"Ridge" else if(av==1)"Lasso" else paste0("Elastic Net (α=",av,")")
    div(class="metric-grid",
      metric_box("Method",    method),
      metric_box("Optimal λ", round(lam,4)),
      metric_box("CV RMSE",   round(rmse,3)),
      metric_box("Non-zero coefs", sum(coef(cv,s="lambda.min")[-1]!=0))
    )
  })
  output$glmnet_path <- renderPlot({
    req(glmnet_path_fit(),glmnet_cv_fit())
    m <- glmnet_path_fit(); lam <- glmnet_cv_fit()$lambda.min
    cm <- as.matrix(t(coef(m)[-1,,drop=FALSE]))
    df <- as.data.frame(cm) %>%
      mutate(log_lambda=log(m$lambda)) %>%
      pivot_longer(-log_lambda,names_to="variable",values_to="coef")
    nv  <- length(unique(df$variable))
    pal <- setNames(colorRampPalette(c("#2ec4b6","#0f1b2d","#f4a261","#6a4c93","#e84855","#2d6a4f","#f4c261"))(nv),
                    unique(df$variable))
    ggplot(df,aes(log_lambda,coef,colour=variable))+geom_line(linewidth=.8)+
      geom_vline(xintercept=log(lam),linetype="dashed",colour="#aaa",linewidth=.6)+
      scale_colour_manual(values=pal)+
      labs(x="log(lambda)",y="Coefficient",colour=NULL)+theme_gpa()+
      theme(legend.position="bottom",legend.text=element_text(size=7.5))
  },bg="transparent")
  output$glmnet_cv_plot <- renderPlot({
    req(glmnet_cv_fit()); cv <- glmnet_cv_fit()
    df <- data.frame(log_lam=log(cv$lambda),cvm=cv$cvm,cvup=cv$cvup,cvlo=cv$cvlo)
    ggplot(df,aes(log_lam,cvm))+
      geom_ribbon(aes(ymin=cvlo,ymax=cvup),fill="#6a4c93",alpha=.15)+
      geom_line(colour="#6a4c93",linewidth=.9)+
      geom_vline(xintercept=log(cv$lambda.min),linetype="dashed",colour="#e84855",linewidth=.7)+
      geom_vline(xintercept=log(cv$lambda.1se),linetype="dashed",colour="#999",linewidth=.7)+
      annotate("text",x=log(cv$lambda.min),y=max(df$cvup),label="lam.min",hjust=-.1,size=3,colour="#e84855")+
      annotate("text",x=log(cv$lambda.1se),y=max(df$cvup),label="lam.1se",hjust=-.1,size=3,colour="#666")+
      labs(x="log(lambda)",y="CV MSE")+theme_gpa()
  },bg="transparent")

  # ── KNN ─────────────────────────────────────────────────────
  knn_cv <- eventReactive(input$run_knn, {
    vars <- isolate(get_inp(input,"knn_main_effects")); req(vars)
    f <- build_formula(vars)
    train(f,data=data,method="knn",preProcess=c("center","scale"),
          tuneGrid=data.frame(k=1:isolate(input$knn_k_max)),trControl=cv5())
  },ignoreInit=TRUE)

  output$knn_metrics_ui <- renderUI({
    req(knn_cv()); best <- knn_cv()$bestTune$k
    rmse <- min(knn_cv()$results$RMSE)
    mae  <- knn_cv()$results$MAE[knn_cv()$results$k==best]
    div(class="metric-grid",
      metric_box("Best k",  best),
      metric_box("CV RMSE", round(rmse,3)),
      metric_box("CV MAE",  round(mae,3))
    )
  })
  output$knn_k_plot <- renderPlot({
    req(knn_cv()); df <- knn_cv()$results; best <- knn_cv()$bestTune$k
    ggplot(df,aes(k,RMSE))+geom_line(colour="#2d6a4f",linewidth=.9)+
      geom_point(size=2.2,colour="#2d6a4f")+
      geom_point(data=df[df$k==best,],aes(k,RMSE),size=4.5,colour="#e84855",shape=18)+
      annotate("text",x=best,y=df$RMSE[df$k==best],
               label=paste0("best k=",best),vjust=-1.3,size=3.2,colour="#e84855",fontface="bold")+
      labs(x="k (neighbours)",y="CV RMSE")+theme_gpa()
  },bg="transparent")

  # ── Model Comparison ─────────────────────────────────────────
  compare_results <- eventReactive(input$compare_models, {
    results <- list()

    run_c <- function(nm, vars, method, extra=list()) {
      tryCatch({
        req(vars)
        f <- build_formula(vars)
        set.seed(42)
        ctrl <- trainControl(method="cv",number=5,savePredictions="final")
        r  <- do.call(train, c(list(form=f,data=data,method=method,trControl=ctrl),extra))
        br <- r$results[which.min(r$results$RMSE),]
        ps <- r$pred[order(r$pred$rowIndex),]
        results[[nm]] <<- list(RMSE=br$RMSE,MAE=br$MAE,
          Rsq=if("Rsquared"%in%names(br)) br$Rsquared else NA,
          pred=ps$pred,obs=ps$obs)
      },error=function(e) NULL)
    }

    # OLS
    tryCatch({
      f <- build_formula(isolate(get_inp(input,"ols_main_effects")),
                         isolate(get_inp(input,"ols_quadratic_terms")),
                         isolate(get_inp(input,"ols_interaction_terms")))
      if(!is.null(f)){
        set.seed(42)
        ctrl <- trainControl(method="cv",number=5,savePredictions="final")
        r  <- train(f,data=data,method="lm",trControl=ctrl)
        br <- r$results; ps <- r$pred[order(r$pred$rowIndex),]
        results[["Linear Regression"]] <- list(RMSE=br$RMSE,MAE=br$MAE,
          Rsq=if("Rsquared"%in%names(br)) br$Rsquared else NA,
          pred=ps$pred,obs=ps$obs)
      }
    },error=function(e) NULL)

    run_c("Decision Tree", isolate(get_inp(input,"tree_main_effects")),
          "rpart", list(tuneGrid=data.frame(cp=0.01)))
    run_c("KNN", isolate(get_inp(input,"knn_main_effects")),
          "knn", list(preProcess=c("center","scale"),tuneGrid=data.frame(k=seq(3,15,2))))

    tryCatch({
      gvars <- isolate(get_inp(input,"glmnet_main_effects"))
      if(is.null(gvars) || length(gvars)<2) gvars <- c("study","sleep")
      X <- as.matrix(data[,gvars,drop=FALSE]); y <- data$GPA
      for(nm_g in c("Ridge","Lasso")){
        al <- if(nm_g=="Ridge") 0 else 1
        set.seed(42); cv_g <- cv.glmnet(X,y,alpha=al,nfolds=5)
        lam <- cv_g$lambda.min; idx <- which(cv_g$lambda==lam)
        fit_g <- glmnet(X,y,alpha=al,lambda=lam); phat <- as.vector(predict(fit_g,newx=X))
        results[[nm_g]] <- list(RMSE=sqrt(cv_g$cvm[idx]),MAE=mean(abs(y-phat)),
                                Rsq=cor(y,phat)^2,pred=phat,obs=y)
      }
    },error=function(e) NULL)

    tryCatch({
      vars <- isolate(get_inp(input,"rf_main_effects")); req(vars)
      set.seed(42)
      m <- randomForest(build_formula(vars),data=data,
                        ntree=isolate(input$rf_ntree),importance=FALSE)
      phat <- predict(m,data); y <- data$GPA
      results[["Random Forest"]] <- list(RMSE=sqrt(m$mse[length(m$mse)]),
        MAE=mean(abs(y-phat)),Rsq=cor(y,phat)^2,pred=phat,obs=y)
    },error=function(e) NULL)

    results
  },ignoreInit=TRUE)

  cmp_plot <- function(metric, ylab, hi=FALSE) {
    req(compare_results()); res <- compare_results()
    if(length(res)==0) return(NULL)
    df <- data.frame(Model=names(res),
      Value=sapply(res,function(r){v<-r[[metric]];if(is.null(v)||length(v)==0)NA else v[1]})) %>%
      filter(!is.na(Value)) %>%
      arrange(if(hi) desc(Value) else Value) %>%
      mutate(Model=factor(Model,levels=Model))
    bi <- if(hi) which.max(df$Value) else which.min(df$Value)
    ggplot(df,aes(Model,Value,fill=Model))+
      geom_col(width=.55,show.legend=FALSE,alpha=ifelse(seq_len(nrow(df))==bi,1,.65))+
      geom_text(aes(label=round(Value,3)),vjust=-.4,size=3.1,fontface="bold")+
      geom_point(data=df[bi,],aes(Model,Value),shape=9,size=3.5,colour="#e84855",
                 position=position_nudge(y=max(df$Value)*.06))+
      scale_fill_manual(values=MODEL_COLORS)+
      scale_y_continuous(expand=expansion(mult=c(0,.22)))+
      labs(x=NULL,y=ylab)+theme_gpa()+
      theme(axis.text.x=element_text(angle=30,hjust=1,size=7.5))
  }

  output$cmp_rmse <- renderPlot({cmp_plot("RMSE","CV RMSE")},bg="transparent")
  output$cmp_mae  <- renderPlot({cmp_plot("MAE","CV MAE")},bg="transparent")
  output$cmp_r2   <- renderPlot({cmp_plot("Rsq","CV R²",hi=TRUE)},bg="transparent")

  output$cmp_pred_actual <- renderPlot({
    req(compare_results()); res <- compare_results()
    long <- do.call(rbind,lapply(names(res),function(nm){
      r<-res[[nm]]; if(is.null(r$pred)) return(NULL)
      n<-min(length(r$pred),length(r$obs))
      data.frame(Model=nm,pred=r$pred[1:n],obs=r$obs[1:n])
    }))
    if(is.null(long)||nrow(long)==0) return(NULL)
    lims <- range(c(long$pred,long$obs),na.rm=TRUE)
    ggplot(long,aes(obs,pred,colour=Model))+
      geom_abline(slope=1,intercept=0,colour="#ccc",linewidth=.7,linetype="dashed")+
      geom_point(alpha=.25,size=.9,show.legend=FALSE)+
      geom_smooth(method="lm",formula=y~x,se=FALSE,linewidth=.8,show.legend=FALSE)+
      facet_wrap(~Model,nrow=1)+scale_colour_manual(values=MODEL_COLORS)+
      coord_fixed(xlim=lims,ylim=lims)+
      labs(x="Actual GPA",y="Predicted GPA")+theme_gpa()+
      theme(strip.text=element_text(face="bold",size=8),
            strip.background=element_rect(fill="#f0ede6",colour=NA))
  },bg="transparent")

  output$cmp_table <- renderTable({
    req(compare_results()); res <- compare_results()
    df <- do.call(rbind,lapply(names(res),function(nm){
      r<-res[[nm]]
      data.frame(Model=nm,RMSE=round(r$RMSE[1],4),MAE=round(r$MAE[1],4),
                 R2=round(ifelse(is.na(r$Rsq[1]),NA,r$Rsq[1]),4),
                 stringsAsFactors=FALSE)
    })) %>% arrange(RMSE)
    names(df)[4] <- "R²"; df
  },striped=TRUE,hover=TRUE,bordered=FALSE,spacing="s",width="100%")

  # ── Predict ─────────────────────────────────────────────────
  pred_result <- eventReactive(input$run_predict, {
    m  <- lm(GPA ~ study+sleep+social+exercise+extra,data=data)
    nd <- data.frame(study=input$p_study,sleep=input$p_sleep,
                     social=input$p_social,exercise=input$p_exercise,extra=input$p_extra)
    pred <- predict(m,newdata=nd,interval="prediction",level=.95)
    list(gpa=round(pred[1,"fit"],2),low=round(pred[1,"lwr"],2),
         high=round(pred[1,"upr"],2),model=m)
  })

  output$pred_result_ui <- renderUI({
    req(pred_result()); r <- pred_result(); gpa <- max(0,min(4,r$gpa))
    div(class="pred-result",
      div(class="pred-label","Predicted GPA"),
      div(class="pred-gpa",gpa),
      div(class="pred-ci",paste0("95% PI: [",max(0,r$low)," – ",min(4,r$high),"]"))
    )
  })

  output$pred_interp_ui <- renderUI({
    req(pred_result()); r <- pred_result(); s <- summary(r$model)
    gpa  <- max(0,min(4,r$gpa))
    tier <- if(gpa>=3.7)"excellent — top-tier academic performance"
            else if(gpa>=3.3)"strong — above the typical student"
            else if(gpa>=3.0)"solid — around average for college students"
            else if(gpa>=2.5)"moderate — some room for improvement"
            else "low — consider reviewing your study habits"
    div(class="interp-box",tags$b("Interpretation: "),
      sprintf("Based on your inputs, the model predicts a GPA of %.2f (%s). ",gpa,tier),
      sprintf("The model (R² = %.3f) explains %.1f%% of GPA variance. ",
              s$r.squared,s$r.squared*100),
      "The 95% prediction interval reflects plausible individual variation.")
  })
}

shinyApp(ui,server)