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
  "Linear Regression"    = "#2ec4b6",
  "Decision Tree"        = "#0f1b2d",
  "Random Forest"        = "#f4a261",
  "Ridge"                = "#6a4c93",
  "Lasso"                = "#6a4c93",
  "Elastic Net (α=0.5)"  = "#6a4c93",
  "KNN"                  = "#2d6a4f"
)

# Helper: get colour for a model name (handles dynamic Elastic Net labels)
model_color <- function(nm) {
  if (nm %in% names(MODEL_COLORS)) return(MODEL_COLORS[[nm]])
  if (grepl("Elastic Net|Ridge|Lasso", nm)) return("#6a4c93")
  "#7a7d8e"  # fallback
}

# ── UI helpers ───────────────────────────────────────────────────

term_badge <- function(txt, col = "#2ec4b6")
  tags$span(txt, style = paste0(
    "display:inline-block;font-size:.68rem;font-weight:700;",
    "text-transform:uppercase;letter-spacing:.6px;padding:.15rem .55rem;",
    "border-radius:20px;border:1px solid ", col, ";color:", col,
    ";margin-right:.3rem;margin-bottom:.3rem;"
  ))

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

# ── Helper: one colour-accented slider row ───────────────────────
make_pred_slider <- function(id, label, default_val, accent_color) {
  tagList(
    div(style = "margin-bottom:.9rem;",
      div(style = "display:flex; justify-content:space-between; align-items:baseline; margin-bottom:.1rem;",
        tags$span(style = "font-size:.84rem; font-weight:600; color:#2d3142;", label),
        uiOutput(paste0(id, "_val_display"), inline = TRUE)
      ),
      sliderInput(id, NULL,
        min = 0, max = 16, value = default_val, step = 0.5,
        width = "100%"
      )
    ),
    tags$style(HTML(paste0(
      "#", id, " .irs--shiny .irs-bar          { background:", accent_color, " !important; border-color:", accent_color, " !important; }",
      "#", id, " .irs--shiny .irs-handle        { background:", accent_color, " !important; border-color:", accent_color, " !important; }",
      "#", id, " .irs--shiny .irs-single        { background:", accent_color, " !important; }"
    )))
  )
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
            uiOutput("ols_metrics_ui"),
            fluidRow(
              column(6,div(class="plot-wrap",
                div(class="plot-label","Coefficient Estimates"),
                div(class="control-hint"," * Point estimate ± 95% CI. Red bars cross zero (not significant) "),
                plotOutput("ols_coef_plot",height="250px")
              )),
              column(6,div(class="plot-wrap",
                div(class="plot-label","Residuals vs Fitted"),
                div(class="control-hint"," * A curved or funnel shape signals non-linearity or heteroscedasticity."),
                plotOutput("ols_resid_plot",height="250px")
              ))
            ),
            fluidRow(
              column(6,div(class="plot-wrap",
                div(class="plot-label","Normal Q-Q"),
                div(class="control-hint"," * A straight 45-degree diagonal line indicates normal residuals."),
                plotOutput("ols_qq_plot",height="210px")
              )),
              column(6,div(class="plot-wrap",
                div(class="plot-label","Scale-Location"),
                div(class="control-hint"," * A flat red line indicates homoscedasticity."),
                plotOutput("ols_scale_plot",height="210px")
              ))
            ),
            div(class="gpa-card",tags$h4("Full Summary"),verbatimTextOutput("ols_summary"))
          )
        )
      ),

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

      tabPanel("Compare All", br(),
        div(class="model-section",
          div(class="model-section-header",
            div(class="model-dot",style="background:#e84855"),
            div(div(class="section-title","Model Comparison"),
                div(class="section-sub","5-fold CV reusing each model you've already fitted — same variables, same hyperparameters. Fit models first in their respective tabs."))
          ),
          uiOutput("cmp_status_ui"),
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


  # ── PREDICT ──────────────────────────────────────────────────
  tabPanel("Predict", div(class="tab-content",

    div(class="hero-band",
      h2("Predict Your GPA"),
      p("Choose a model you've already fitted in the Models tab, enter your daily hours, and get a personalised GPA estimate."),
      div(
        span(class="stat-pill", "Use Your Fitted Models"),
        span(class="stat-pill", "24-hr Budget Enforced"),
        span(class="stat-pill", "Per-model Insights")
      )
    ),

    fluidRow(

      # ── Left: inputs ──────────────────────────────────────
      column(7,

        # Model selector card
        div(class="gpa-card", style="margin-bottom:1rem;",
          tags$h4("Select Model"),
          tags$p(style="font-size:.8rem;color:#7a7d8e;margin-top:-.5rem;margin-bottom:1rem;",
            "Only models fitted in the Models tab are available. Head there first and click a Fit button."
          ),
          uiOutput("pred_model_selector")
        ),

        # Inputs card
        div(class="gpa-card",
          tags$h4("Daily Hour Allocation"),

          # Budget bar
          div(style="margin-bottom:1.5rem;",
            div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:.4rem;",
              tags$span(style="font-size:.72rem;font-weight:700;text-transform:uppercase;letter-spacing:.9px;color:#7a7d8e;",
                        "Hours used today"),
              uiOutput("budget_label")
            ),
            div(style="background:#e4e0d8;border-radius:99px;height:11px;overflow:hidden;",
              uiOutput("budget_bar")
            ),
            uiOutput("budget_warning")
          ),

          make_pred_slider("p_study",    "📚  Study",           6, "#2ec4b6"),
          make_pred_slider("p_sleep",    "😴  Sleep",           7, "#6a4c93"),
          make_pred_slider("p_social",   "💬  Social",          2, "#f4a261"),
          make_pred_slider("p_exercise", "🏃  Exercise",        1, "#2d6a4f"),
          make_pred_slider("p_extra",    "🎭  Extracurricular", 1, "#e84855"),

          # Stress radio (only relevant for OLS/Ridge/Lasso — shown always, used when applicable)
          div(style="margin-top:1.2rem;",
            tags$p(style="font-size:.72rem;font-weight:700;text-transform:uppercase;letter-spacing:.9px;color:#7a7d8e;margin-bottom:.55rem;",
                   "😰  Stress Level"),
            div(style="display:flex;gap:.7rem;",
              lapply(list(list("Low",0,"#2ec4b6"), list("Moderate",1,"#f4a261"), list("High",2,"#e84855")),
                function(x) {
                  input_attrs <- list(type="radio", name="p_stress_radio",
                                      value=x[[2]], style="display:none;")
                  if (x[[2]] == 1) input_attrs[["checked"]] <- NA
                  tags$label(
                    class = "stress-radio-label",
                    style = paste0(
                      "flex:1;text-align:center;cursor:pointer;padding:.6rem .4rem;",
                      "border-radius:9px;border:2px solid ", x[[3]], "30;",
                      "font-size:.83rem;font-weight:600;color:", x[[3]], ";",
                      "transition:background .15s,border-color .15s;"
                    ),
                    do.call(tags$input, input_attrs),
                    x[[1]]
                  )
                }
              )
            )
          ),

          div(style="margin-top:1.6rem;",
            actionButton("run_predict", "Predict My GPA  →",
                         class="btn btn-primary",
                         style="width:100%;font-size:.93rem;padding:.72rem;letter-spacing:.3px;")
          )
        )
      ),

      # ── Right: results ────────────────────────────────────
      column(5,
        uiOutput("pred_result_ui"),
        uiOutput("pred_interp_ui"),
        uiOutput("pred_breakdown_ui")
      )
    ),

    tags$script(HTML("
      function styleStress() {
        document.querySelectorAll('input[name=\"p_stress_radio\"]').forEach(function(r) {
          var lbl = r.closest('label');
          if (!lbl) return;
          if (r.checked) {
            var col = r.value == 0 ? '#2ec4b6' : r.value == 1 ? '#f4a261' : '#e84855';
            lbl.style.background  = col + '18';
            lbl.style.borderColor = col;
          } else {
            lbl.style.background  = 'transparent';
            lbl.style.borderColor = '';
          }
        });
        var checked = document.querySelector('input[name=\"p_stress_radio\"]:checked');
        if (checked) Shiny.setInputValue('p_stress', parseInt(checked.value), {priority:'event'});
      }
      $(document).ready(function() {
        setTimeout(function() {
          styleStress();
          document.querySelectorAll('input[name=\"p_stress_radio\"]').forEach(function(r) {
            r.addEventListener('change', styleStress);
          });
        }, 500);
      });
    "))
  ))
)  # end navbarPage


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
    m <- lm(f, data=data)
    m$call$formula <- f
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
    req(ols_model())
    m  <- ols_model()
    s  <- summary(m)
    r2   <- round(s$r.squared,     3)
    ar2  <- round(s$adj.r.squared, 3)
    # CV RMSE: use ols_cv if available, else show training RMSE with asterisk
    rmse_txt <- tryCatch({
      cv_res <- ols_cv()
      rmse_v <- cv_res$results$RMSE
      paste0(round(min(rmse_v, na.rm=TRUE), 3))
    }, error = function(e) {
      paste0(round(sqrt(mean(resid(m)^2)), 3), "*")
    })
    aic  <- round(AIC(m), 1)
    f    <- s$fstatistic
    pval <- if (!is.null(f)) pf(f[1], f[2], f[3], lower.tail=FALSE) else NA
    pval_txt <- if (!is.na(pval)) {
      if (pval < 0.001) "< 0.001" else as.character(round(pval, 3))
    } else "—"
    n_terms <- length(coef(m)) - 1L
    div(class="metric-grid", style="margin-bottom:1rem;",
      metric_box("R²",      r2),
      metric_box("Adj. R²", ar2),
      metric_box("CV RMSE", rmse_txt),
      metric_box("AIC",     aic),
      metric_box("F p-val", pval_txt),
      metric_box("Terms",   n_terms)
    )
  })

  output$ols_summary    <- renderPrint({ req(ols_model()); summary(ols_model()) })

  output$ols_coef_plot  <- renderPlot({
    req(ols_model())
    m <- ols_model()
    ci <- confint(m)
    coefs <- coef(m)
    idx <- names(coefs) != "(Intercept)"
    df  <- data.frame(term=names(coefs)[idx],est=coefs[idx],lo=ci[idx,1],hi=ci[idx,2])
    df$label <- df$term
    df$label <- gsub("stress","Stress",df$label)
    df$label <- gsub("I\\((.+)\\^2\\)","\\1²",df$label)
    df$label <- gsub(":"," × ",df$label)
    df <- df[order(abs(df$est)),]
    df$label <- factor(df$label, levels=df$label)
    sig <- ifelse(df$lo > 0 | df$hi < 0, "#2ec4b6", "#e84855")
    ggplot(df,aes(x=est,y=label))+
      geom_vline(xintercept=0,linetype="dashed",colour="#bbb",linewidth=.6)+
      geom_errorbarh(aes(xmin=lo,xmax=hi),height=.25,colour=sig,linewidth=.75)+
      geom_point(size=2.8,colour=sig)+
      labs(x="Coefficient Estimate (95% CI)",y=NULL)+theme_gpa()+
      theme(axis.text.y=element_text(size=8))
  },bg="transparent")
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
    # ── Reuse each already-fitted model reactive; run proper 5-fold CV for all ──
    results <- list()
    ctrl5 <- trainControl(method="cv", number=5, savePredictions="final")

    cv_metrics <- function(preds_df) {
      # preds_df has columns: pred, obs, Resample (from caret savePredictions)
      ps  <- preds_df[order(preds_df$rowIndex), ]
      rmse <- sqrt(mean((ps$obs - ps$pred)^2))
      mae  <- mean(abs(ps$obs - ps$pred))
      rsq  <- cor(ps$obs, ps$pred)^2
      list(RMSE=rmse, MAE=mae, Rsq=rsq, pred=ps$pred, obs=ps$obs)
    }

    # ── OLS: re-run CV with exact same formula as fitted model ──────────────
    tryCatch({
      m <- ols_model()                          # already fitted reactive
      f <- formula(m)
      set.seed(42)
      r <- train(f, data=data, method="lm", trControl=ctrl5)
      results[["Linear Regression"]] <- cv_metrics(r$pred)
    }, error=function(e) NULL)

    # ── Decision Tree: re-run CV with same vars + same max depth ────────────
    tryCatch({
      m    <- tree_model()
      vars <- isolate(get_inp(input, "tree_main_effects"))
      f    <- build_formula(vars)
      depth <- isolate(input$tree_depth) %||% 3
      set.seed(42)
      r <- train(f, data=data, method="rpart", trControl=ctrl5,
                 tuneGrid=data.frame(cp=m$control$cp %||% 0.01),
                 control=rpart.control(maxdepth=depth))
      results[["Decision Tree"]] <- cv_metrics(r$pred)
    }, error=function(e) NULL)

    # ── Random Forest: 5-fold CV using same formula + ntree ─────────────────
    tryCatch({
      m     <- rf_model()
      f     <- formula(m$terms)
      ntree <- isolate(input$rf_ntree) %||% 200
      set.seed(42)
      r <- train(f, data=data, method="rf", trControl=ctrl5,
                 ntree=ntree,
                 tuneGrid=data.frame(mtry=m$mtry))
      results[["Random Forest"]] <- cv_metrics(r$pred)
    }, error=function(e) NULL)

    # ── Elastic Net: re-run CV with same vars + same alpha + lambda.min ─────
    tryCatch({
      cv_fit <- glmnet_cv_fit()
      g_in   <- glmnet_inputs()
      X      <- g_in$X; y <- g_in$y
      alpha  <- g_in$alpha
      lam    <- cv_fit$lambda.min

      # 5-fold CV predictions using the fitted lambda
      set.seed(42)
      folds <- sample(rep(1:5, length.out=nrow(X)))
      preds <- numeric(nrow(X)); obs <- numeric(nrow(X))
      for (k in 1:5) {
        test_idx  <- folds == k
        train_idx <- !test_idx
        fit_k <- glmnet(X[train_idx,], y[train_idx], alpha=alpha, lambda=lam)
        preds[test_idx] <- as.numeric(predict(fit_k, newx=X[test_idx,], s=lam))
        obs[test_idx]   <- y[test_idx]
      }
      rmse <- sqrt(mean((obs-preds)^2)); mae <- mean(abs(obs-preds)); rsq <- cor(obs,preds)^2
      method_label <- if(alpha==0) "Ridge" else if(alpha==1) "Lasso" else
                        paste0("Elastic Net (α=",alpha,")")
      results[[method_label]] <- list(RMSE=rmse, MAE=mae, Rsq=rsq, pred=preds, obs=obs)
    }, error=function(e) NULL)

    # ── KNN: re-run CV with same vars + best k from fitted model ────────────
    tryCatch({
      m    <- knn_cv()
      vars <- isolate(get_inp(input, "knn_main_effects"))
      f    <- build_formula(vars)
      best_k <- m$bestTune$k
      set.seed(42)
      r <- train(f, data=data, method="knn", trControl=ctrl5,
                 preProcess=c("center","scale"),
                 tuneGrid=data.frame(k=best_k))
      results[["KNN"]] <- cv_metrics(r$pred)
    }, error=function(e) NULL)

    results
  }, ignoreInit=TRUE)

  # ── Status banner showing which models are fitted ─────────────────────
  output$cmp_status_ui <- renderUI({
    model_checks <- list(
      list("Linear Regression", !is.null(tryCatch(ols_model(),  error=function(e) NULL))),
      list("Decision Tree",     !is.null(tryCatch(tree_model(), error=function(e) NULL))),
      list("Random Forest",     !is.null(tryCatch(rf_model(),   error=function(e) NULL))),
      list("Regularised",       !is.null(tryCatch(glmnet_cv_fit(), error=function(e) NULL))),
      list("KNN",               !is.null(tryCatch(knn_cv(),     error=function(e) NULL)))
    )
    pills <- lapply(model_checks, function(x) {
      nm     <- x[[1]]; fitted <- x[[2]]
      col    <- if (fitted) "#2ec4b6" else "#e4e0d8"
      txtcol <- if (fitted) "#0f1b2d" else "#aaa"
      icon   <- if (fitted) "✓" else "○"
      tags$span(style=paste0(
        "display:inline-flex;align-items:center;gap:.3rem;",
        "background:", col, "22;border:1px solid ", col, ";",
        "border-radius:20px;padding:.2rem .65rem;font-size:.72rem;",
        "font-weight:700;color:", txtcol, ";margin-right:.4rem;margin-bottom:.4rem;"
      ), icon, " ", nm)
    })
    n_fitted <- sum(sapply(model_checks, `[[`, 2))
    div(style="margin-bottom:1rem;",
      div(style="font-size:.75rem;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#7a7d8e;margin-bottom:.5rem;",
          paste0("Models ready: ", n_fitted, " / ", length(model_checks))),
      div(pills),
      if (n_fitted == 0)
        div(style="margin-top:.6rem;font-size:.82rem;color:#e84855;",
            "No models fitted yet — go to the Models tab and fit at least one.")
      else if (n_fitted < length(model_checks))
        div(style="margin-top:.6rem;font-size:.82rem;color:#7a7d8e;",
            "Unfitted models will be skipped in the comparison.")
    )
  })

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
      scale_fill_manual(values=setNames(sapply(levels(df$Model), model_color), levels(df$Model)))+
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
      facet_wrap(~Model,nrow=1)+
      scale_colour_manual(values=setNames(sapply(unique(long$Model), model_color), unique(long$Model)))+
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


  # ══════════════════════════════════════════════════════════
  #  PREDICT — server
  # ══════════════════════════════════════════════════════════

  # ── Slider value displays ────────────────────────────────
  lapply(list(
    list("p_study",    "#2ec4b6"),
    list("p_sleep",    "#6a4c93"),
    list("p_social",   "#f4a261"),
    list("p_exercise", "#2d6a4f"),
    list("p_extra",    "#e84855")
  ), function(x) {
    local({
      id  <- x[[1]]
      col <- x[[2]]
      output[[paste0(id, "_val_display")]] <- renderUI({
        val <- input[[id]] %||% 0
        tags$span(style = paste0("font-size:.82rem;font-weight:700;color:", col, ";"),
                  paste0(val, " hrs"))
      })
    })
  })

  # ── 24-hr budget ─────────────────────────────────────────
  total_hours <- reactive({
    round((input$p_study %||% 0) + (input$p_sleep %||% 0) +
          (input$p_social %||% 0) + (input$p_exercise %||% 0) +
          (input$p_extra %||% 0), 1)
  })

  output$budget_bar <- renderUI({
    pct <- min(total_hours() / 24 * 100, 100)
    col <- if (pct <= 70) "#2ec4b6" else if (pct <= 90) "#f4a261" else "#e84855"
    div(style = paste0("width:", pct, "%; height:100%; border-radius:99px;",
                       "background:", col, "; transition:width .25s, background .25s;"))
  })

  output$budget_label <- renderUI({
    tot <- total_hours(); rem <- round(24 - tot, 1)
    col <- if (tot <= 17) "#2ec4b6" else if (tot <= 22) "#f4a261" else "#e84855"
    tags$span(style = paste0("font-size:.82rem;font-weight:700;color:", col, ";"),
              paste0(tot, " / 24 hrs  (", max(rem, 0), " free)"))
  })

  output$budget_warning <- renderUI({
    if (total_hours() > 24)
      div(style = "margin-top:.5rem;padding:.4rem .8rem;background:#fef2f3;border-left:3px solid #e84855;border-radius:0 6px 6px 0;font-size:.78rem;color:#e84855;font-weight:600;",
          "⚠️  Total exceeds 24 hours — reduce one or more values before predicting.")
  })

  # ── Model selector — dynamic, only fitted models enabled ─
  # Keys that map selector label → internal id
  MODEL_KEYS <- c(
    "Linear Regression (OLS)" = "ols",
    "Decision Tree"           = "tree",
    "Random Forest"           = "rf",
    "Regularised (Elastic Net)" = "glmnet",
    "KNN"                     = "knn"
  )
  MODEL_ACCENT <- c(
    ols   = "#2ec4b6",
    tree  = "#0f1b2d",
    rf    = "#f4a261",
    glmnet = "#6a4c93",
    knn   = "#2d6a4f"
  )

  # Reactive: which models are currently fitted
  fitted_flags <- reactive({
    list(
      ols   = !is.null(tryCatch(ols_model(),   error=function(e) NULL)),
      tree  = !is.null(tryCatch(tree_model(),  error=function(e) NULL)),
      rf    = !is.null(tryCatch(rf_model(),    error=function(e) NULL)),
      glmnet = !is.null(tryCatch(glmnet_cv_fit(), error=function(e) NULL)),
      knn   = !is.null(tryCatch(knn_cv(),      error=function(e) NULL))
    )
  })

  output$pred_model_selector <- renderUI({
    flags <- fitted_flags()
    any_fitted <- any(unlist(flags))

    if (!any_fitted) {
      return(div(
        style = "padding:.8rem 1rem;background:#fdfaf6;border:1px dashed #ddd8ce;border-radius:8px;font-size:.83rem;color:#7a7d8e;",
        "No models fitted yet. Go to the ",
        tags$b("Models"), " tab, choose variables, and click a Fit button."
      ))
    }

    # Build radio-button style tiles for each model
    tiles <- lapply(names(MODEL_KEYS), function(label) {
      key     <- MODEL_KEYS[[label]]
      is_fit  <- isTRUE(flags[[key]])
      accent  <- MODEL_ACCENT[[key]]

      if (is_fit) {
        first_fitted <- names(which(unlist(flags)))[1]
        inp_attrs <- list(
          type  = "radio",
          name  = "pred_model_radio",
          value = key,
          style = paste0("accent-color:", accent, ";width:14px;height:14px;flex-shrink:0;")
        )
        if (key == first_fitted) inp_attrs[["checked"]] <- NA
        radio_input <- do.call(tags$input, inp_attrs)
        tags$label(
          style = paste0(
            "display:flex;align-items:center;gap:.6rem;padding:.6rem .9rem;",
            "border-radius:8px;border:2px solid ", accent, "30;",
            "cursor:pointer;margin-bottom:.4rem;",
            "transition:background .15s,border-color .15s;"
          ),
          class = "model-sel-label",
          `data-accent` = accent,
          radio_input,
          div(
            div(style = paste0("font-size:.83rem;font-weight:600;color:", accent, ";"), label),
            div(style = "font-size:.7rem;color:#2d6a4f;font-weight:600;", "✓ Fitted")
          )
        )
      } else {
        div(
          style = paste0(
            "display:flex;align-items:center;gap:.6rem;padding:.6rem .9rem;",
            "border-radius:8px;border:2px solid #e4e0d8;",
            "margin-bottom:.4rem;opacity:.45;"
          ),
          div(style="width:14px;height:14px;border-radius:50%;border:2px solid #ccc;flex-shrink:0;"),
          div(
            div(style="font-size:.83rem;font-weight:600;color:#7a7d8e;", label),
            div(style="font-size:.7rem;color:#aaa;", "Not fitted — go to Models tab")
          )
        )
      }
    })

    tagList(
      div(tiles),
      tags$script(HTML("
        $(document).on('change', 'input[name=\"pred_model_radio\"]', function() {
          // Style selected tile
          $('.model-sel-label').each(function() {
            var acc = $(this).data('accent');
            var inp = $(this).find('input[type=radio]');
            if (inp.is(':checked')) {
              $(this).css({'background': acc + '12', 'border-color': acc});
            } else {
              $(this).css({'background': 'transparent', 'border-color': acc + '30'});
            }
          });
          Shiny.setInputValue('pred_model_choice', $(this).val(), {priority:'event'});
        });
        // Init
        setTimeout(function() {
          var first = $('input[name=\"pred_model_radio\"]:first');
          if (first.length) {
            first.prop('checked', true).trigger('change');
          }
        }, 300);
      "))
    )
  })

  # ── Build new-data frame from slider inputs ───────────────
  make_newdata <- function() {
    stress_val <- if (!is.null(input$p_stress)) as.integer(input$p_stress) else 1L
    data.frame(
      study    = input$p_study    %||% 6,
      sleep    = input$p_sleep    %||% 7,
      social   = input$p_social   %||% 2,
      exercise = input$p_exercise %||% 1,
      extra    = input$p_extra    %||% 1,
      stress   = stress_val
    )
  }

  # ── Prediction dispatcher ─────────────────────────────────
  pred_result <- eventReactive(input$run_predict, {
    choice <- input$pred_model_choice %||% "ols"
    nd     <- make_newdata()
    flags  <- fitted_flags()

    # Return error as data so only ONE panel shows it
    if (total_hours() > 24)
      return(list(error="Total hours exceed 24 — please reduce one or more sliders."))
    if (!isTRUE(flags[[choice]]))
      return(list(error=paste0("The selected model hasn't been fitted yet. ",
                               "Go to the Models tab and click its Fit button.")))

    result <- switch(choice,

      "ols" = {
        m    <- ols_model()
        # Only use variables that were actually in the model
        nd_m <- nd[, intersect(names(nd), all.vars(formula(m))[-1]), drop=FALSE]
        p    <- predict(m, newdata=nd_m, interval="prediction", level=.95)
        list(gpa=round(p[1,"fit"],2), low=round(p[1,"lwr"],2), high=round(p[1,"upr"],2),
             has_pi=TRUE, model_obj=m, type="ols", nd=nd_m)
      },

      "tree" = {
        m    <- tree_model()
        nd_m <- nd[, intersect(names(nd), attr(m$terms,"term.labels")), drop=FALSE]
        p    <- predict(m, newdata=nd_m)
        list(gpa=round(p,2), low=NA, high=NA,
             has_pi=FALSE, model_obj=m, type="tree", nd=nd_m)
      },

      "rf" = {
        m    <- rf_model()
        nd_m <- nd[, intersect(names(nd), rownames(importance(m))), drop=FALSE]
        p    <- predict(m, newdata=nd_m)
        list(gpa=round(p,2), low=NA, high=NA,
             has_pi=FALSE, model_obj=m, type="rf", nd=nd_m)
      },

      "glmnet" = {
        cv_fit <- glmnet_cv_fit()
        g_in   <- glmnet_inputs()
        nd_m   <- nd[, g_in$vars, drop=FALSE]
        X_new  <- as.matrix(nd_m)
        p      <- predict(cv_fit, newx=X_new, s="lambda.min")
        alpha  <- g_in$alpha
        method_label <- if(alpha==0) "Ridge" else if(alpha==1) "Lasso" else paste0("Elastic Net (α=",alpha,")")
        list(gpa=round(as.numeric(p),2), low=NA, high=NA,
             has_pi=FALSE, model_obj=cv_fit, type="glmnet", nd=nd_m,
             g_vars=g_in$vars, method_label=method_label)
      },

      "knn" = {
        m    <- knn_cv()
        nd_m <- nd[, intersect(names(nd), m$finalModel$xNames %||%
                     names(m$trainingData)[-ncol(m$trainingData)]), drop=FALSE]
        p    <- predict(m, newdata=nd_m)
        list(gpa=round(p,2), low=NA, high=NA,
             has_pi=FALSE, model_obj=m, type="knn", nd=nd_m)
      }
    )
    result
  }, ignoreInit=TRUE)

  # ── Result card ───────────────────────────────────────────
  output$pred_result_ui <- renderUI({
    req(pred_result())
    r <- pred_result()
    if (!is.null(r$error)) {
      return(div(class="interp-box",
        style="border-left-color:#e84855;background:#fef2f3;margin-bottom:1rem;",
        tags$b("⚠️ "), r$error
      ))
    }
    gpa <- max(0, min(4, r$gpa))
    col <- if (gpa >= 3.5) "#2ec4b6" else if (gpa >= 3.0) "#f4a261" else "#e84855"

    model_label <- if (!is.null(r$method_label)) r$method_label else {
      lbl <- names(MODEL_KEYS)[MODEL_KEYS == r$type]
      if (length(lbl)) lbl else toupper(r$type)
    }

    ci_section <- if (r$has_pi) {
      lo <- max(0, r$low); hi <- min(4, r$high)
      tagList(
        div(class="pred-ci",
          "95% Prediction Interval", tags$br(),
          tags$strong(style=paste0("color:", col, ";font-size:.95rem;"),
                      paste0("[", lo, "  –  ", hi, "]"))
        ),
        div(style="margin-top:1.2rem;padding-top:1rem;border-top:1px solid rgba(255,255,255,.12);",
          div(style="font-size:.63rem;font-weight:700;text-transform:uppercase;letter-spacing:1px;color:rgba(245,240,232,.4);margin-bottom:.4rem;",
              "GPA  0.0 ──────────── 4.0"),
          div(style="background:rgba(255,255,255,.1);border-radius:99px;height:8px;position:relative;",
            div(style=paste0("position:absolute;top:0;height:100%;border-radius:99px;",
                             "left:", round(lo/4*100,1), "%;",
                             "width:", round((hi-lo)/4*100,1), "%;",
                             "background:", col, "40;")),
            div(style=paste0("position:absolute;top:0;height:100%;border-radius:99px;",
                             "width:", round(gpa/4*100,1), "%;background:", col, ";"))
          )
        )
      )
    } else {
      tagList(
        div(class="pred-ci",
          tags$span(style="font-size:.72rem;color:rgba(245,240,232,.5);",
                    "Point estimate only — prediction intervals are not available for this model type.")
        ),
        div(style="margin-top:1.2rem;padding-top:1rem;border-top:1px solid rgba(255,255,255,.12);",
          div(style="font-size:.63rem;font-weight:700;text-transform:uppercase;letter-spacing:1px;color:rgba(245,240,232,.4);margin-bottom:.4rem;",
              "GPA  0.0 ──────────── 4.0"),
          div(style="background:rgba(255,255,255,.1);border-radius:99px;height:8px;position:relative;",
            div(style=paste0("position:absolute;top:0;height:100%;border-radius:99px;",
                             "width:", round(gpa/4*100,1), "%;background:", col, ";"))
          )
        )
      )
    }

    div(class="pred-result", style="margin-bottom:1rem;",
      div(style=paste0("font-size:.65rem;font-weight:700;text-transform:uppercase;letter-spacing:1px;",
                       "color:", MODEL_ACCENT[[r$type]], ";margin-bottom:.3rem;"),
          model_label),
      div(class="pred-label", "Predicted GPA"),
      div(class="pred-gpa", style=paste0("color:", col, ";"), gpa),
      ci_section
    )
  })

  # ── Interpretation box ────────────────────────────────────
  output$pred_interp_ui <- renderUI({
    req(pred_result())
    r <- pred_result()
    if (!is.null(r$error)) return(NULL)
    gpa <- max(0, min(4, r$gpa))
    tier <- if (gpa >= 3.7) "excellent — top-tier academic performance"
            else if (gpa >= 3.3) "strong — above the typical student"
            else if (gpa >= 3.0) "solid — around the college average"
            else if (gpa >= 2.5) "moderate — some room for improvement"
            else "below average — consider reviewing your study habits"

    model_label <- if (!is.null(r$method_label)) r$method_label else {
      lbl <- names(MODEL_KEYS)[MODEL_KEYS == r$type]
      if (length(lbl)) lbl else toupper(r$type)
    }
    pi_note <- if (r$has_pi) {
      s <- summary(r$model_obj)
      sprintf("The model (R² = %.3f) explains %.1f%% of GPA variance. The 95%% PI reflects plausible individual variation.",
              s$r.squared, s$r.squared*100)
    } else {
      paste0("Note: ", model_label, " does not produce analytical prediction intervals. ",
             "For uncertainty estimates, switch to the OLS model.")
    }

    div(class="interp-box", style="margin-bottom:1rem;",
      tags$b("Interpretation: "),
      sprintf("Using %s, the model predicts a GPA of %.2f (%s). ", model_label, gpa, tier),
      pi_note
    )
  })

  # ── Per-model insight panel ───────────────────────────────
  output$pred_breakdown_ui <- renderUI({
    req(pred_result())
    r <- pred_result()
    if (!is.null(r$error)) return(NULL)

    if (r$type == "ols") {
      # Coefficient × input contributions
      cf   <- coef(r$model_obj)
      nd   <- r$nd
      vars <- intersect(c("study","sleep","social","exercise","extra","stress"), names(nd))
      labs_map <- c(study="Study", sleep="Sleep", social="Social",
                    exercise="Exercise", extra="Extra", stress="Stress")
      cols_map <- c(study="#2ec4b6", sleep="#6a4c93", social="#f4a261",
                    exercise="#2d6a4f", extra="#e84855", stress="#7a7d8e")

      contribs <- sapply(vars, function(v) {
        cv <- cf[v]; if (is.na(cv)) 0 else cv * nd[[v]]
      })
      abs_max <- max(abs(contribs), na.rm=TRUE)
      if (!is.finite(abs_max) || abs_max == 0) abs_max <- 1

      rows <- lapply(vars, function(v) {
        val <- round(contribs[v], 3)
        if (is.na(val)) val <- 0
        pct <- abs(val) / abs_max * 100
        col <- cols_map[v]
        pos <- val >= 0
        div(style="display:flex;align-items:center;gap:.6rem;margin-bottom:.5rem;",
          div(style="width:72px;font-size:.75rem;font-weight:600;color:#2d3142;flex-shrink:0;", labs_map[v]),
          div(style="flex:1;background:#f5f0e8;border-radius:99px;height:7px;position:relative;overflow:hidden;",
            div(style=paste0("position:absolute;top:0;height:100%;border-radius:99px;background:", col, ";",
                             if(pos) paste0("left:0;width:",round(pct,1),"%;")
                             else    paste0("right:0;width:",round(pct,1),"%;")))
          ),
          div(style=paste0("width:50px;text-align:right;font-size:.75rem;font-weight:700;color:",
                           if(pos)"#2ec4b6" else "#e84855",";"),
              if(pos) paste0("+",val) else as.character(val))
        )
      })
      div(class="gpa-card",
        tags$h4("Factor Contributions"),
        tags$p(style="font-size:.78rem;color:#7a7d8e;margin-top:-.5rem;margin-bottom:.9rem;",
               "OLS coefficient × your input — each variable's pull on predicted GPA"),
        div(rows)
      )

    } else if (r$type %in% c("tree", "rf")) {
      # Variable importance from the fitted model
      imp <- if (r$type == "tree") {
        vi <- r$model_obj$variable.importance
        if (is.null(vi)) return(NULL)
        data.frame(var=names(vi), imp=as.numeric(vi), stringsAsFactors=FALSE)
      } else {
        vi <- importance(r$model_obj, type=1)
        data.frame(var=rownames(vi), imp=vi[,1], stringsAsFactors=FALSE)
      }
      imp <- imp[order(imp$imp, decreasing=TRUE),]
      imp$imp_pct <- imp$imp / max(imp$imp) * 100
      col <- if (r$type=="tree") "#0f1b2d" else "#f4a261"

      rows <- lapply(seq_len(nrow(imp)), function(i) {
        div(style="display:flex;align-items:center;gap:.6rem;margin-bottom:.5rem;",
          div(style="width:72px;font-size:.75rem;font-weight:600;color:#2d3142;flex-shrink:0;",
              imp$var[i]),
          div(style="flex:1;background:#f5f0e8;border-radius:99px;height:7px;overflow:hidden;",
            div(style=paste0("width:",round(imp$imp_pct[i],1),"%;height:100%;background:",col,";border-radius:99px;"))
          ),
          div(style=paste0("width:50px;text-align:right;font-size:.75rem;font-weight:700;color:",col,";"),
              round(imp$imp[i],1))
        )
      })
      type_label <- if(r$type=="tree") "Decision Tree" else "Random Forest"
      imp_label  <- if(r$type=="tree") "Importance score" else "% Inc. MSE"
      div(class="gpa-card",
        tags$h4("Variable Importance"),
        tags$p(style="font-size:.78rem;color:#7a7d8e;margin-top:-.5rem;margin-bottom:.9rem;",
               paste0(type_label, " importance (", imp_label, ") from your fitted model")),
        div(rows)
      )

    } else if (r$type == "glmnet") {
      # Show non-zero coefficients at lambda.min
      cf_mat  <- coef(r$model_obj, s="lambda.min")
      cf_vec  <- as.numeric(cf_mat)[-1]
      nms     <- rownames(cf_mat)[-1]
      df      <- data.frame(var=nms, coef=cf_vec, stringsAsFactors=FALSE)
      df      <- df[order(abs(df$coef), decreasing=TRUE),]
      abs_max <- max(abs(df$coef)); if(abs_max==0) abs_max <- 1
      col     <- "#6a4c93"
      method  <- r$method_label %||% "Regularised"
      alpha   <- glmnet_inputs()$alpha
      lasso_note <- if(alpha==1) " — zero coefficients dropped by Lasso" else
                    if(alpha==0) "" else " — partial shrinkage via Elastic Net"

      rows <- lapply(seq_len(nrow(df)), function(i) {
        val <- round(df$coef[i], 4)
        pct <- abs(val)/abs_max*100
        pos <- val >= 0
        div(style="display:flex;align-items:center;gap:.6rem;margin-bottom:.5rem;",
          div(style="width:72px;font-size:.75rem;font-weight:600;color:#2d3142;flex-shrink:0;", df$var[i]),
          div(style="flex:1;background:#f5f0e8;border-radius:99px;height:7px;position:relative;overflow:hidden;",
            div(style=paste0("position:absolute;top:0;height:100%;border-radius:99px;background:",col,";",
                             if(pos) paste0("left:0;width:",round(pct,1),"%;")
                             else    paste0("right:0;width:",round(pct,1),"%;")))
          ),
          div(style=paste0("width:58px;text-align:right;font-size:.75rem;font-weight:700;color:",
                           if(pos)"#2ec4b6" else "#e84855",";"),
              if(pos) paste0("+",val) else as.character(val))
        )
      })
      div(class="gpa-card",
        tags$h4(paste0(method, " Coefficients")),
        tags$p(style="font-size:.78rem;color:#7a7d8e;margin-top:-.5rem;margin-bottom:.9rem;",
               paste0("Shrunk coefficients at lambda.min", lasso_note)),
        div(rows)
      )

    } else if (r$type == "knn") {
      best_k <- r$model_obj$bestTune$k
      rmse   <- round(min(r$model_obj$results$RMSE), 3)
      div(class="gpa-card",
        tags$h4("KNN Model Info"),
        tags$p(style="font-size:.78rem;color:#7a7d8e;margin-top:-.5rem;margin-bottom:.9rem;",
               "KNN predicts by averaging the GPA of the nearest neighbours in the training data."),
        div(class="metric-grid",
          metric_box("Best k",   best_k),
          metric_box("CV RMSE",  rmse)
        ),
        div(style="font-size:.78rem;color:#7a7d8e;margin-top:.5rem;",
          "KNN has no coefficients or importances — interpretability comes from the k-plot in the Models tab."
        )
      )
    }
  })

}  # end server

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

shinyApp(ui, server)