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

# ── Data ──────────────────────────────────────────────────────────────────────
data <- load_data("data/student_lifestyle_dataset.csv")
gpa_breaks <- quantile(data$GPA, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)

# ── Shared constants ───────────────────────────────────────────────────────────
NUMERIC_VARS <- c(
  "Study Hours"         = "study",
  "Sleep Hours"         = "sleep",
  "Social Hours"        = "social",
  "Exercise Hours"      = "exercise",
  "Extracurricular Hrs" = "extra"
)

MODEL_COLORS <- c(
  "Linear Regression"   = "#2ec4b6",
  "Decision Tree"       = "#0f1b2d",
  "Random Forest"       = "#f4a261",
  "Ridge"               = "#6a4c93",
  "Lasso"               = "#6a4c93",
  "Elastic Net (α=0.5)" = "#6a4c93",
  "KNN"                 = "#2d6a4f"
)

MODEL_ACCENT <- c(
  ols    = "#2ec4b6",
  tree   = "#0f1b2d",
  rf     = "#f4a261",
  glmnet = "#6a4c93",
  knn    = "#2d6a4f"
)

# Resolve colour for any model name; handles dynamic Elastic Net labels
model_color <- function(nm) {
  if (nm %in% names(MODEL_COLORS)) return(MODEL_COLORS[[nm]])
  if (grepl("Elastic Net|Ridge|Lasso", nm)) return("#6a4c93")
  "#7a7d8e"
}

# ── Shared UI helpers ──────────────────────────────────────────────────────────
term_badge <- function(txt, col = "#2ec4b6")
  tags$span(txt, style = paste0(
    "display:inline-block;font-size:.68rem;font-weight:700;",
    "text-transform:uppercase;letter-spacing:.6px;padding:.15rem .55rem;",
    "border-radius:20px;border:1px solid ", col, ";color:", col,
    ";margin-right:.3rem;margin-bottom:.3rem;"
  ))


# Checkbox grid used in every model sidebar panel
var_grid_ui <- function(ns, show_quadratic = FALSE, default_checked = "study") {
  cb_name_me <- paste0(ns, "_main_effects")
  cb_name_qe <- paste0(ns, "_quadratic_terms")
  cb_cls     <- paste0("vgcb-", ns)

  col_headers <- if (show_quadratic) c("Variable", "Linear", "Quadratic")
                 else                c("Variable", "Include")

  th_style_var   <- "padding:.38rem .7rem;font-size:.67rem;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#2ec4b6;text-align:left;"
  th_style_chk   <- "padding:.38rem .7rem;font-size:.67rem;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#2ec4b6;text-align:center;width:62px;"
  td_style_var   <- "padding:.32rem .7rem;font-size:.83rem;color:#2d3142;border-top:1px solid #eee;"
  td_style_chk   <- "padding:.32rem .7rem;text-align:center;border-top:1px solid #eee;"
  td_style_var_s <- "padding:.32rem .7rem;font-size:.83rem;color:#7a7d8e;font-style:italic;border-top:2px dashed #ddd8ce;background:#fdfaf6;"
  td_style_chk_s <- "padding:.32rem .7rem;text-align:center;border-top:2px dashed #ddd8ce;background:#fdfaf6;"

  make_numeric_row <- function(label, val) {
    cb_me <- tags$input(type="checkbox", name=cb_name_me, value=val,
                        class=paste("var-grid-cb", cb_cls),
                        style="width:15px;height:15px;accent-color:#2ec4b6;cursor:pointer;",
                        checked=if(val %in% default_checked) NA else NULL)
    if (show_quadratic) {
      cb_qe <- tags$input(type="checkbox", name=cb_name_qe, value=paste0(val, "^2"),
                          class=paste("var-grid-cb", cb_cls),
                          style="width:15px;height:15px;accent-color:#2ec4b6;cursor:pointer;")
      tags$tr(tags$td(style=td_style_var, label),
              tags$td(style=td_style_chk, cb_me),
              tags$td(style=td_style_chk, cb_qe))
    } else {
      tags$tr(tags$td(style=td_style_var, label),
              tags$td(style=td_style_chk, cb_me))
    }
  }

  stress_cb    <- tags$input(type="checkbox", name=cb_name_me, value="stress",
                              class=paste("var-grid-cb", cb_cls),
                              style="width:15px;height:15px;accent-color:#2ec4b6;cursor:pointer;")
  stress_qe_cb <- tags$input(type="checkbox", name=cb_name_qe, value="stress^2",
                              class=paste("var-grid-cb", cb_cls),
                              style="width:15px;height:15px;accent-color:#2ec4b6;cursor:pointer;")
  stress_row <- if (show_quadratic) {
    tags$tr(
      tags$td(style=td_style_var_s, tagList("Stress Level ",
        tags$span(style="font-size:.68rem;color:#aaa;font-style:normal;", "(0=Low 1=Mod 2=High)"))),
      tags$td(style=td_style_chk_s, stress_cb),
      tags$td(style=td_style_chk_s, stress_qe_cb))
  } else {
    tags$tr(
      tags$td(style=td_style_var_s, tagList("Stress Level ",
        tags$span(style="font-size:.68rem;color:#aaa;font-style:normal;", "(0/1/2)"))),
      tags$td(style=td_style_chk_s, stress_cb))
  }

  numeric_rows <- lapply(seq_along(NUMERIC_VARS), function(i)
    make_numeric_row(names(NUMERIC_VARS)[i], unname(NUMERIC_VARS)[i]))

  tagList(
    tags$table(
      style = "width:100%;border-collapse:collapse;border:1px solid #e4e0d8;border-radius:8px;overflow:hidden;font-family:'DM Sans',sans-serif;",
      tags$thead(tags$tr(style="background:#0f1b2d;",
        lapply(col_headers, function(h)
          tags$th(style=if(h=="Variable") th_style_var else th_style_chk, h)))),
      tags$tbody(c(numeric_rows, list(stress_row)))
    ),
    tags$script(HTML(sprintf("
      $(document).on('change', '.%s', function() {
        var nm = $(this).attr('name');
        var vals = [];
        $('input.%s[name=\"' + nm + '\"]:checked').each(function() { vals.push($(this).val()); });
        Shiny.setInputValue(nm, vals, {priority:'event'});
      });
      $(document).on('shiny:connected', function() {
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

# Interaction checkbox grid (OLS only)
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
      abbrev[j]))

  rows <- lapply(1:(n-1), function(i) {
    cells <- list(tags$td(
      style="font-size:.72rem;font-weight:600;color:#2d3142;padding:.22rem .5rem .22rem 0;white-space:nowrap;",
      abbrev[i]))
    for (k in seq_len(i-1)) cells[[length(cells)+1]] <- tags$td(style="width:38px;")
    for (j in (i+1):n) {
      ikey <- paste(vars[i], vars[j], sep=":")
      cells[[length(cells)+1]] <- tags$td(style="width:38px;padding:2px;",
        tags$div(style="display:flex;justify-content:center;align-items:center;width:30px;height:30px;border-radius:5px;background:#f8f6f0;margin:auto;",
          tags$input(type="checkbox", name=cb_nm, value=ikey,
                     class=paste("var-grid-cb", cb_cls),
                     title=paste(labels[i], "×", labels[j]),
                     style="width:14px;height:14px;accent-color:#2ec4b6;cursor:pointer;margin:0;")))
    }
    tags$tr(cells)
  })

  tagList(
    tags$div(style="overflow-x:auto;",
      tags$table(
        style="border-collapse:separate;border-spacing:3px;width:fit-content;font-family:'DM Sans',sans-serif;",
        tags$thead(tags$tr(c(list(tags$th(style="width:44px;")), col_ths))),
        tags$tbody(rows))),
    tags$script(HTML(sprintf("
      $(document).on('change', '.%s', function() {
        var vals = [];
        $('input.%s:checked').each(function() { vals.push($(this).val()); });
        Shiny.setInputValue('%s', vals, {priority:'event'});
      });
    ", cb_cls, cb_cls, cb_nm)))
  )
}

# Coloured slider row used in Predict tab
make_pred_slider <- function(id, label, default_val, accent_color) {
  tagList(
    div(style = "margin-bottom:.9rem;",
      div(style = "display:flex; justify-content:space-between; align-items:baseline; margin-bottom:.1rem;",
        tags$span(style = "font-size:.84rem; font-weight:600; color:#2d3142;", label),
        uiOutput(paste0(id, "_val_display"), inline = TRUE)
      ),
      sliderInput(id, NULL, min=0, max=16, value=default_val, step=0.5, width="100%")
    ),
    tags$style(HTML(paste0(
      "#", id, " .irs--shiny .irs-bar    { background:", accent_color, " !important; border-color:", accent_color, " !important; }",
      "#", id, " .irs--shiny .irs-handle { background:", accent_color, " !important; border-color:", accent_color, " !important; }",
      "#", id, " .irs--shiny .irs-single { background:", accent_color, " !important; }"
    )))
  )
}

# ── Shared server helpers ──────────────────────────────────────────────────────

# 5-fold cross-validation
cv5 <- function(save_preds = TRUE)
  trainControl(method="cv", number=5,
               savePredictions=if(save_preds) "final" else "none")

# Build regression formula using string manipulation
build_formula <- function(main_vars, quad=NULL, inter=NULL) {
  if (is.null(main_vars) || length(main_vars)==0) return(NULL)
  terms <- c(main_vars,
             if (!is.null(quad))  paste0("I(", quad, ")"),
             if (!is.null(inter)) inter)
  as.formula(paste("GPA ~", paste(terms, collapse="+")))
}

# Read R-shiny inputs
get_inp <- function(input, nm) {
  v <- input[[nm]]
  if (is.null(v) || length(v)==0) NULL else v
}

# Helper: gracefully handle missing values
`%||%` <- function(a, b) if (!is.null(a)) a else b
