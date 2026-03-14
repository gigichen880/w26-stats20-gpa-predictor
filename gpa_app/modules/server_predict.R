# Predict tab

# ── Slider value displays ──────────────────────────────────────────────────────
lapply(list(
  list("p_study",    "#2ec4b6"),
  list("p_sleep",    "#6a4c93"),
  list("p_social",   "#f4a261"),
  list("p_exercise", "#2d6a4f"),
  list("p_extra",    "#e84855")
), function(x) {
  local({
    id <- x[[1]]; col <- x[[2]]
    output[[paste0(id, "_val_display")]] <- renderUI({
      tags$span(style=paste0("font-size:.82rem;font-weight:700;color:",col,";"),
                paste0(input[[id]] %||% 0, " hrs"))
    })
  })
})

# ── 24-hr budget ───────────────────────────────────────────────────────────────
total_hours <- reactive({
  round((input$p_study %||% 0) + (input$p_sleep %||% 0) +
        (input$p_social %||% 0) + (input$p_exercise %||% 0) +
        (input$p_extra %||% 0), 1)
})

# Budge Progress Bar
output$budget_bar <- renderUI({
  pct <- min(total_hours() / 24 * 100, 100)
  col <- if (pct <= 70) "#2ec4b6"
         else if (pct <= 90) "#f4a261"
         else "#e84855"
  div(
    style="background:#e9e6df;border-radius:99px;height:14px;width:100%;overflow:hidden;",
    div(
      style=paste0(
        "height:100%;width:", pct, "%;",
        "background:", col, ";",
        "border-radius:99px;",
        "transition:width .25s ease;"
      )
    )
  )
})


# Budget Label
output$budget_label <- renderUI({
  tot <- total_hours(); rem <- round(24 - tot, 1)
  col <- if (tot <= 17) "#2ec4b6" else if (tot <= 22) "#f4a261" else "#e84855"
  tags$span(style=paste0("font-size:.82rem;font-weight:700;color:",col,";"),
            paste0(tot, " / 24 hrs"))
})

# Budget Warning
output$budget_warning <- renderUI({
  if (total_hours() > 24)
    div(style="margin-top:.5rem;padding:.4rem .8rem;background:#fef2f3;border-left:3px solid #e84855;border-radius:0 6px 6px 0;font-size:.78rem;color:#e84855;font-weight:600;",
        "!!! Total exceeds 24 hours: reduce one or more values before predicting.")
})

# ── Model selector ─────────────────────────────────────────────────────────────
MODEL_KEYS <- c(
  "Linear Regression (OLS)" = "ols",
  "Decision Tree" = "tree",
  "Random Forest" = "rf",
  "Regularized OLS" = "glmnet",
  "KNN" = "knn"
)

# Detected fitted models
fitted_flags <- reactive({
  list(
    ols = !is.null(tryCatch(ols_model(), error=function(e) NULL)),
    tree = !is.null(tryCatch(tree_model(), error=function(e) NULL)),
    rf = !is.null(tryCatch(rf_model(), error=function(e) NULL)),
    glmnet = !is.null(tryCatch(glmnet_cv_fit(), error=function(e) NULL)),
    knn = !is.null(tryCatch(knn_cv(), error=function(e) NULL))
  )
})

# Select fitted model
output$pred_model_selector <- renderUI({
  flags <- fitted_flags()
  if (!any(unlist(flags))) {
    return(div(
      style="padding:.8rem 1rem;background:#fdfaf6;border:1px dashed #ddd8ce;border-radius:8px;font-size:.83rem;color:#7a7d8e;",
      "No models fitted yet. Go to the ", tags$b("Models"), " tab, choose variables, and click a Fit button."
    ))
  }

  tiles <- lapply(names(MODEL_KEYS), function(label) {
    key    <- MODEL_KEYS[[label]]
    is_fit <- isTRUE(flags[[key]])
    accent <- MODEL_ACCENT[[key]]

    if (is_fit) {
      first_fitted <- names(which(unlist(flags)))[1]
      inp_attrs <- list(type="radio", name="pred_model_radio", value=key,
                        style=paste0("accent-color:",accent,";width:14px;height:14px;flex-shrink:0;"))
      if (key == first_fitted) inp_attrs[["checked"]] <- NA
      tags$label(
        style=paste0("display:flex;align-items:center;gap:.6rem;padding:.6rem .9rem;",
                     "border-radius:8px;border:2px solid ",accent,"30;",
                     "cursor:pointer;margin-bottom:.4rem;transition:background .15s,border-color .15s;"),
        class="model-sel-label", `data-accent`=accent,
        do.call(tags$input, inp_attrs),
        div(
          div(style=paste0("font-size:.83rem;font-weight:600;color:",accent,";"), label),
          div(style="font-size:.7rem;color:#2d6a4f;font-weight:600;", "✓ Fitted")
        )
      )
    } else {
      div(style=paste0("display:flex;align-items:center;gap:.6rem;padding:.6rem .9rem;",
                       "border-radius:8px;border:2px solid #e4e0d8;margin-bottom:.4rem;opacity:.45;"),
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
      setTimeout(function() {
        var first = $('input[name=\"pred_model_radio\"]:first');
        if (first.length) first.prop('checked', true).trigger('change');
      }, 300);
    "))
  )
})

# ── Prediction dispatcher ──────────────────────────────────────────────────────
make_newdata <- function() {
  data.frame(
    study = input$p_study %||% 6,
    sleep = input$p_sleep %||% 7,
    social = input$p_social %||% 2,
    exercise = input$p_exercise %||% 1,
    extra = input$p_extra %||% 1,
    stress = if (!is.null(input$p_stress)) as.integer(input$p_stress) else 1L
  )
}

# Predict GPA using the selected fitted model
pred_result <- eventReactive(input$run_predict, {
  choice <- input$pred_model_choice %||% "ols"
  nd     <- make_newdata()
  flags  <- fitted_flags()

  if (total_hours() > 24)
    return(list(error="Total hours exceed 24. Please reduce one or more sliders."))
  if (!isTRUE(flags[[choice]]))
    return(list(error="The selected model hasn't been fitted yet. Go to the Models tab and click its Fit button."))

  switch(choice,
    "ols" = {
      m <- ols_model()
      nd_m <- nd[, intersect(names(nd), all.vars(formula(m))[-1]), drop=FALSE]
      p <- predict(m, newdata=nd_m, interval="prediction", level=.95)
      list(gpa=round(p[1,"fit"],2), low=round(p[1,"lwr"],2), high=round(p[1,"upr"],2),
           has_pi=TRUE, model_obj=m, type="ols", nd=nd_m)
    },
    "tree" = {
      m <- tree_model()
      nd_m <- nd[, intersect(names(nd), attr(m$terms,"term.labels")), drop=FALSE]
      list(gpa=round(predict(m, newdata=nd_m),2), low=NA, high=NA,
           has_pi=FALSE, model_obj=m, type="tree", nd=nd_m)
    },
    "rf" = {
      m <- rf_model()
      nd_m <- nd[, intersect(names(nd), rownames(importance(m))), drop=FALSE]
      list(gpa=round(predict(m, newdata=nd_m),2), low=NA, high=NA,
           has_pi=FALSE, model_obj=m, type="rf", nd=nd_m)
    },
    # Uses the best lambda from CV
    "glmnet" = {
      cv_fit <- glmnet_cv_fit(); g_in <- glmnet_inputs()
      nd_m <- nd[, g_in$vars, drop=FALSE]
      p <- predict(cv_fit, newx=as.matrix(nd_m), s="lambda.min")
      alpha <- g_in$alpha
      method_label <- if (alpha==0) "Ridge" else if (alpha==1) "Lasso" else
                        paste0("Elastic Net (alpha=",alpha,")")
      list(gpa=round(as.numeric(p),2), low=NA, high=NA,
           has_pi=FALSE, model_obj=cv_fit, type="glmnet", nd=nd_m,
           g_vars=g_in$vars, method_label=method_label)
    },
    "knn" = {
      m <- knn_cv()
      nd_m <- nd[, intersect(names(nd), m$finalModel$xNames %||%
                   names(m$trainingData)[-ncol(m$trainingData)]), drop=FALSE]
      list(gpa=round(predict(m, newdata=nd_m),2), low=NA, high=NA,
           has_pi=FALSE, model_obj=m, type="knn", nd=nd_m)
    }
  )
}, ignoreInit=TRUE)

# ── Result card ────────────────────────────────────────────────────────────────
output$pred_result_ui <- renderUI({
  req(pred_result()); r <- pred_result()
  if (!is.null(r$error))
    return(div(class="interp-box",
               style="border-left-color:#e84855;background:#fef2f3;margin-bottom:1rem;",
               r$error))

  gpa <- max(0, min(4, r$gpa))
  col <- if (gpa >= 3.5) "#2ec4b6" else if (gpa >= 3.0) "#f4a261" else "#e84855"
  model_label <- if (!is.null(r$method_label)) r$method_label else {
    lbl <- names(MODEL_KEYS)[MODEL_KEYS == r$type]
    if (length(lbl)) lbl else toupper(r$type)
  }

  ci_section <- if (r$has_pi) {
    lo <- max(0, r$low); hi <- min(4, r$high)
    tagList(
      div(class="pred-ci", "95% Prediction Interval", tags$br(),
          tags$strong(style=paste0("color:",col,";font-size:.95rem;"),
                      paste0("[", lo, "  –  ", hi, "]"))),
      div(style="margin-top:1.2rem;padding-top:1rem;border-top:1px solid rgba(255,255,255,.12);",
        div(style="font-size:.63rem;font-weight:700;text-transform:uppercase;letter-spacing:1px;color:rgba(245,240,232,.4);margin-bottom:.4rem;",
            "GPA  0.0 ──────────── 4.0"),
        div(style="background:rgba(255,255,255,.1);border-radius:99px;height:8px;position:relative;",
          div(style=paste0("position:absolute;top:0;height:100%;border-radius:99px;left:",round(lo/4*100,1),"%;width:",round((hi-lo)/4*100,1),"%;background:",col,"40;")),
          div(style=paste0("position:absolute;top:0;height:100%;border-radius:99px;width:",round(gpa/4*100,1),"%;background:",col,";"))
        )
      )
    )
  } else {
    tagList(
      div(class="pred-ci",
          tags$span(style="font-size:.72rem;color:rgba(245,240,232,.5);",
                    "Point estimate only. Prediction intervals not available for this model type.")),
      div(style="margin-top:1.2rem;padding-top:1rem;border-top:1px solid rgba(255,255,255,.12);",
        div(style="font-size:.63rem;font-weight:700;text-transform:uppercase;letter-spacing:1px;color:rgba(245,240,232,.4);margin-bottom:.4rem;",
            "GPA  0.0 ──────────── 4.0"),
        div(style="background:rgba(255,255,255,.1);border-radius:99px;height:8px;position:relative;",
          div(style=paste0("position:absolute;top:0;height:100%;border-radius:99px;width:",round(gpa/4*100,1),"%;background:",col,";")))
      )
    )
  }

  div(class="pred-result", style="margin-bottom:1rem;",
    div(style=paste0("font-size:.65rem;font-weight:700;text-transform:uppercase;letter-spacing:1px;color:",MODEL_ACCENT[[r$type]],";margin-bottom:.3rem;"), model_label),
    div(class="pred-label", "Predicted GPA"),
    div(class="pred-gpa", style=paste0("color:",col,";"), gpa),
    ci_section
  )
})

# ── Interpretation box ─────────────────────────────────────────────────────────
output$pred_interp_ui <- renderUI({
  req(pred_result()); r <- pred_result()
  if (!is.null(r$error)) return(NULL)
  gpa <- max(0, min(4, r$gpa))
  # Thresholds are quantiles of the data 
  tier <- if (gpa >= gpa_breaks[4]) { "Excellent! Top-tier academic performance." } 
          else if (gpa >= gpa_breaks[3]) { "Strong! Above the typical student." } 
          else if (gpa >= gpa_breaks[2]) { "Solid! Around the college average." } 
          else if (gpa >= gpa_breaks[1]) { "Moderate! Some room for improvement." } 
          else { "Below average! Consider reviewing your study habits." }

  model_label <- if (!is.null(r$method_label)) r$method_label else {
    lbl <- names(MODEL_KEYS)[MODEL_KEYS == r$type]; if(length(lbl)) lbl else toupper(r$type)
  }
  pi_note <- if (r$has_pi) {
    s <- summary(r$model_obj)
    sprintf("The model (R-Squared = %.3f) explains %.1f%% of GPA variance. The 95%% prediction interval reflects plausible individual variation.",
            s$r.squared, s$r.squared*100)
  } else {
    paste0("Note: ", model_label, " does not produce analytical prediction intervals. For uncertainty estimates, switch to OLS.")
  }
  div(class="interp-box", style="margin-bottom:1rem;",
    sprintf("%s", tier),
    pi_note
  )
})
