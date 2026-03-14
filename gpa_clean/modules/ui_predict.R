# Predict tab UI
tab_predict <- tabPanel("Predict", div(class="tab-content",

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

    # ── Left: model selector + inputs ─────────────────────────────────────────
    column(7,

      div(class="gpa-card", style="margin-bottom:1rem;",
        tags$h4("Select Model"),
        tags$p(style="font-size:.8rem;color:#7a7d8e;margin-top:-.5rem;margin-bottom:1rem;",
          "Only models fitted in the Models tab are available. Head there first and click a Fit button."),
        uiOutput("pred_model_selector")
      ),

      div(class="gpa-card",
        tags$h4("Daily Hour Allocation"),

        # 24-hr budget bar
        div(style="margin-bottom:1.5rem;",
          div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:.4rem;",
            tags$span(style="font-size:.72rem;font-weight:700;text-transform:uppercase;letter-spacing:.9px;color:#7a7d8e;",
                      "Hours used today"),
            uiOutput("budget_label")
          ),
          div(style="background:#e4e0d8;border-radius:99px;height:11px;overflow:hidden;",
            uiOutput("budget_bar")),
          uiOutput("budget_warning")
        ),

        make_pred_slider("p_study",    "📚  Study",           6, "#2ec4b6"),
        make_pred_slider("p_sleep",    "😴  Sleep",           7, "#6a4c93"),
        make_pred_slider("p_social",   "💬  Social",          2, "#f4a261"),
        make_pred_slider("p_exercise", "🏃  Exercise",        1, "#2d6a4f"),
        make_pred_slider("p_extra",    "🎭  Extracurricular", 1, "#e84855"),

        # Stress level radio
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

    # ── Right: results ────────────────────────────────────────────────────────
    column(5,
      uiOutput("pred_result_ui"),
      uiOutput("pred_interp_ui"),
      uiOutput("pred_breakdown_ui")
    )
  ),

  # Stress radio JS
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
