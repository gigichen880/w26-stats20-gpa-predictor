# Overview tab: summary of dataset, EDA

# Dataset Summary
output$overview_stats <- renderUI({
  div(class="metric-grid",
    metric_box("Observations", nrow(data)),
    metric_box("# Predictors",   length(NUMERIC_VARS)+1L), # One categorical Stress_Level
    metric_box("Avg GPA",      round(mean(data$GPA, na.rm=TRUE), 2)),
    metric_box("GPA Range",    paste0(round(min(data$GPA), 2), " – ", round(max(data$GPA), 2)))
  )
})

# Histogram of GPA
output$overview_hist <- renderPlot({
  ggplot(data, aes(x=GPA)) +
    geom_histogram(bins=28, fill="#2ec4b6", colour="#1a8c84", alpha=.85) +
    geom_vline(xintercept=mean(data$GPA), colour="#0f1b2d", linetype="dashed", linewidth=.7) +
    annotate("text", x=mean(data$GPA)+.04, y=Inf, vjust=1.7,
             label=paste0("mean = ", round(mean(data$GPA), 2)),
             size=3, colour="#0f1b2d", fontface="bold", hjust=0) +
    labs(x="GPA", y="Count") + theme_gpa()
}, bg="transparent")

# 5 scatterplots of GPA vs numeric variables
output$overview_scatter <- renderPlot({
  nv <- unname(NUMERIC_VARS); nl <- names(NUMERIC_VARS)
  long <- data |>
    pivot_longer(all_of(nv), names_to="variable", values_to="hours") |> # wide to long
    mutate(variable=recode(variable, !!!setNames(nl, nv)))
  ggplot(long, aes(x=hours, y=GPA)) +
    geom_point(alpha=.18, size=.85, colour="#2ec4b6") +
    # Added local regression (smooth curve)
    geom_smooth(method="loess", formula=y~x, se=TRUE,
                colour="#e84855", fill="#e84855", alpha=.12, linewidth=.9) + 
    geom_smooth(method="lm", formula=y~x, se=FALSE,
                colour="#0f1b2d", linetype="dashed", linewidth=.65) +
    facet_wrap(~variable, nrow=1, scales="free_x") + # One plot per variable
    labs(x="Daily Hours", y="GPA") + theme_gpa() +
    theme(strip.text=element_text(face="bold", size=8.5),
          strip.background=element_rect(fill="#f0ede6", colour=NA))
}, bg="transparent")

# Correlation heatmap among numeric variables
output$overview_heatmap <- renderPlot({
  plot_corr_heatmap(data, unname(NUMERIC_VARS), names(NUMERIC_VARS))
}, bg="transparent")

# Side-by-side boxplots of stress levels
output$overview_stress <- renderPlot({
  df <- data |>
    mutate(stress = factor(stress,
                           levels = c(0,1,2),
                           labels = c("Low","Moderate","High")))
  ggplot(df, aes(x=stress, y=GPA, fill=stress)) +
    geom_boxplot(alpha=.75, width=.6, outlier.shape=NA) +
    geom_jitter(width=.15, alpha=.2, size=.8, colour="#2ec4b6") +
    scale_fill_manual(values=c("#2ec4b6","#f4a261","#e84855")) +
    labs(x="Stress Level", y="GPA") + theme_gpa() + theme(legend.position="none")
}, bg="transparent")
