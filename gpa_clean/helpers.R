#  helpers.R: shared utilities 

load_data <- function(path) {
  df <- read.csv(path, stringsAsFactors=FALSE)
  # Rename columns
  rename_map <- c(
    Study_Hours_Per_Day = "study",
    Sleep_Hours_Per_Day = "sleep",
    Social_Hours_Per_Day = "social",
    Physical_Activity_Hours_Per_Day = "exercise",
    Extracurricular_Hours_Per_Day = "extra",
    Stress_Level = "stress"
  )
  for (old in names(rename_map)) {
    new <- rename_map[[old]]
    if (old %in% names(df) && !new %in% names(df))
      names(df)[names(df) == old] <- new
  }

  # Convert stress Low, Moderate, High to 0, 1, 2
  lvls <- c("Low","Moderate","High")
  present <- unique(df$stress)
  use_lvls <- intersect(lvls, present)
  if (length(use_lvls) == 0) use_lvls <- sort(present)
  df$stress <- as.integer(factor(df$stress, levels=use_lvls)) - 1L

  key_cols <- intersect(c("GPA","study","sleep","social","exercise","extra", "stress"), names(df))
  # Remove rows with missing data
  df[complete.cases(df[, key_cols, drop=FALSE]), ]
}

# Unify the theme of visualizations
theme_gpa <- function(base_size=11) {
  ggplot2::theme_minimal(base_size=base_size, base_family="DM Sans") +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill="transparent", colour=NA),
      panel.background = ggplot2::element_rect(fill="transparent", colour=NA),
      panel.grid.major = ggplot2::element_line(colour="#ece8e0", linewidth=.4),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(colour="#7a7d8e", size=8.5),
      axis.title       = ggplot2::element_text(colour="#2d3142", size=9),
      plot.title       = ggplot2::element_text(colour="#0f1b2d", face="bold", size=10.5,
                                               margin=ggplot2::margin(b=6)),
      strip.text       = ggplot2::element_text(colour="#2d3142", face="bold", size=9),
      legend.text      = ggplot2::element_text(colour="#2d3142", size=8.5),
      legend.title     = ggplot2::element_text(colour="#2d3142", size=8.5, face="bold"),
      plot.margin      = ggplot2::margin(6, 8, 6, 8)
    )
}

# Visualize regression coefficients with CIs
plot_coefs <- function(model) {
  coefs <- coef(model)
  ci    <- suppressMessages(confint(model))

  idx <- names(coefs) != "(Intercept)"

  df <- data.frame(
    term = names(coefs)[idx],
    est  = coefs[idx],
    lo   = ci[idx,1],
    hi   = ci[idx,2]
  )
  
  # Clean labels
  df$label <- gsub("stress", "Stress", df$term)
  df$label <- gsub("I\\((.+)\\^2\\)", "\\1²", df$label)
  df$label <- gsub(":", " × ", df$label)

  # Order by effect size
  df <- df[order(abs(df$est)), ]
  df$label <- factor(df$label, levels=df$label)

  # Significant if CI does not cross 0
  sig <- ifelse(df$lo > 0 | df$hi < 0, "#2ec4b6", "#e84855")

  ggplot(df, aes(x=est, y=label)) +
    geom_vline(xintercept=0, linetype="dashed", colour="#bbb", linewidth=.6) +
    geom_errorbarh(aes(xmin=lo, xmax=hi), height=.25, colour=sig, linewidth=.75) +
    geom_point(size=2.8, colour=sig) +
    labs(x="Coefficient Estimate (95% CI)", y=NULL) +
    theme_gpa() +
    theme(axis.text.y=element_text(size=8))
}

# Visualize correlation matrix as heatmap
plot_corr_heatmap <- function(df, var_vals, var_labels) {
  all_vars   <- c(var_vals, "GPA")
  all_labels <- c(var_labels, "GPA")
  cor_mat <- cor(df[, all_vars], use="complete.obs")
  rownames(cor_mat) <- colnames(cor_mat) <- all_labels
  long <- as.data.frame(as.table(cor_mat))
  names(long) <- c("Var1","Var2","r")
  long$Var1 <- factor(long$Var1, levels=all_labels)
  long$Var2 <- factor(long$Var2, levels=rev(all_labels))
  ggplot2::ggplot(long, ggplot2::aes(Var1, Var2, fill=r)) +
    ggplot2::geom_tile(colour="white", linewidth=.5) +
    ggplot2::geom_text(ggplot2::aes(label=round(r,2),
                                    colour=abs(r) > .5),
                       size=3, fontface="bold") +
    ggplot2::scale_fill_gradient2(low="#e84855", mid="#f5f0e8", high="#2ec4b6",
                                  midpoint=0, limits=c(-1,1), name="r") +
    ggplot2::scale_colour_manual(values=c("TRUE"="white","FALSE"="#2d3142"), guide="none") +
    ggplot2::coord_fixed() +
    ggplot2::labs(x=NULL, y=NULL) + theme_gpa() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=35, hjust=1, size=8),
                   axis.text.y=ggplot2::element_text(size=8),
                   panel.grid=ggplot2::element_blank(),
                   legend.position="right")
}

# Metric Box UI
metric_box <- function(label, value, desc = NULL) {
  shiny::tags$div(class="metric-box",
    shiny::tags$div(class="metric-label", label),
    if (!is.null(desc))
      shiny::tags$div(class="control-hint", desc),
    shiny::tags$div(class="metric-value", as.character(value)),
    
  )
}