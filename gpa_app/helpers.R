# ============================================================
#  helpers.R  —  shared utilities for GPA Lifestyle Analyzer
# ============================================================

load_data <- function(path) {
  df <- read.csv(path, stringsAsFactors=FALSE)
  rename_map <- c(
    Study_Hours_Per_Day             = "study",
    Sleep_Hours_Per_Day             = "sleep",
    Social_Hours_Per_Day            = "social",
    Physical_Activity_Hours_Per_Day = "exercise",
    Extracurricular_Hours_Per_Day   = "extra"
  )
  for (old in names(rename_map)) {
    new <- rename_map[[old]]
    if (old %in% names(df) && !new %in% names(df))
      names(df)[names(df) == old] <- new
  }
  if (!"GPA" %in% names(df)) {
    gpa_col <- names(df)[tolower(names(df)) == "gpa"]
    if (length(gpa_col) == 1) names(df)[names(df) == gpa_col] <- "GPA"
  }
  # Normalise stress column name if present
  stress_candidates <- names(df)[grepl("stress", names(df), ignore.case=TRUE)]
  if (length(stress_candidates) > 0 && !"Stress_Level" %in% names(df)) {
    names(df)[names(df) == stress_candidates[1]] <- "Stress_Level"
  }

  # Convert stress to ordered factor if present
  if ("Stress_Level" %in% names(df)) {
    lvls <- c("Low","Moderate","High")
    present <- unique(df$Stress_Level)
    # use whatever levels exist, fall back to alphabetical
    use_lvls <- intersect(lvls, present)
    if (length(use_lvls) == 0) use_lvls <- sort(present)
    df$Stress_Level <- factor(df$Stress_Level, levels=use_lvls, ordered=TRUE)
  }

  key_cols <- intersect(c("GPA","study","sleep","social","exercise","extra"), names(df))
  df[complete.cases(df[, key_cols, drop=FALSE]), ]
}

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

plot_coefs <- function(model) {
  coefs <- coef(model)
  ci    <- suppressMessages(confint(model))
  idx   <- -1
  df <- data.frame(term=names(coefs)[idx], est=coefs[idx],
                   lo=ci[idx,1], hi=ci[idx,2])
  df$term <- factor(df$term, levels=df$term[order(df$est)])
  df$sign <- ifelse(df$est >= 0, "pos", "neg")
  ggplot2::ggplot(df, ggplot2::aes(est, term, colour=sign)) +
    ggplot2::geom_vline(xintercept=0, linetype="dashed", colour="#ccc", linewidth=.7) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin=lo, xmax=hi),
                            height=.25, linewidth=.7, alpha=.6) +
    ggplot2::geom_point(size=3) +
    ggplot2::scale_colour_manual(values=c(pos="#2ec4b6", neg="#e84855"), guide="none") +
    ggplot2::labs(x="Coefficient Estimate (95% CI)", y=NULL) + theme_gpa()
}

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

metric_box <- function(label, value) {
  shiny::tags$div(class="metric-box",
    shiny::tags$div(class="metric-label", label),
    shiny::tags$div(class="metric-value", as.character(value))
  )
}