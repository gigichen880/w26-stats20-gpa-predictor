# Models tab: OLS, Decision Tree, Random Forest, Regularized OLS, KNN, Compare All Models

# ── OLS ────────────────────────────────────────────────────────────────────────

# Build OLS Formula
ols_formula <- eventReactive(input$run_ols, {
  main  <- isolate(get_inp(input, "ols_main_effects"))
  quad  <- isolate(get_inp(input, "ols_quadratic_terms"))
  inter <- isolate(get_inp(input, "ols_interaction_terms"))
  build_formula(main, quad, inter)
})

# Run OLS 
ols_model <- reactive({
  lm(ols_formula(), data=data)
})

# Run OLS CV5
ols_cv <- reactive({
  train(ols_formula(), data=data, method="lm", trControl=cv5())
})

# OLS metrics panel
output$ols_metrics_ui <- renderUI({
  req(ols_model(), ols_cv())

  m <- ols_model()
  cv <- ols_cv()

  s <- summary(m)

  r2  <- round(s$r.squared, 4)
  ar2 <- round(s$adj.r.squared, 4)
  rmse_txt <- round(cv$results$RMSE[1], 4)

  aic  <- round(AIC(m), 1)

  f <- s$fstatistic
  pval <- if (!is.null(f)) pf(f[1], f[2], f[3], lower.tail=FALSE) else NA

  pval_txt <- if (!is.na(pval)) {
    if (pval < 0.001) "< 0.001" else as.character(round(pval, 4))
  } else "—"

  div(class="metric-grid",
    metric_box("R-Sqaured", r2),
    metric_box("Adj. R-Sqaured", ar2),
    metric_box("CV RMSE", rmse_txt),
    metric_box("AIC", aic),
    metric_box("F p-val", pval_txt),
    metric_box("# Variables", length(coef(m)) - 1L)
  )
})

output$ols_summary <- renderPrint({ req(ols_model()); summary(ols_model()) })

# Coefficient Estimates
output$ols_coef_plot <- renderPlot({
  req(ols_model())
  plot_coefs(ols_model())
}, bg="transparent")

# Residuals vs Fitted
output$ols_resid_plot <- renderPlot({
  req(ols_model())
  df <- data.frame(fitted=fitted(ols_model()), resid=resid(ols_model()))
  ggplot(df, aes(fitted, resid)) +
    geom_point(alpha=.4, colour="#2ec4b6", size=1.3) +
    geom_smooth(se=FALSE, colour="#e84855", linewidth=.8, method="loess", formula=y~x) +
    geom_hline(yintercept=0, linetype="dashed", colour="#999") +
    labs(x="Fitted GPA", y="Residuals") + theme_gpa()
}, bg="transparent")

# Q-Q: sample residual quantiles vs theoretical normal quantiles
output$ols_qq_plot <- renderPlot({
  req(ols_model())
  ggplot(data.frame(resid=resid(ols_model())), aes(sample=resid)) +
    stat_qq(colour="#2ec4b6", alpha=.5, size=1.2) +
    stat_qq_line(colour="#e84855", linewidth=.8) +
    labs(x="Theoretical Normal Quantiles", y="Sample Residual Quantiles") + theme_gpa()
}, bg="transparent")

# Homoscedasticity: sqrt std residuals vs fitted
output$ols_scale_plot <- renderPlot({
  req(ols_model())
  df <- data.frame(fitted=fitted(ols_model()), sqrt_std=sqrt(abs(rstandard(ols_model()))))
  ggplot(df, aes(fitted, sqrt_std)) +
    geom_point(alpha=.4, colour="#2ec4b6", size=1.3) +
    geom_smooth(se=FALSE, colour="#e84855", linewidth=.8, method="loess", formula=y~x) +
    labs(x="Fitted GPA", y="Sqrt Std Residual") + theme_gpa()
}, bg="transparent")

# ── Decision Tree ──────────────────────────────────────────────────────────────

# Build Tree Model with selected variables
tree_model <- eventReactive(input$run_tree, {
  vars <- isolate(get_inp(input, "tree_main_effects")); req(vars)
  # CART
  rpart(build_formula(vars), data=data,
        control=rpart.control(maxdepth=isolate(input$tree_depth)))
}, ignoreInit=TRUE)

# Decision Tree 5-fold CV
tree_cv <- eventReactive(input$run_tree, {
  vars <- isolate(get_inp(input, "tree_main_effects")); req(vars)
  train(build_formula(vars), data=data, method="rpart", trControl=cv5())
}, ignoreInit=TRUE)

# Tree Metrics Panel
output$tree_metrics_ui <- renderUI({
  req(tree_cv(), tree_model())
  div(class="metric-grid",
    metric_box("CV RMSE", round(tree_cv()$results$RMSE[1], 4)),
    metric_box("# Depth", input$tree_depth),
    metric_box("# Leaves", sum(tree_model()$frame$var == "<leaf>"))
  )
})

# Visualize CART
output$tree_plot <- renderPlot({
  req(tree_model())
  rpart.plot(tree_model(), type=4, extra=101,
             box.palette=list("#0f1b2d","#2ec4b6"),
             branch.lty=3, shadow.col="grey70", main="")
}, bg="transparent")

# Visualize Variable Importance
output$tree_importance <- renderPlot({
  req(tree_model())
  imp <- tree_model()$variable.importance
  if (is.null(imp)) return(
    ggplot() + annotate("text", x=.5, y=.5, label="No importance\n(tree is a stump)",
                        colour="#7a7d8e", size=4) + theme_void())
  df <- data.frame(variable=names(imp), importance=imp) |>
    arrange(importance) |> mutate(variable=factor(variable, levels=variable))
  ggplot(df, aes(importance, variable)) +
    geom_col(fill="#0f1b2d", alpha=.85, width=.6) +
    geom_text(aes(label=round(importance,1)), hjust=-.15, size=3, colour="#2d3142") +
    scale_x_continuous(expand=expansion(mult=c(0,.2))) +
    labs(x="Importance", y=NULL) + theme_gpa()
}, bg="transparent")

# ── Random Forest ──────────────────────────────────────────────────────────────

# Run RF models with selected variables
rf_model <- eventReactive(input$run_rf, {
  vars <- isolate(get_inp(input, "rf_main_effects")); req(vars)
  set.seed(42)
  randomForest(build_formula(vars), data=data,
               ntree=isolate(input$rf_ntree), importance=TRUE)
}, ignoreInit=TRUE)

# RF Metrics Panel
output$rf_metrics_ui <- renderUI({
  req(rf_model()); m <- rf_model()
  div(class="metric-grid",
    metric_box(
      "OOB RMSE",
      round(sqrt(m$mse[length(m$mse)]), 3),
      "Average prediction error using out-of-bag samples."
    ),
    metric_box(
      "OOB R-Squared",
      round(m$rsq[length(m$rsq)], 3),
      "Proportion of GPA variance explained by the forest."
    ),
    metric_box(
      "Trees",
      input$rf_ntree,
      "Total number of decision trees in the forest."
    ),
    metric_box(
      "mtry",
      m$mtry,
      "Number of predictors randomly tested at each split."
    )
  )
})

# RF Variable Importance Plot
output$rf_importance <- renderPlot({
  req(rf_model())
  imp <- importance(rf_model(), type=1)
  df <- data.frame(variable=rownames(imp), inc_mse=imp[,1]) |>
    arrange(inc_mse) |> mutate(variable=factor(variable, levels=variable))
  ggplot(df, aes(inc_mse, variable)) +
    geom_col(fill="#f4a261", alpha=.9, width=.6) +
    geom_text(aes(label=round(inc_mse,2)), hjust=-.15, size=3, colour="#2d3142") +
    scale_x_continuous(expand=expansion(mult=c(0,.2))) +
    labs(x="% Increase in MSE", y=NULL) + theme_gpa()
}, bg="transparent")

# OOB Error Curve
output$rf_error_plot <- renderPlot({
  req(rf_model())
  df <- data.frame(trees=seq_along(rf_model()$mse), mse=rf_model()$mse)
  ggplot(df, aes(trees, sqrt(mse))) +
    geom_line(colour="#f4a261", linewidth=.9) +
    labs(x="Number of Trees", y="OOB RMSE") + theme_gpa()
}, bg="transparent")

# ── Regularized OLS ──────────────────────────────────────────────────────────────

# Prepare inputs for glmnet
glmnet_inputs <- eventReactive(input$run_glmnet, {
  vars <- isolate(get_inp(input, "glmnet_main_effects"))
  alpha <- isolate(input$glmnet_alpha)
  req(vars)
  req(length(vars) >= 2)
  list(X=as.matrix(data[,vars,drop=FALSE]), y=data$GPA, alpha=alpha, vars=vars)
}, ignoreInit=TRUE)

# Run 5-fold CV to find the best lambda
glmnet_cv_fit <- eventReactive(input$run_glmnet, {
  d <- glmnet_inputs(); set.seed(42)
  cv.glmnet(d$X, d$y, alpha=d$alpha, nfolds=5)
}, ignoreInit=TRUE)

# Fit the entire regularization path
glmnet_path_fit <- eventReactive(input$run_glmnet, {
  d <- glmnet_inputs()
  glmnet(d$X, d$y, alpha=d$alpha)
}, ignoreInit=TRUE)

# Reg OLS Metrics Panel
output$glmnet_metrics_ui <- renderUI({
  req(glmnet_cv_fit())
  cv <- glmnet_cv_fit(); lam <- cv$lambda.min
  idx <- which(cv$lambda == lam); rmse <- sqrt(cv$cvm[idx])
  av <- glmnet_inputs()$alpha
  method <- if (av==0) "Ridge" else if (av==1) "Lasso" else paste0("Elastic Net (alpha=",av,")")
  div(class="metric-grid",
    metric_box("Method", method),
    metric_box("Optimal Lambda", round(lam, 4)),
    metric_box("CV RMSE", round(rmse, 4)),
    metric_box("# Non-zero coefs", sum(coef(cv, s="lambda.min")[-1] != 0))
  )
})

# Plot the regularization path (how each coeff change with lambda)
output$glmnet_path <- renderPlot({
  req(glmnet_path_fit(), glmnet_cv_fit())
  m <- glmnet_path_fit(); lam <- glmnet_cv_fit()$lambda.min
  cm <- as.matrix(t(coef(m)[-1,,drop=FALSE]))
  df <- as.data.frame(cm) |>
    mutate(log_lambda=log(m$lambda)) |>
    pivot_longer(-log_lambda, names_to="variable", values_to="coef")
  nv  <- length(unique(df$variable))
  pal <- setNames(colorRampPalette(c("#2ec4b6","#0f1b2d","#f4a261","#6a4c93","#e84855","#2d6a4f","#f4c261"))(nv),
                  unique(df$variable))
  ggplot(df, aes(log_lambda, coef, colour=variable)) + geom_line(linewidth=.8) +
    geom_vline(xintercept=log(lam), linetype="dashed", colour="#aaa", linewidth=.6) +
    scale_colour_manual(values=pal) +
    labs(x="log(lambda)", y="Coefficient", colour=NULL) + theme_gpa() +
    theme(legend.position="bottom", legend.text=element_text(size=7.5))
}, bg="transparent")

# Plot the cross-validation MSE vs lambda
output$glmnet_cv_plot <- renderPlot({
  req(glmnet_cv_fit()); cv <- glmnet_cv_fit()
  df <- data.frame(log_lam=log(cv$lambda), cvm=cv$cvm, cvup=cv$cvup, cvlo=cv$cvlo)
  ggplot(df, aes(log_lam, cvm)) +
    geom_ribbon(aes(ymin=cvlo, ymax=cvup), fill="#6a4c93", alpha=.15) +
    geom_line(colour="#6a4c93", linewidth=.9) +
    geom_vline(xintercept=log(cv$lambda.min), linetype="dashed", colour="#e84855", linewidth=.7) +
    geom_vline(xintercept=log(cv$lambda.1se), linetype="dashed", colour="#999", linewidth=.7) +
    annotate("text", x=log(cv$lambda.min), y=max(df$cvup), label="lam-min", hjust=-.1, size=3, colour="#e84855") +
    annotate("text", x=log(cv$lambda.1se), y=max(df$cvup), label="lam-1se", hjust=-.1, size=3, colour="#666") +
    labs(x="log(lambda)", y="CV MSE") + theme_gpa()
}, bg="transparent")

# glmnet needs at least 2 variables
output$glmnet_warning <- renderUI({
  if (input$run_glmnet == 0) return(NULL)
  vars <- input$glmnet_main_effects
  if (is.null(vars) || length(vars) < 2) {
    div(
      style="
        margin-top:.6rem;
        padding:.55rem .7rem;
        background:#fff4f4;
        border:1px solid #f3b6b6;
        border-radius:6px;
        font-size:.82rem;
        color:#a94442;",
      icon("exclamation-triangle"),
      " Regularized regression requires at least two predictors."
    )
  }
})

# ── KNN ────────────────────────────────────────────────────────────────────────

# Train 5-fold CV KNN
knn_cv <- eventReactive(input$run_knn, {
  vars <- isolate(get_inp(input, "knn_main_effects")); req(vars)
  train(build_formula(vars), data=data, method="knn",
        preProcess=c("center","scale"),
        tuneGrid=data.frame(k=1:isolate(input$knn_k_max)),
        trControl=cv5())
}, ignoreInit=TRUE)

# KNN Metrics Panel
output$knn_metrics_ui <- renderUI({
  req(knn_cv()); best <- knn_cv()$bestTune$k
  rmse <- min(knn_cv()$results$RMSE)
  mae  <- knn_cv()$results$MAE[knn_cv()$results$k == best]
  div(class="metric-grid",
    metric_box("Best k", best),
    metric_box("CV RMSE", round(rmse, 3)),
    metric_box("CV MAE", round(mae, 3))
  )
})

# KNN k Plot
output$knn_k_plot <- renderPlot({
  req(knn_cv()); df <- knn_cv()$results; best <- knn_cv()$bestTune$k
  ggplot(df, aes(k, RMSE)) +
    geom_line(colour="#2d6a4f", linewidth=.9) +
    geom_point(size=2.2, colour="#2d6a4f") +
    geom_point(data=df[df$k==best,], aes(k,RMSE), size=4.5, colour="#e84855", shape=18) +
    annotate("text", x=best, y=df$RMSE[df$k==best],
             label=paste0("best k=",best), vjust=-1.3, size=3.2, colour="#e84855", fontface="bold") +
    labs(x="k (neighbours)", y="CV RMSE") + theme_gpa()
}, bg="transparent")

# ── Model Comparison ───────────────────────────────────────────────────────────

compare_results <- eventReactive(input$compare_models, {
  results <- list()
  ctrl5   <- trainControl(method="cv", number=5, savePredictions="final")

  cv_metrics <- function(preds_df) {
    ps <- preds_df[order(preds_df$rowIndex), ]
    rmse <- sqrt(mean((ps$obs - ps$pred)^2))
    mae <- mean(abs(ps$obs - ps$pred))
    rsq <- cor(ps$obs, ps$pred)^2
    list(RMSE=rmse, MAE=mae, Rsq=rsq, pred=ps$pred, obs=ps$obs)
  }

  # OLS: reuse OLS cv results
  tryCatch({
    r <- ols_cv()
    results[["Linear Regression"]] <- cv_metrics(r$pred)
  }, error=function(e) NULL)

  # Decision Tree: reuse DT cv results
  tryCatch({
    r <- tree_cv()
    results[["Decision Tree"]] <- cv_metrics(r$pred)
  }, error=function(e) NULL)

  # Random Forest: same formula + ntree + mtry
  tryCatch({
    m <- rf_model(); f <- formula(m$terms)
    ntree <- isolate(input$rf_ntree) %||% 200
    set.seed(42)
    r <- train(f, data=data, method="rf", trControl=ctrl5,
               ntree=ntree, tuneGrid=data.frame(mtry=m$mtry))
    results[["Random Forest"]] <- cv_metrics(r$pred)
  }, error=function(e) NULL)

  # Elastic Net: manual 5-fold CV at fitted lambda
  tryCatch({
    cv_fit <- glmnet_cv_fit(); g_in <- glmnet_inputs()
    X <- g_in$X; y <- g_in$y; alpha <- g_in$alpha; lam <- cv_fit$lambda.min
    set.seed(42)
    folds <- sample(rep(1:5, length.out=nrow(X)))
    preds <- numeric(nrow(X)); obs <- numeric(nrow(X))
    for (k in 1:5) {
      ti <- folds != k; vi <- folds == k
      fit_k <- glmnet(X[ti,], y[ti], alpha=alpha, lambda=lam)
      preds[vi] <- as.numeric(predict(fit_k, newx=X[vi,], s=lam))
      obs[vi] <- y[vi]
    }
    method_label <- if (alpha==0) "Ridge" else if (alpha==1) "Lasso" else
                      paste0("Elastic Net (alpha=",alpha,")")
    results[[method_label]] <- list(
      RMSE=sqrt(mean((obs-preds)^2)), MAE=mean(abs(obs-preds)),
      Rsq=cor(obs,preds)^2, pred=preds, obs=obs)
  }, error=function(e) NULL)

  # KNN: reuse KNN cv results
  tryCatch({
    r <- knn_cv()
    results[["KNN"]] <- cv_metrics(r$pred)
  }, error=function(e) NULL)

    results
  }, ignoreInit=TRUE)

# Display which models are ready for comparison
output$cmp_status_ui <- renderUI({
  model_checks <- list(
    list("Linear Regression", !is.null(tryCatch(ols_model(), error=function(e) NULL))),
    list("Decision Tree", !is.null(tryCatch(tree_model(), error=function(e) NULL))),
    list("Random Forest", !is.null(tryCatch(rf_model(), error=function(e) NULL))),
    list("Regularised", !is.null(tryCatch(glmnet_cv_fit(), error=function(e) NULL))),
    list("KNN", !is.null(tryCatch(knn_cv(),  error=function(e) NULL)))
  )
  pills <- lapply(model_checks, function(x) {
    col    <- if (x[[2]]) "#2ec4b6" else "#e4e0d8"
    txtcol <- if (x[[2]]) "#0f1b2d" else "#aaa"
    icon   <- if (x[[2]]) "√" else "ø"
    tags$span(style=paste0(
      "display:inline-flex;align-items:center;gap:.3rem;",
      "background:", col, "22;border:1px solid ", col, ";",
      "border-radius:20px;padding:.2rem .65rem;font-size:.72rem;",
      "font-weight:700;color:", txtcol, ";margin-right:.4rem;margin-bottom:.4rem;"
    ), " ", x[[1]])
  })
  n_fitted <- sum(sapply(model_checks, `[[`, 2))
  div(style="margin-bottom:1rem;",
    div(style="font-size:.75rem;font-weight:700;text-transform:uppercase;letter-spacing:.8px;color:#7a7d8e;margin-bottom:.5rem;",
        paste0("Models ready: ", n_fitted, " / ", length(model_checks))),
    div(pills),
    if (n_fitted == 0)
      div(style="margin-top:.6rem;font-size:.82rem;color:#e84855;",
          "No models fitted yet: go to previous model tabs and fit at least one.")
    else if (n_fitted < length(model_checks))
      div(style="margin-top:.6rem;font-size:.82rem;color:#7a7d8e;",
          "Unfitted models will be skipped in the comparison.")
  )
})

# Model Comparison: Metrics Plots
cmp_plot <- function(metric, ylab, hi=FALSE) {
  req(compare_results()); res <- compare_results()
  if (length(res) == 0) return(NULL)
  df <- data.frame(
    Model = names(res),
    Value = sapply(res, function(r) { v <- r[[metric]]; if(is.null(v)||length(v)==0) NA else v[1] })
  ) |>  filter(!is.na(Value)) |>  arrange(if(hi) desc(Value) else Value) |>  mutate(Model=factor(Model, levels=Model))
  bi <- if (hi) which.max(df$Value) else which.min(df$Value)
  ggplot(df, aes(Model, Value, fill=Model)) +
    geom_col(width=.55, show.legend=FALSE, alpha=ifelse(seq_len(nrow(df))==bi, 1, .65)) +
    geom_text(aes(label=round(Value,4)), vjust=-.4, size=3.1, fontface=ifelse(seq_len(nrow(df))==bi,"bold","plain")) +
    scale_fill_manual(values=setNames(sapply(levels(df$Model), model_color), levels(df$Model))) +
    scale_y_continuous(expand=expansion(mult=c(0,.22))) +
    labs(x=NULL, y=ylab) + theme_gpa() +
    theme(axis.text.x=element_text(angle=30, hjust=1, size=7.5))
}

output$cmp_rmse <- renderPlot({ cmp_plot("RMSE", "CV RMSE") }, bg="transparent")
output$cmp_mae <- renderPlot({ cmp_plot("MAE", "CV MAE")  }, bg="transparent")
output$cmp_r2 <- renderPlot({ cmp_plot("Rsq", "CV R-Squared", hi=TRUE) }, bg="transparent")

# Model Comparison: Actual vs Predicted GPA (Cross-Val)
output$cmp_pred_actual <- renderPlot({
  req(compare_results()); res <- compare_results()
  long <- do.call(rbind, lapply(names(res), function(nm) {
    r <- res[[nm]]; if (is.null(r$pred)) return(NULL)
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
    scale_colour_manual(values=setNames(sapply(unique(long$Model), model_color), unique(long$Model))) +
    coord_fixed(xlim=lims, ylim=lims) +
    labs(x="Actual GPA", y="Predicted GPA") + theme_gpa() +
    theme(strip.text=element_text(face="bold", size=8),
          strip.background=element_rect(fill="#f0ede6", colour=NA))
}, bg="transparent")

# Model Comparison: Metrics Comparison Table
output$cmp_table <- renderTable({
  req(compare_results()); res <- compare_results()
  df <- do.call(rbind, lapply(names(res), function(nm) {
    r <- res[[nm]]
    data.frame(Model=nm, RMSE=round(r$RMSE[1],4), MAE=round(r$MAE[1],4),
               R2=round(ifelse(is.na(r$Rsq[1]),NA,r$Rsq[1]),4), stringsAsFactors=FALSE)
  })) |> arrange(RMSE)
  names(df)[4] <- "R²"; df
}, striped=TRUE, hover=TRUE, bordered=FALSE, spacing="s", width="100%")
