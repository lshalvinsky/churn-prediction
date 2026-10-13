stratified_split <- function(y, test_fraction = 0.25, seed = 42L) {
  if (!is.numeric(test_fraction) || length(test_fraction) != 1L ||
      test_fraction <= 0 || test_fraction >= 1) {
    stop("test_fraction must be between zero and one.")
  }
  set.seed(seed)
  groups <- split(seq_along(y), y)
  test <- unlist(lapply(groups, function(idx) {
    n_test <- max(1L, floor(length(idx) * test_fraction))
    if (length(idx) < 2L) stop("Each outcome class needs at least two observations.")
    sample(idx, size = min(n_test, length(idx) - 1L))
  }), use.names = FALSE)
  list(train = setdiff(seq_along(y), test), test = test)
}

roc_auc <- function(truth, scores) {
  if (length(truth) != length(scores) || anyNA(truth) || anyNA(scores) ||
      any(!is.finite(scores))) {
    stop("truth and scores must be equal-length, complete vectors.")
  }
  positives <- sum(truth == 1)
  negatives <- sum(truth == 0)
  if (positives == 0 || negatives == 0) stop("AUC requires both outcome classes.")
  ranks <- rank(scores, ties.method = "average")
  (sum(ranks[truth == 1]) - positives * (positives + 1) / 2) /
    (positives * negatives)
}

top_fraction_metrics <- function(truth, scores, fraction = 0.2) {
  if (length(truth) != length(scores) || length(truth) == 0L ||
      fraction <= 0 || fraction > 1) {
    stop("Invalid inputs or targeting fraction.")
  }
  selected <- order(scores, decreasing = TRUE)[
    seq_len(max(1L, ceiling(length(scores) * fraction)))
  ]
  total_events <- sum(truth == 1)
  events_targeted <- sum(truth[selected] == 1)
  data.frame(
    targeted_fraction = length(selected) / length(truth),
    customers_targeted = length(selected),
    churners_targeted = events_targeted,
    recall_at_fraction = if (total_events == 0) 0 else events_targeted / total_events,
    churn_rate_in_target_group = mean(truth[selected])
  )
}

prepare_churn_matrix <- function(data, factor_levels = NULL) {
  features <- data[c(CHURN_NUMERIC_FEATURES, CHURN_CATEGORICAL_FEATURES)]
  if (is.null(factor_levels)) {
    factor_levels <- lapply(features[CHURN_CATEGORICAL_FEATURES], function(x) {
      sort(unique(as.character(x)))
    })
  }
  for (column in names(factor_levels)) {
    values <- as.character(features[[column]])
    unknown <- setdiff(unique(values), factor_levels[[column]])
    if (length(unknown)) {
      stop("Unseen values in ", column, ": ", paste(unknown, collapse = ", "))
    }
    features[[column]] <- factor(values, levels = factor_levels[[column]])
  }
  matrix <- stats::model.matrix(~ ., data = features)
  list(matrix = matrix, factor_levels = factor_levels)
}

train_churn_models <- function(data, target = "churned_90d", test_fraction = 0.25,
                               seed = 42L) {
  if (!target %in% names(data)) stop("Target column not found: ", target)
  if (!all(data[[target]] %in% c(0, 1))) stop("Churn target must contain only 0 and 1.")
  if (!"segment" %in% names(data)) stop("Fit behavioral segments before churn models.")
  predictors <- data
  predictors$segment <- factor(data$segment)
  segment_levels <- levels(predictors$segment)
  encoded <- prepare_churn_matrix(predictors)
  segment_columns <- stats::model.matrix(~ segment - 1, data = predictors)
  x <- cbind(encoded$matrix, segment_columns)
  y <- as.integer(data[[target]])
  split <- stratified_split(y, test_fraction, seed)
  x_train <- x[split$train, , drop = FALSE]
  x_test <- x[split$test, , drop = FALSE]

  varying <- apply(x_train, 2, function(column) length(unique(column)) > 1L)
  varying["(Intercept)"] <- TRUE
  x_train <- x_train[, varying, drop = FALSE]
  x_test <- x_test[, colnames(x_train), drop = FALSE]
  centers <- colMeans(x_train)
  scales <- apply(x_train, 2, stats::sd)
  scales[!is.finite(scales) | scales == 0] <- 1
  if ("(Intercept)" %in% names(centers)) {
    centers[["(Intercept)"]] <- 0
    scales[["(Intercept)"]] <- 1
  }
  x_train_scaled <- scale(x_train, center = centers, scale = scales)
  x_test_scaled <- scale(x_test, center = centers, scale = scales)

  logistic <- stats::glm.fit(
    x = x_train_scaled, y = y[split$train], family = stats::binomial()
  )
  coefficients <- logistic$coefficients
  coefficients[is.na(coefficients)] <- 0
  logistic_scores <- stats::plogis(drop(x_test_scaled %*% coefficients))

  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("Install the ranger package to fit the random-forest churn model.")
  }
  forest_data <- data[c(CHURN_NUMERIC_FEATURES, CHURN_CATEGORICAL_FEATURES)]
  forest_data$segment <- factor(data$segment, levels = segment_levels)
  forest <- ranger::ranger(
    x = forest_data[split$train, , drop = FALSE],
    y = factor(y[split$train], levels = c(0, 1)),
    probability = TRUE, num.trees = 400, min.node.size = 10,
    importance = "permutation", seed = seed
  )
  forest_scores <- predict(
    forest, data = forest_data[split$test, , drop = FALSE]
  )$predictions[, "1"]

  truth <- y[split$test]
  list(
    models = list(
      "Logistic regression" = list(
        model = logistic, scores = logistic_scores,
        auc = roc_auc(truth, logistic_scores)
      ),
      "Random forest" = list(
        model = forest, scores = forest_scores,
        auc = roc_auc(truth, forest_scores),
        importance = sort(forest$variable.importance, decreasing = TRUE)
      )
    ),
    truth = truth,
    test_indices = split$test,
    factor_levels = encoded$factor_levels,
    segment_levels = segment_levels,
    columns = colnames(x_train),
    centers = centers,
    scales = scales
  )
}

predict_churn <- function(fit, data, model = "Random forest") {
  if (!"segment" %in% names(data)) stop("Prediction data must include segment.")
  if (!model %in% names(fit$models)) stop("Unknown model: ", model)
  if (model == "Random forest") {
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop("Install the ranger package to predict with the random forest.")
    }
    forest_data <- data[c(CHURN_NUMERIC_FEATURES, CHURN_CATEGORICAL_FEATURES)]
    forest_data$segment <- factor(data$segment, levels = fit$segment_levels)
    if (anyNA(forest_data$segment)) stop("Prediction data contains an unseen segment.")
    return(as.numeric(predict(
      fit$models[["Random forest"]]$model, data = forest_data
    )$predictions[, "1"]))
  }
  encoded <- prepare_churn_matrix(data, fit$factor_levels)$matrix
  segment <- factor(data$segment, levels = fit$segment_levels)
  if (anyNA(segment)) stop("Prediction data contains an unseen segment.")
  x <- cbind(encoded, stats::model.matrix(~ segment - 1, data = data.frame(segment)))
  missing <- setdiff(fit$columns, colnames(x))
  if (length(missing)) {
    stop("Prediction data is missing model columns: ", paste(missing, collapse = ", "))
  }
  x <- x[, fit$columns, drop = FALSE]
  x <- scale(x, center = fit$centers, scale = fit$scales)
  coefficients <- fit$models[["Logistic regression"]]$model$coefficients
  coefficients[is.na(coefficients)] <- 0
  stats::plogis(drop(x %*% coefficients))
}

churn_deciles <- function(truth, scores, n_bins = 10L) {
  if (length(truth) != length(scores) || length(truth) == 0L || n_bins < 1L) {
    stop("Invalid inputs for churn deciles.")
  }
  order_idx <- order(scores, decreasing = TRUE)
  decile <- floor((seq_along(order_idx) - 1) * n_bins / length(order_idx)) + 1L
  ranked <- data.frame(
    truth = truth[order_idx], score = scores[order_idx], decile = decile
  )
  groups <- split(ranked, ranked$decile)
  out <- do.call(rbind, lapply(groups, function(group) {
    data.frame(
      customers = nrow(group),
      churners = sum(group$truth),
      mean_score = mean(group$score)
    )
  }))
  out$decile <- as.integer(rownames(out))
  out$churn_rate <- out$churners / out$customers
  out$lift = out$churn_rate / mean(truth)
  out$cumulative_customers <- cumsum(out$customers) / length(truth)
  total <- sum(truth)
  out$cumulative_churners <- if (total == 0) 0 else cumsum(out$churners) / total
  rownames(out) <- NULL
  out
}

classification_metrics <- function(truth, scores, threshold = 0.5) {
  if (length(truth) != length(scores) || length(truth) == 0L ||
      any(!truth %in% c(0, 1)) || any(!is.finite(scores)) ||
      threshold < 0 || threshold > 1) {
    stop("Invalid truth, scores, or classification threshold.")
  }
  predicted <- as.integer(scores >= threshold)
  tp <- sum(truth == 1 & predicted == 1)
  tn <- sum(truth == 0 & predicted == 0)
  fp <- sum(truth == 0 & predicted == 1)
  fn <- sum(truth == 1 & predicted == 0)
  precision <- if (tp + fp == 0) 0 else tp / (tp + fp)
  recall <- if (tp + fn == 0) 0 else tp / (tp + fn)
  data.frame(
    threshold = threshold,
    accuracy = (tp + tn) / length(truth),
    precision = precision,
    recall = recall,
    f1 = if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
  )
}
