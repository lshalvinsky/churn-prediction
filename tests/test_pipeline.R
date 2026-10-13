for (file in c("data.R", "segmentation.R", "churn_model.R")) {
  source(file.path("R", file))
}

customers <- generate_customers(n = 1200, seed = 7)
stopifnot(nrow(customers) == 1200L)
stopifnot(length(unique(customers$customer_id)) == 1200L)
stopifnot(all(customers$churned_90d %in% c(0, 1)))
stopifnot(all(customers$responded_campaign %in% c(0, 1)))
stopifnot(length(unique(customers$true_persona)) == length(PERSONAS))

rfm <- rfm_scores(customers)
stopifnot(all(vapply(rfm[c("R", "F", "M")], function(x) {
  all(x >= 1 & x <= 5)
}, logical(1))))

segmented <- fit_segments(customers, k = 5, seed = 9)
stopifnot(length(unique(segmented$segment)) == 5L)
stopifnot(!any(c("churned_90d", "true_persona") %in% SEGMENT_FEATURES))
profile <- segment_profile(segmented)
stopifnot(nrow(profile) == 5L)
stopifnot(abs(sum(profile$share) - 1) < 1e-10)

stopifnot(abs(roc_auc(c(1, 0, 1, 0), c(0.9, 0.8, 0.2, 0.1)) - 0.75) < 1e-10)
top <- top_fraction_metrics(c(1, 1, 0, 0), c(0.9, 0.8, 0.2, 0.1), 0.5)
stopifnot(top$recall_at_fraction == 1)
deciles <- churn_deciles(c(1, 0, 1, 0), c(0.9, 0.8, 0.2, 0.1), 2)
stopifnot(identical(deciles$churners, c(1, 1)))
classification <- classification_metrics(c(1, 0, 1, 0), c(0.9, 0.8, 0.2, 0.1))
stopifnot(classification$accuracy == 0.5)

if (requireNamespace("ranger", quietly = TRUE)) {
  model <- train_churn_models(segmented)
  probabilities <- predict_churn(model, segmented)
  stopifnot(length(probabilities) == nrow(segmented))
  stopifnot(all(is.finite(probabilities) & probabilities >= 0 & probabilities <= 1))
}

message("All churn-prediction and segmentation checks passed.")
