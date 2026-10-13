for (file in c("data.R", "segmentation.R", "churn_model.R")) {
  source(file.path("R", file))
}

customers <- load_customers()
customers <- fit_segments(customers, k = 5)
customers <- cbind(customers, rfm_scores(customers))
profiles <- segment_profile(customers)
model_fit <- train_churn_models(customers)
forest <- model_fit$models[["Random forest"]]
customers$churn_probability <- predict_churn(model_fit, customers)
deciles <- churn_deciles(model_fit$truth, forest$scores)

dir.create(file.path("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create("reports", recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  customers[c("customer_id", "segment", "R", "F", "M", "churn_probability")],
  file.path("data", "processed", "customer_churn_scores.csv"),
  row.names = FALSE
)
utils::write.csv(profiles, file.path("reports", "segment_profiles.csv"), row.names = FALSE)
utils::write.csv(deciles, file.path("reports", "churn_deciles.csv"), row.names = FALSE)
metrics <- data.frame(
  model = names(model_fit$models),
  auc = vapply(model_fit$models, `[[`, numeric(1), "auc")
)
classification <- do.call(rbind, lapply(names(model_fit$models), function(name) {
  cbind(
    model = name,
    classification_metrics(model_fit$truth, model_fit$models[[name]]$scores)
  )
}))
utils::write.csv(metrics, file.path("reports", "model_metrics.csv"), row.names = FALSE)
utils::write.csv(
  classification, file.path("reports", "classification_metrics.csv"), row.names = FALSE
)
print(metrics)
print(classification)
print(profiles)
message("Analysis outputs written to data/processed/ and reports/.")
