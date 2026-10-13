SEGMENT_FEATURES <- c(
  "recency_days", "frequency_12m", "avg_order_value", "discount_usage_rate",
  "web_sessions_month", "app_sessions_month", "email_open_rate", "social_engagement",
  "support_tickets_12m", "nps_score"
)

CHURN_NUMERIC_FEATURES <- c(
  "age", "income_k", "tenure_months", "recency_days", "frequency_12m",
  "monetary_12m", "avg_order_value", "discount_usage_rate",
  "web_sessions_month", "app_sessions_month", "email_open_rate",
  "email_click_rate", "social_engagement", "support_tickets_12m", "nps_score"
)

CHURN_CATEGORICAL_FEATURES <- c("gender", "region", "preferred_channel")

prepare_segments <- function(data, features = SEGMENT_FEATURES) {
  log_features <- intersect(
    c("recency_days", "frequency_12m", "avg_order_value", "web_sessions_month",
      "app_sessions_month", "social_engagement", "support_tickets_12m"),
    features
  )
  transformed <- data[features]
  transformed[log_features] <- lapply(transformed[log_features], log1p)
  center <- vapply(transformed, mean, numeric(1))
  spread <- vapply(transformed, stats::sd, numeric(1))
  if (any(!is.finite(spread) | spread == 0)) {
    stop("Segmentation features must have finite, non-constant values.")
  }
  scale(transformed, center = center, scale = spread)
}

fit_segments <- function(data, k = 5L, seed = 42L) {
  x <- prepare_segments(data)
  set.seed(seed)
  fit <- stats::kmeans(x, centers = k, nstart = 20, iter.max = 100)
  data$segment_id <- as.integer(fit$cluster)
  data$segment <- paste("Segment", data$segment_id)
  data
}

segment_profile <- function(data) {
  required <- c("segment", "churned_90d", "monetary_12m", "responded_campaign")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Missing profile columns: ", paste(missing, collapse = ", "))
  groups <- split(data, data$segment)
  profiles <- lapply(groups, function(group) {
    data.frame(
      customers = nrow(group),
      share = nrow(group) / nrow(data),
      avg_recency_days = mean(group$recency_days),
      avg_frequency = mean(group$frequency_12m),
      avg_revenue = mean(group$monetary_12m),
      campaign_response_rate = mean(group$responded_campaign),
      churn_rate = mean(group$churned_90d)
    )
  })
  out <- do.call(rbind, profiles)
  out$segment <- rownames(out)
  rownames(out) <- NULL
  out[c("segment", setdiff(names(out), "segment"))]
}

rfm_scores <- function(data) {
  quintile <- function(x) {
    pmin(5L, as.integer(ceiling(rank(x, ties.method = "first") * 5 / length(x))))
  }
  r <- 6L - quintile(data$recency_days)
  f <- quintile(data$frequency_12m)
  m <- quintile(data$monetary_12m)
  data.frame(R = r, F = f, M = m, rfm_score = r + f + m)
}
