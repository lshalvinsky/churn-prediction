PERSONAS <- list(
  "Loyal High-Value" = list(
    share = 0.18, age = c(48, 10), income = 105, tenure = 54, recency = 12,
    frequency = 20, aov = 125, discount = c(2, 10), web = 8, app = 7,
    email = c(8, 3), social = 2.5, support = 0.7, nps = 8.5,
    channel = c(0.25, 0.50, 0.25)
  ),
  "Promotion Seekers" = list(
    share = 0.24, age = c(39, 12), income = 55, tenure = 32, recency = 25,
    frequency = 16, aov = 42, discount = c(9, 3), web = 11, app = 4,
    email = c(6, 4), social = 3, support = 1.2, nps = 6.5,
    channel = c(0.40, 0.48, 0.12)
  ),
  "Digital Engaged" = list(
    share = 0.22, age = c(28, 6), income = 58, tenure = 18, recency = 20,
    frequency = 12, aov = 58, discount = c(4, 6), web = 6, app = 17,
    email = c(4, 6), social = 13, support = 0.8, nps = 7.5,
    channel = c(0.05, 0.28, 0.67)
  ),
  "At-Risk Dormant" = list(
    share = 0.21, age = c(45, 13), income = 62, tenure = 42, recency = 175,
    frequency = 3, aov = 62, discount = c(3, 7), web = 1.5, app = 0.7,
    email = c(1, 9), social = 0.6, support = 2.1, nps = 4.2,
    channel = c(0.55, 0.35, 0.10)
  ),
  "New Growth" = list(
    share = 0.15, age = c(32, 9), income = 68, tenure = 5, recency = 18,
    frequency = 4, aov = 72, discount = c(5, 5), web = 7, app = 8,
    email = c(5, 5), social = 6, support = 0.9, nps = 7.0,
    channel = c(0.15, 0.40, 0.45)
  )
)

draw_counts <- function(mean, n, shape = 1.5) {
  stats::rpois(n, mean * stats::rgamma(n, shape = shape, scale = 1 / shape))
}

draw_beta <- function(ab, n, concentration = 0.5) {
  stats::rbeta(n, ab[[1L]] * concentration, ab[[2L]] * concentration)
}

generate_customers <- function(n = 6000L, seed = 42L) {
  stopifnot(length(n) == 1L, is.finite(n), n >= 1L, n == as.integer(n))
  set.seed(seed)
  persona_names <- names(PERSONAS)
  shares <- vapply(PERSONAS, `[[`, numeric(1), "share")
  counts <- as.vector(stats::rmultinom(1, n, shares))
  active <- counts > 0L

  blocks <- Map(function(name, count) {
    p <- PERSONAS[[name]]
    age <- round(pmax(18, pmin(85, stats::rnorm(count, p$age[[1]], p$age[[2]]))))
    tenure <- round(pmax(1, pmin(180, stats::rgamma(count, shape = 4, scale = p$tenure / 4))))
    recency <- round(pmax(0, pmin(730, stats::rexp(count, rate = 1 / p$recency))))
    frequency <- 1 + draw_counts(max(p$frequency - 1, 0.1), count)
    aov <- stats::rlnorm(count, log(p$aov), 0.5)
    monetary <- round(frequency * aov * stats::rlnorm(count, 0, 0.1), 2)
    email_open <- draw_beta(p$email, count)
    data <- data.frame(
      age = as.integer(age),
      gender = sample(c("F", "M", "Other"), count, replace = TRUE,
                      prob = c(0.52, 0.46, 0.02)),
      region = sample(c("Northeast", "Southeast", "Midwest", "Southwest", "West"),
                      count, replace = TRUE),
      income_k = round(stats::rlnorm(count, log(p$income), 0.4), 1),
      tenure_months = as.integer(tenure),
      recency_days = as.integer(recency),
      frequency_12m = frequency,
      monetary_12m = monetary,
      avg_order_value = round(monetary / frequency, 2),
      discount_usage_rate = round(draw_beta(p$discount, count), 3),
      web_sessions_month = draw_counts(p$web, count),
      app_sessions_month = draw_counts(p$app, count),
      email_open_rate = round(email_open, 3),
      email_click_rate = round(email_open * stats::rbeta(count, 2, 6), 3),
      social_engagement = draw_counts(p$social, count),
      support_tickets_12m = stats::rpois(count, p$support),
      nps_score = as.integer(round(pmax(0, pmin(10, stats::rnorm(count, p$nps, 1.8))))),
      preferred_channel = sample(c("store", "web", "app"), count, replace = TRUE,
                                 prob = p$channel),
      true_persona = name,
      stringsAsFactors = FALSE
    )
    data
  }, persona_names[active], counts[active])

  customers <- do.call(rbind, blocks)
  customers <- customers[sample.int(nrow(customers)), , drop = FALSE]
  rownames(customers) <- NULL
  customers$customer_id <- sprintf("C%06d", seq_len(nrow(customers)))
  customers <- customers[c("customer_id", setdiff(names(customers), "customer_id"))]

  persona_response <- c(
    "Loyal High-Value" = -0.45,
    "Promotion Seekers" = 0.15,
    "Digital Engaged" = -0.65,
    "At-Risk Dormant" = -1.45,
    "New Growth" = -0.20
  )
  response_logit <- unname(persona_response[customers$true_persona]) +
    2.0 * (customers$email_open_rate - 0.4) +
    1.1 * (customers$discount_usage_rate - 0.4) -
    0.003 * (customers$recency_days - 45) +
    0.12 * (customers$nps_score - 6) + stats::rnorm(nrow(customers), 0, 0.5)
  customers$responded_campaign <- stats::rbinom(
    nrow(customers), 1, stats::plogis(response_logit)
  )

  persona_churn <- c(
    "Loyal High-Value" = -1.25,
    "Promotion Seekers" = -0.35,
    "Digital Engaged" = -0.8,
    "At-Risk Dormant" = 0.75,
    "New Growth" = -0.65
  )
  churn_logit <- unname(persona_churn[customers$true_persona]) +
    0.010 * (customers$recency_days - 45) -
    0.10 * (customers$frequency_12m - 8) -
    0.20 * (customers$nps_score - 6) +
    0.20 * customers$support_tickets_12m -
    0.012 * (customers$tenure_months - 24) -
    0.50 * customers$email_open_rate +
    stats::rnorm(nrow(customers), 0, 0.45)
  customers$churned_90d <- stats::rbinom(nrow(customers), 1, stats::plogis(churn_logit))
  customers
}

save_customers <- function(customers, path = file.path("data", "raw", "customers.csv")) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(customers, path, row.names = FALSE)
  invisible(path)
}

load_customers <- function(path = file.path("data", "raw", "customers.csv")) {
  if (!file.exists(path)) save_customers(generate_customers(), path)
  utils::read.csv(path, stringsAsFactors = FALSE)
}
