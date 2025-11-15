#' Mean Absolute Scaled Error (MASE)
mase <- function(y_true, y_pred, y_insample = NULL) {
  y_true <- as.numeric(y_true); y_pred <- as.numeric(y_pred)
  if (is.null(y_insample)) y_insample <- y_true
  if (length(y_insample) < 2) {
    denom <- mean(abs(y_true - mean(y_true)))
  } else {
    denom <- mean(abs(diff(y_insample)))
  }
  if (!is.finite(denom) || denom == 0) denom <- 1
  mean(abs(y_true - y_pred)) / denom
}

#' Symmetric MAPE (percent)
smape <- function(y_true, y_pred) {
  y_true <- as.numeric(y_true); y_pred <- as.numeric(y_pred)
  denom <- abs(y_true) + abs(y_pred)
  denom[denom == 0] <- 1
  100 * mean(2 * abs(y_pred - y_true) / denom)
}

#' Pinball loss for a single quantile q in (0,1)
pinball_loss <- function(y_true, y_q, q) {
  y_true <- as.numeric(y_true); y_q <- as.numeric(y_q)
  u <- y_true - y_q
  mean(pmax(q * u, (q - 1) * u))
}
