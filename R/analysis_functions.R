# R/analysis_functions.R

# Core analysis functions for internal PoS Shiny app
# Statistical approach:
# - Historical studies provide OS/PFS treatment effects and within-study covariance
# - Fit bivariate meta-analysis using mvmeta
# - Estimate between-study covariance (Psi) using REML or method of moments
# - Use current-study PFS plus estimated covariance structure to derive
#   predictive distribution for current-study OS
# - Compute PoS = P(current OS log(HR) < log(success_hr_threshold))

make_cov <- function(sd1, sd2, rho) {
  if (!is.numeric(sd1) || length(sd1) != 1 || is.na(sd1) || sd1 <= 0) {
    stop("sd1 must be a single positive numeric value.")
  }
  if (!is.numeric(sd2) || length(sd2) != 1 || is.na(sd2) || sd2 <= 0) {
    stop("sd2 must be a single positive numeric value.")
  }
  if (!is.numeric(rho) || length(rho) != 1 || is.na(rho) || rho < -1 || rho > 1) {
    stop("rho must be a single numeric value between -1 and 1.")
  }
  
  matrix(
    c(
      sd1^2, rho * sd1 * sd2,
      rho * sd1 * sd2, sd2^2
    ),
    nrow = 2,
    byrow = TRUE
  )
}

validate_hist_data <- function(hist_dat) {
  required_cols <- c("logHR_OS", "logHR_PFS", "SE_OS", "SE_PFS", "R_WITHIN")
  
  if (!is.data.frame(hist_dat)) {
    stop("Historical data must be a data.frame.")
  }
  
  missing_cols <- setdiff(required_cols, names(hist_dat))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Historical data is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  for (col in required_cols) {
    if (!is.numeric(hist_dat[[col]])) {
      stop(sprintf("Column '%s' must be numeric.", col))
    }
  }
  
  if (nrow(hist_dat) < 3) {
    stop("Historical data must contain at least 3 studies.")
  }
  
  if (any(is.na(hist_dat[, required_cols]))) {
    stop("Historical data contains missing values in required columns.")
  }
  
  if (any(hist_dat$SE_OS <= 0)) {
    stop("All SE_OS values must be > 0.")
  }
  
  if (any(hist_dat$SE_PFS <= 0)) {
    stop("All SE_PFS values must be > 0.")
  }
  
  if (any(hist_dat$R_WITHIN < -1 | hist_dat$R_WITHIN > 1)) {
    stop("All R_WITHIN values must be between -1 and 1.")
  }
  
  invisible(TRUE)
}

is_near_zero_matrix <- function(mat, abs_tol = 1e-4, fro_tol = 1e-4) {
  if (is.null(mat)) return(TRUE)
  vals <- as.numeric(mat)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(TRUE)
  
  max_abs <- max(abs(vals))
  frob <- sqrt(sum(vals^2))
  
  (max_abs < abs_tol) || (frob < fro_tol)
}

compute_pos <- function(
    hist_dat,
    current_pfs_loghr,
    current_os_se,
    current_pfs_se,
    current_rho,
    method = c("reml", "mm"),
    success_hr_threshold = 1.0
) {
  method <- match.arg(method)
  
  validate_hist_data(hist_dat)
  
  if (!is.numeric(current_pfs_loghr) || length(current_pfs_loghr) != 1 || is.na(current_pfs_loghr)) {
    stop("current_pfs_loghr must be a single numeric value.")
  }
  if (!is.numeric(current_os_se) || length(current_os_se) != 1 || is.na(current_os_se) || current_os_se <= 0) {
    stop("current_os_se must be a single positive numeric value.")
  }
  if (!is.numeric(current_pfs_se) || length(current_pfs_se) != 1 || is.na(current_pfs_se) || current_pfs_se <= 0) {
    stop("current_pfs_se must be a single positive numeric value.")
  }
  if (!is.numeric(current_rho) || length(current_rho) != 1 || is.na(current_rho) ||
      current_rho < -1 || current_rho > 1) {
    stop("current_rho must be a single numeric value between -1 and 1.")
  }
  if (!is.numeric(success_hr_threshold) || length(success_hr_threshold) != 1 ||
      is.na(success_hr_threshold) || success_hr_threshold <= 0) {
    stop("success_hr_threshold must be a single positive numeric value.")
  }
  
  warning_messages <- character(0)
  
  y_hist <- as.matrix(hist_dat[, c("logHR_OS", "logHR_PFS")])
  colnames(y_hist) <- c("logHR_OS", "logHR_PFS")
  
  S_hist <- lapply(seq_len(nrow(hist_dat)), function(i) {
    make_cov(
      sd1 = hist_dat$SE_OS[i],
      sd2 = hist_dat$SE_PFS[i],
      rho = hist_dat$R_WITHIN[i]
    )
  })
  
  fit <- tryCatch(
    mvmeta::mvmeta(y_hist, S = S_hist, method = method),
    error = function(e) {
      stop(paste("mvmeta model fitting failed:", e$message))
    }
  )
  
  eta_hat <- tryCatch(
    as.numeric(stats::coef(fit)),
    error = function(e) stop("Failed to extract fixed-effect estimates from mvmeta fit.")
  )
  
  V_eta <- tryCatch(
    stats::vcov(fit),
    error = function(e) stop("Failed to extract covariance matrix of estimated historical mean effects.")
  )
  
  Sigma0_hat <- fit$Psi
  if (is.null(Sigma0_hat)) {
    stop("Failed to extract estimated between-study covariance matrix (Psi).")
  }
  
  # More robust warning for near-zero heterogeneity
  if (is_near_zero_matrix(Sigma0_hat, abs_tol = 5e-4, fro_tol = 8e-4)) {
    warning_messages <- c(
      warning_messages,
      "Estimated between-study covariance matrix is approximately zero or very close to the boundary. Predictive uncertainty may be underestimated, and PoS may appear overly stable."
    )
  }
  
  if (nrow(hist_dat) < 5) {
    warning_messages <- c(
      warning_messages,
      "Number of historical studies is small. PoS results may be unstable and should be interpreted with caution."
    )
  }
  
  S_current <- make_cov(
    sd1 = current_os_se,
    sd2 = current_pfs_se,
    rho = current_rho
  )
  
  Sigma_cur <- Sigma0_hat + S_current
  
  if (!is.finite(Sigma_cur[2, 2]) || Sigma_cur[2, 2] <= 0) {
    stop("Computed predictive variance for current-study PFS is non-positive. Please check inputs.")
  }
  
  h <- Sigma_cur[1, 2] / Sigma_cur[2, 2]
  v_cond <- Sigma_cur[1, 1] - (Sigma_cur[1, 2]^2 / Sigma_cur[2, 2])
  
  if (!is.finite(v_cond) || v_cond <= 0) {
    stop("Computed conditional predictive variance for current-study OS is non-positive or invalid.")
  }
  
  a <- c(1, -h)
  mean_pred <- eta_hat[1] + h * (current_pfs_loghr - eta_hat[2])
  var_pred <- as.numeric(t(a) %*% V_eta %*% a + v_cond)
  
  if (!is.finite(var_pred) || var_pred <= 0) {
    stop("Final predictive variance is non-positive or invalid.")
  }
  
  sd_pred <- sqrt(var_pred)
  threshold_loghr <- log(success_hr_threshold)
  
  pos <- stats::pnorm(threshold_loghr, mean = mean_pred, sd = sd_pred)
  
  if (!is.finite(pos)) {
    stop("Computed PoS is invalid.")
  }
  
  if (pos < 0.05 || pos > 0.95) {
    warning_messages <- c(
      warning_messages,
      "PoS is very close to 0 or 1. Check whether this is driven by strong data support or by restrictive modeling assumptions."
    )
  }
  
  list(
    method = method,
    pos = pos,
    mean_pred = mean_pred,
    sd_pred = sd_pred,
    threshold_loghr = threshold_loghr,
    eta_hat = eta_hat,
    Sigma0_hat = Sigma0_hat,
    warning_messages = unique(warning_messages),
    fit = fit
  )
}

compute_pos_both_methods <- function(
    hist_dat,
    current_pfs_loghr,
    current_os_se,
    current_pfs_se,
    current_rho,
    success_hr_threshold = 1.0
) {
  res_reml <- tryCatch(
    compute_pos(
      hist_dat = hist_dat,
      current_pfs_loghr = current_pfs_loghr,
      current_os_se = current_os_se,
      current_pfs_se = current_pfs_se,
      current_rho = current_rho,
      method = "reml",
      success_hr_threshold = success_hr_threshold
    ),
    error = function(e) structure(list(error_message = e$message), class = "app_error")
  )
  
  res_mm <- tryCatch(
    compute_pos(
      hist_dat = hist_dat,
      current_pfs_loghr = current_pfs_loghr,
      current_os_se = current_os_se,
      current_pfs_se = current_pfs_se,
      current_rho = current_rho,
      method = "mm",
      success_hr_threshold = success_hr_threshold
    ),
    error = function(e) structure(list(error_message = e$message), class = "app_error")
  )
  
  list(reml = res_reml, mm = res_mm)
}