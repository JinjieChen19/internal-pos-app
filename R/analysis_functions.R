# R/analysis_functions.R
#
# Core analysis functions for internal PoS Shiny app
#
# This version follows the Daniels & Hughes (1997) surrogate-endpoint structure
# more closely than the earlier conditional-update implementation:
#
# Historical level
#   Y_i = (Y_OS,i, Y_PFS,i)' | (T_i, S_i)' ~ N((T_i, S_i)', Sigma_i)
#   T_i | S_i ~ N(alpha + beta * S_i, tau2)
#   S_i ~ N(eta, psi2)
#
# Current-study prediction target
#   The target is always the latent final OS effect T_*.
#   PFS-only and PFS+OS-interim modes therefore predict the same target.
#
# Current-study observation model
#   Y_PFS,* | S_* ~ N(S_*, se_pfs^2)
#   Y_OS,int,* | T_* ~ N(T_*, se_os_int^2)  [optional extension]
#
# REML and method-of-moments are both retained through mvmeta::mvmeta()
# for the historical bivariate random-effects fit. The fitted Psi is then
# re-parameterized into the Daniels-Hughes regression form.
#
# V_eta is propagated only as an additional variance term in the final
# predictive variance via a delta-method correction on the posterior mean.

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

fit_bivariate_meta <- function(hist_dat, method = c("reml", "mm")) {
  method <- match.arg(method)
  
  validate_hist_data(hist_dat)
  
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
  
  warning_messages <- character(0)
  
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
  
  list(
    fit = fit,
    eta_hat = eta_hat,
    V_eta = V_eta,
    Sigma0_hat = Sigma0_hat,
    warning_messages = unique(warning_messages)
  )
}

fit_dh_params <- function(eta_hat, Sigma0_hat, eps = 1e-10) {
  if (!is.numeric(eta_hat) || length(eta_hat) != 2 || any(!is.finite(eta_hat))) {
    stop("eta_hat must be a length-2 finite numeric vector.")
  }
  if (!is.matrix(Sigma0_hat) || !all(dim(Sigma0_hat) == c(2, 2)) || any(!is.finite(Sigma0_hat))) {
    stop("Sigma0_hat must be a 2x2 finite numeric matrix.")
  }
  
  # Historical ordering from mvmeta fit: (OS, PFS)
  mu_t <- as.numeric(eta_hat[1])
  mu_s <- as.numeric(eta_hat[2])
  
  var_t <- as.numeric(Sigma0_hat[1, 1])
  cov_ts <- as.numeric(Sigma0_hat[1, 2])
  var_s <- as.numeric(Sigma0_hat[2, 2])
  
  if (!is.finite(var_s) || var_s <= eps) {
    warning("Estimated between-study variance for PFS is near zero; applying numerical lower bound.")
    var_s <- eps
  }
  
  beta <- cov_ts / var_s
  alpha <- mu_t - beta * mu_s
  tau2 <- var_t - (cov_ts^2) / var_s
  tau2 <- max(tau2, eps)
  
  # Prior for current latent (S, T) implied by D&H model.
  mu_z <- c(mu_s, mu_t)
  Sigma_z <- matrix(
    c(
      var_s, cov_ts,
      cov_ts, var_t
    ),
    nrow = 2,
    byrow = TRUE
  )
  
  list(
    alpha = alpha,
    beta = beta,
    tau2 = tau2,
    eta = mu_s,
    psi2 = var_s,
    mu_z = mu_z,
    Sigma_z = Sigma_z
  )
}

predict_os_dh <- function(
    eta_hat,
    Sigma0_hat,
    V_eta,
    pfs_loghr,
    se_pfs,
    os_interim_loghr = NA_real_,
    se_os_interim = NA_real_,
    rho_pfs_os_interim = 0,
    huge_var = 1e10
) {
  if (!is.numeric(eta_hat) || length(eta_hat) != 2 || any(!is.finite(eta_hat))) {
    stop("eta_hat must be a length-2 finite numeric vector.")
  }
  if (!is.matrix(Sigma0_hat) || !all(dim(Sigma0_hat) == c(2, 2)) || any(!is.finite(Sigma0_hat))) {
    stop("Sigma0_hat must be a 2x2 finite numeric matrix.")
  }
  if (!is.matrix(V_eta) || !all(dim(V_eta) == c(2, 2)) || any(!is.finite(V_eta))) {
    stop("V_eta must be a 2x2 finite numeric matrix.")
  }
  if (!is.numeric(pfs_loghr) || length(pfs_loghr) != 1 || !is.finite(pfs_loghr)) {
    stop("pfs_loghr must be a single finite numeric value.")
  }
  if (!is.numeric(se_pfs) || length(se_pfs) != 1 || !is.finite(se_pfs) || se_pfs <= 0) {
    stop("se_pfs must be a single positive numeric value.")
  }
  if (!is.numeric(rho_pfs_os_interim) || length(rho_pfs_os_interim) != 1 || is.na(rho_pfs_os_interim) ||
      rho_pfs_os_interim < -1 || rho_pfs_os_interim > 1) {
    stop("rho_pfs_os_interim must be between -1 and 1.")
  }
  
  has_os_int <- is.finite(os_interim_loghr) && is.finite(se_os_interim) && se_os_interim > 0
  
  dh <- fit_dh_params(eta_hat = eta_hat, Sigma0_hat = Sigma0_hat)
  mu_z <- dh$mu_z             # order: (S, T)
  Sigma_z <- dh$Sigma_z       # order: (S, T)
  
  if (!has_os_int) {
    y <- matrix(pfs_loghr, nrow = 1)
    H <- matrix(c(1, 0), nrow = 1)
    R <- matrix(se_pfs^2, nrow = 1)
    mode_label <- "PFS-only mode"
  } else {
    y <- matrix(c(pfs_loghr, os_interim_loghr), nrow = 2)
    H <- diag(2)
    R <- matrix(
      c(
        se_pfs^2, rho_pfs_os_interim * se_pfs * se_os_interim,
        rho_pfs_os_interim * se_pfs * se_os_interim, se_os_interim^2
      ),
      nrow = 2,
      byrow = TRUE
    )
    mode_label <- "Joint mode: D&H latent-final-OS prediction updated by current PFS and OS interim"
  }
  
  HSHT <- H %*% Sigma_z %*% t(H) + R
  inv_HSHT <- tryCatch(solve(HSHT), error = function(e) NULL)
  if (is.null(inv_HSHT)) {
    stop("Failed to invert the current-study observation covariance. Check SE inputs and rho_pfs_os_interim.")
  }
  
  K <- Sigma_z %*% t(H) %*% inv_HSHT  # 2 x k
  mu_obs <- H %*% mu_z
  post_mean_z <- mu_z + K %*% (y - mu_obs)
  post_cov_z <- Sigma_z - K %*% H %*% Sigma_z
  
  # Target is latent final OS effect T_* (second component under z=(S,T)).
  mean_pred <- as.numeric(post_mean_z[2])
  var_pred <- as.numeric(post_cov_z[2, 2])
  
  # Delta-method propagation of uncertainty in eta_hat only.
  # mu_z = P %*% eta_hat where eta_hat is ordered (OS, PFS).
  P_swap <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
  grad_mu_z <- matrix(c(0, 1), nrow = 1) %*% (diag(2) - K %*% H)
  grad_eta_hat <- grad_mu_z %*% P_swap
  var_pred <- var_pred + as.numeric(grad_eta_hat %*% V_eta %*% t(grad_eta_hat))
  
  if (!is.finite(var_pred) || var_pred <= 0) {
    stop("Final predictive variance is non-positive or invalid.")
  }
  
  if (!has_os_int) {
    pfs_weight <- as.numeric(K[2, 1])
    os_weight <- 0
  } else {
    pfs_weight <- as.numeric(K[2, 1])
    os_weight <- as.numeric(K[2, 2])
  }
  
  list(
    mean = mean_pred,
    se = sqrt(var_pred),
    y = y,
    H = H,
    R = R,
    K = K,
    mu_prior = mu_z,
    Sigma_prior = Sigma_z,
    post_mean_z = post_mean_z,
    post_cov_z = post_cov_z,
    dh_params = dh,
    has_os_interim = has_os_int,
    os_interim_weight = os_weight,
    pfs_weight = pfs_weight,
    weight_note = "Weights are coefficients on observed current-study endpoints in posterior mean of latent final OS.",
    mode_label = mode_label
  )
}

compute_pos <- function(
    hist_dat,
    current_pfs_loghr,
    current_os_se,
    current_pfs_se,
    current_rho,
    method = c("reml", "mm"),
    success_hr_threshold = 1.0,
    use_os_interim = FALSE,
    current_os_int_loghr = NA_real_,
    current_os_int_se = NA_real_,
    rho_pfs_osint = 0
) {
  method <- match.arg(method)
  
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
  if (!is.numeric(rho_pfs_osint) || length(rho_pfs_osint) != 1 || is.na(rho_pfs_osint) ||
      rho_pfs_osint < -1 || rho_pfs_osint > 1) {
    stop("rho_pfs_osint must be a single numeric value between -1 and 1.")
  }
  
  if (isTRUE(use_os_interim)) {
    if (!is.numeric(current_os_int_loghr) || length(current_os_int_loghr) != 1 || is.na(current_os_int_loghr)) {
      stop("current_os_int_loghr must be a single numeric value when use_os_interim = TRUE.")
    }
    if (!is.numeric(current_os_int_se) || length(current_os_int_se) != 1 || is.na(current_os_int_se) ||
        current_os_int_se <= 0) {
      stop("current_os_int_se must be a single positive numeric value when use_os_interim = TRUE.")
    }
  }
  
  meta_res <- fit_bivariate_meta(hist_dat = hist_dat, method = method)
  
  pred_res <- predict_os_dh(
    eta_hat = meta_res$eta_hat,
    Sigma0_hat = meta_res$Sigma0_hat,
    V_eta = meta_res$V_eta,
    pfs_loghr = current_pfs_loghr,
    se_pfs = current_pfs_se,
    os_interim_loghr = if (isTRUE(use_os_interim)) current_os_int_loghr else NA_real_,
    se_os_interim = if (isTRUE(use_os_interim)) current_os_int_se else NA_real_,
    rho_pfs_os_interim = if (isTRUE(use_os_interim)) rho_pfs_osint else 0
  )
  
  pos <- stats::pnorm(
    log(success_hr_threshold),
    mean = pred_res$mean,
    sd = pred_res$se
  )
  
  if (!is.finite(pos)) {
    stop("Computed PoS is invalid.")
  }
  
  warning_messages <- meta_res$warning_messages
  note_messages <- character(0)
  
  note_messages <- c(
    note_messages,
    "Prediction target is the latent final OS treatment effect, consistent across PFS-only and PFS+OS-interim modes.",
    "Under the Daniels-Hughes formulation, current_os_se is not used in the current-study prediction step because the target is latent final OS, not a future noisy final-OS estimate."
  )
  
  if (pos < 0.05 || pos > 0.95) {
    warning_messages <- c(
      warning_messages,
      "PoS is very close to 0 or 1. Check whether this is driven by strong data support or by restrictive modeling assumptions."
    )
  }
  
  if (isTRUE(use_os_interim)) {
    note_messages <- c(
      note_messages,
      "OS interim mode is ON: current PFS and current OS interim update the same latent final OS target.",
      sprintf(
        "Current-study observation weights for latent final OS: PFS = %.3f, OS interim = %.3f.",
        pred_res$pfs_weight, pred_res$os_interim_weight
      )
    )
    if (abs(rho_pfs_osint) < 1e-12) {
      note_messages <- c(
        note_messages,
        "rho_pfs_osint is set to 0. This is a sensitivity-analysis default, not a scientifically established true value."
      )
    }
  } else {
    note_messages <- c(
      note_messages,
      sprintf(
        "Current-study observation weight for latent final OS from PFS = %.3f.",
        pred_res$pfs_weight
      )
    )
  }
  
  list(
    method = method,
    pos = pos,
    mean_pred = pred_res$mean,
    sd_pred = pred_res$se,
    threshold_loghr = log(success_hr_threshold),
    eta_hat = meta_res$eta_hat,
    Sigma0_hat = meta_res$Sigma0_hat,
    warning_messages = unique(warning_messages),
    note_messages = unique(note_messages),
    fit = meta_res$fit,
    V_eta = meta_res$V_eta,
    predictor_details = pred_res,
    post_mean_vec = c(NA_real_, pred_res$mean),
    post_cov = matrix(c(NA_real_, NA_real_, NA_real_, pred_res$se^2), nrow = 2),
    os_interim_weight = pred_res$os_interim_weight,
    pfs_weight = pred_res$pfs_weight,
    dh_alpha = pred_res$dh_params$alpha,
    dh_beta = pred_res$dh_params$beta,
    dh_tau2 = pred_res$dh_params$tau2,
    dh_eta = pred_res$dh_params$eta,
    dh_psi2 = pred_res$dh_params$psi2
  )
}

compute_pos_both_methods <- function(
    hist_dat,
    current_pfs_loghr,
    current_os_se,
    current_pfs_se,
    current_rho,
    success_hr_threshold = 0.80,
    use_os_interim = FALSE,
    current_os_int_loghr = NA_real_,
    current_os_int_se = NA_real_,
    rho_pfs_osint = 0
) {
  res_reml <- tryCatch(
    compute_pos(
      hist_dat = hist_dat,
      current_pfs_loghr = current_pfs_loghr,
      current_os_se = current_os_se,
      current_pfs_se = current_pfs_se,
      current_rho = current_rho,
      method = "reml",
      success_hr_threshold = success_hr_threshold,
      use_os_interim = use_os_interim,
      current_os_int_loghr = current_os_int_loghr,
      current_os_int_se = current_os_int_se,
      rho_pfs_osint = rho_pfs_osint
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
      success_hr_threshold = success_hr_threshold,
      use_os_interim = use_os_interim,
      current_os_int_loghr = current_os_int_loghr,
      current_os_int_se = current_os_int_se,
      rho_pfs_osint = rho_pfs_osint
    ),
    error = function(e) structure(list(error_message = e$message), class = "app_error")
  )
  
  list(
    reml = res_reml,
    mm = res_mm
  )
}
