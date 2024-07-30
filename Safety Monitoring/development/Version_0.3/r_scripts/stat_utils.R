#-----------------------------------------------------------------------------
# Purpose:  Utility functions for statistical modelling/testing
# Author:   Feiyang Niu
# Date:     July 18, 2017
#-----------------------------------------------------------------------------

#' Calculate Fisher's exact test p value for the hypothesis that AE incidence 
#' rate in the Treatment arm is significantly greater than that in the Control
#' arm
#' 
#' @param n_ae_trt Number of AE in the Treatment arm
#' @param n_ae_ctr Number of AE in the Control arm
#' @param n_pts_trt Number of patients in the Treatment arm
#' @param n_pts_ctr Number of patients in the Control arm
fisher_pvalue <- function(n_ae_trt, n_ae_ctr, n_pts_trt, n_pts_ctr) {
    count <- cbind(n_ae_trt, n_ae_ctr, n_pts_trt, n_pts_ctr)
    fisher_p <- function(x) {
        fisher.test(matrix(c(x[1], x[2], x[3] - x[1], x[4] - x[2]), nrow = 2),
                    alternative = 'greater')$p.value
    }
    apply(count, 1, fisher_p)
}


#' Calculate EAIR test p value between Treatment and Control arms
#' 
#' @param n_ae_trt Number of AE in the Treatment arm
#' @param n_ae_ctr Number of AE in the Control arm
#' @param exp_trt Total exposure time of AE in the Treatment arm
#' @param exp_ctr Total exposure time of AE in the Control arm
EAIR_pvalue <- function(n_ae_trt, n_ae_ctr, exp_trt, exp_ctr) {
    poisson_exact <- function(n, t){
        1 - pbinom(n[1] - 1, sum(n), t[1] / sum(t))
    }
    count_time <- cbind(n_ae_trt, exp_trt, n_ae_ctr, exp_ctr)
    apply(count_time, 1, function(x) ifelse(all(!is.na(x)), poisson_exact(x[c(1, 3)], x[c(2, 4)]), NA))
}


#' Calculate logrank test p value between Treatment and Control arms
#' 
#' @param n_pts_trt Number of patients in the Treatment arm
#' @param n_pts_ctr Number of patients in the Control arm
#' @param total_pt Total number of PT's
#' @param surv_trt Censored survival for the Treatment arm (time to AE or to 
#'  the last follow-up (i.e. patient's level exposure time))
#' @param surv_ctr Censored survival for the Control arm (time to AE or to 
#'  the last follow-up (i.e. patient's level exposure time))
#' @param status_trt AE indicator for the Treatment arm
#' @param status_ctr AE indicator for the Control arm
logrank_pvalue <- function(n_pts_trt, n_pts_ctr, total_pt,
                           surv_trt, surv_ctr, status_trt, status_ctr) {
    logrank_p <- function(time, status, trt){
        fit <- survdiff(Surv(time, status) ~ trt)
        p <- (1 - pchisq(fit$chisq, 1)) / 2
        if (fit$obs[2] < fit$exp[2]) p <- 1 - p
        p
    }
    Nt <- unique(n_pts_trt)
    Nc <- unique(n_pts_ctr)
    Surv_arr <- array(NA, dim = c((Nt + Nc), 3, unique(total_pt)))
    Surv_arr[1:Nt, 1, ] <- surv_trt
    Surv_arr[(Nt + 1):(Nt + Nc), 1, ] <- surv_ctr
    Surv_arr[1:Nt, 2, ] <- status_trt
    Surv_arr[(Nt + 1):(Nt + Nc), 2, ] <- status_ctr
    Surv_arr[, 3, ] <- rep(c(1, 0), c(Nt, Nc))
    
    apply(Surv_arr, 3, function(x) ifelse(all(!is.na(x[, 1])), (logrank_p(x[, 1], x[, 2], x[, 3])), NA))
}


#' Calculate landmark KM test p value between Treatment and Control arms
#' 
#' @param n_pts_trt Number of patients in the Treatment arm
#' @param n_pts_ctr Number of patients in the Control arm
#' @param total_pt Total number of PT's
#' @param surv_trt Censored survival for the Treatment arm (time to AE or to 
#'  the last follow-up (i.e. patient's level exposure time))
#' @param surv_ctr Censored survival for the Control arm (time to AE or to 
#'  the last follow-up (i.e. patient's level exposure time))
#' @param status_trt AE indicator for the Treatment arm
#' @param status_ctr AE indicator for the Control arm
landmark_km_pvalue <- function(n_pts_trt, n_pts_ctr, total_pt,
                               surv_trt, surv_ctr, status_trt, status_ctr, t.lm = 1) {
    km_p <- function(time, status, trt, t.lm = 1){
        fit1 <- survfit(Surv(time, status) ~ trt)
        n0.out <- fit1$strata[1]
        n1.out <- fit1$strata[2]
        ind0 <- max(sum(fit1$time[1:n0.out] <= t.lm), 1)
        ind1 <- max(sum(fit1$time[(n0.out + 1):(n0.out + n1.out)] <= t.lm), 1)
        S0 <- fit1$surv[ind0]
        n0.risk <- fit1$n.risk[ind0]
        S1 <- fit1$surv[n0.out + ind1]
        n1.risk <- fit1$n.risk[n0.out + ind1]
        # pnorm((asin(sqrt(S1)) - asin(sqrt(S0)))/sqrt(0.25/n0.risk+0.25/n1.risk))
        return(list(km_ctr = S0,
                    km_trt = S1,
                    km_p = pnorm((asin(sqrt(S1)) - asin(sqrt(S0)))/sqrt(0.25/n0.risk+0.25/n1.risk))
                    )
               )
    }
    Nt <- unique(n_pts_trt)
    Nc <- unique(n_pts_ctr)
    Surv_arr <- array(NA, dim = c((Nt + Nc), 4, unique(total_pt)))
    Surv_arr[1:Nt, 1, ] <- surv_trt
    Surv_arr[(Nt+1):(Nt+Nc), 1, ] <- surv_ctr
    Surv_arr[1:Nt, 2, ] <- status_trt
    Surv_arr[(Nt+1):(Nt+Nc), 2, ] <- status_ctr
    Surv_arr[, 3, ] <- rep(c(1, 0), c(Nt, Nc))
    Surv_arr[1, 4, ] <- t.lm
    # apply(Surv_arr, 3, function(x) ifelse(all(!is.na(x[, 1])), (km_p(x[, 1], x[, 2], x[, 3], x[1, 4])$km_p), NA))
    return(list(km_trt = apply(Surv_arr, 3, function(x) ifelse(all(!is.na(x[, 1])), (km_p(x[, 1], x[, 2], x[, 3], x[1, 4])$km_trt), NA)),
                km_ctr = apply(Surv_arr, 3, function(x) ifelse(all(!is.na(x[, 1])), (km_p(x[, 1], x[, 2], x[, 3], x[1, 4])$km_ctr), NA)),
                km_p = apply(Surv_arr, 3, function(x) ifelse(all(!is.na(x[, 1])), (km_p(x[, 1], x[, 2], x[, 3], x[1, 4])$km_p), NA))
                )
           )
}


#' Calculate odds ratio and its confidence intervals
#' 
#' @description Calculate odds ratio and its confidence intervals based on
#' approximation, followed by null-hypothesis (odds ratio equals to 1) testing.
#' 
#' @param a The number of individuals who both suffer from exposure and disease.
#' @param b The number of individuals who suffer from exposure but are healthy.
#' @param c The number of individuals who suffer from disesase but not exposed.
#' @param d The number of individuals who neither suffered from exposure nor
#'  disease
#' @param zero_correction Number to be added to \code{a}, \code{b}, \code{c},
#'  and \code{d} when either \code{a} or \code{b} is zero
#' @param conf_level Probability for confidence intervals. Default is 0.95.
#' @param ci Whether confidence interval needs to be included in the result
#' @param p_value Whether p value needs to be included in the result
#' @return A list of the following components: \code{estimate} (point of
#'  estimate of odds ratio), \code{ci} (a numeric vector of length 2 to
#'  give lower/upper limit of confidence interval), and \code{p_value} (the
#'  significance probability as the result of null-hypothesis testings)
odds_ratio <- function(a, b, c, d, zero_correction = 0.5, conf_level = 0.95,
                       ci = TRUE, p_value = TRUE) {
    idx_correction <- a == 0 | c == 0
    if(sum(idx_correction) > 0) {
        a[idx_correction] <- a[idx_correction] + zero_correction
        b[idx_correction] <- b[idx_correction] + zero_correction
        c[idx_correction] <- c[idx_correction] + zero_correction
        d[idx_correction] <- d[idx_correction] + zero_correction
    }
    estimate <- a * d / (b * c)
    result <- list(estimate = estimate)
    if(isTRUE(ci) || isTRUE(p_value)) {
        alpha <- 1 - conf_level
        qvalue <- qnorm(1 - alpha / 2)
        log_or <- log(estimate)
        log_se <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
        if(isTRUE(ci)) {
            ci <- cbind(exp(log_or - qvalue * log_se),
                        exp(log_or + qvalue * log_se))
            colnames(ci) <- paste0(c('Lower ', 'Upper '), conf_level * 100, '%')
            result$ci <- ci
        }
        if(isTRUE(p_value)) {
            p_value <- 2 * pnorm(-abs(log_or) / log_se)
            result$p_value <- p_value
        }
    }
    return(result)
}



#' Calculate relative risk and its confidence intervals
#' 
#' @description Calculate relative risk and its confidence intervals based on
#' approximation, followed by null-hypothesis (relative risk equals to 1)
#' testing.
#' 
#' @param a The number of individuals who both suffer from exposure and disease.
#' @param b The number of individuals who suffer from exposure but are healthy.
#' @param c The number of individuals who suffer from disesase but not exposed.
#' @param d The number of individuals who neither suffered from exposure nor
#'  disease
#' @param zero_correction Number to be added to \code{a}, \code{b}, \code{c},
#'  and \code{d} when either \code{a} or \code{b} is zero
#' @param conf_level Probability for confidence intervals. Default is 0.95.
#' @param ci Whether confidence interval needs to be included in the result
#' @return A list of the following components: \code{estimate} (point of
#'  estimate of relative risk), \code{ci} (a numeric vector of length 2 to
#'  give lower/upper limit of confidence interval)
relative_rick <- function(a, b, c, d, zero_correction = 0.5, conf_level = 0.95,
                          ci = TRUE) {
    idx_correction <- a == 0 | c == 0
    if(sum(idx_correction) > 0) {
        a[idx_correction] <- a[idx_correction] + zero_correction
        b[idx_correction] <- b[idx_correction] + zero_correction
        c[idx_correction] <- c[idx_correction] + zero_correction
        d[idx_correction] <- d[idx_correction] + zero_correction
    }
    estimate <- a * (c + d) / (c * (a + b))
    result <- list(estimate = estimate)
    if(isTRUE(ci) || isTRUE(p_value)) {
        alpha <- 1 - conf_level
        qvalue <- qnorm(1 - alpha / 2)
        log_rr <- log(estimate)
        log_se <- sqrt(1 / a - 1 / (a + b) + 1 / c - 1 / (c + d))
        if(isTRUE(ci)) {
            ci <- cbind(exp(log_rr - qvalue * log_se),
                        exp(log_rr + qvalue * log_se))
            colnames(ci) <- paste0(c('Lower ', 'Upper '), conf_level * 100, '%')
            result$ci <- ci
        }
    }
    return(result)
}


#' Calculate risk difference and its confidence intervals
#' 
#' @description Calculate risk difference and its confidence intervals based on
#' approximation, followed by null-hypothesis (risk difference equals to 0)
#' testing.
#' 
#' @param a The number of individuals who both suffer from exposure and disease.
#' @param b The number of individuals who suffer from exposure but are healthy.
#' @param c The number of individuals who suffer from disesase but not exposed.
#' @param d The number of individuals who neither suffered from exposure nor
#'  disease
#' @param conf_level Probability for confidence intervals. Default is 0.95.
#' @param ci Whether confidence interval needs to be included in the result
#' @return A list of the following components: \code{estimate} (point of
#'  estimate of risk difference), \code{ci} (a numeric vector of length 2 to
#'  give lower/upper limit of confidence interval)
risk_difference <- function(a, b, c, d, conf_level = 0.95, ci = TRUE,
                            p_value = TRUE) {
    p_trt <- a / (a + b)
    p_ctr <- c / (c + d)
    estimate <- p_trt - p_ctr
    result <- list(estimate = estimate)
    if(isTRUE(ci) || isTRUE(p_value)) {
        alpha <- 1 - conf_level
        qvalue <- qnorm(1 - alpha / 2)
        se <- sqrt(p_trt * (1 - p_trt) / (a + b) + p_ctr * (1 - p_ctr) / (c + d))
        if(isTRUE(ci)) {
            ci <- cbind(estimate - qvalue * se, estimate + qvalue * se)
            colnames(ci) <- paste0(c('Lower ', 'Upper '), conf_level * 100, '%')
            result$ci <- ci
        }
        if(isTRUE(p_value)) {
            chi <- estimate / se
            p_value <- 1 - pnorm(chi)
            result$p_value <- p_value
        }
    }
    return(result)
}



#' Calculate hazard ratio and its confidence intervals
#' 
#' @param n_pts_trt Number of patients in the Treatment arm
#' @param n_pts_ctr Number of patients in the Control arm
#' @param total_pt Total number of PT's
#' @param surv_trt Censored survival for the Treatment arm (time to AE or to 
#'  the last follow-up (i.e. patient's level exposure time))
#' @param surv_ctr Censored survival for the Control arm (time to AE or to 
#'  the last follow-up (i.e. patient's level exposure time))
#' @param status_trt AE indicator for the Treatment arm
#' @param status_ctr AE indicator for the Control arm
hazard_ratio <- function(n_pts_trt, n_pts_ctr, total_pt,
                         surv_trt, surv_ctr, status_trt, status_ctr,
                         conf_level = 0.95, ci = FALSE) {
    hratio <- function(time, status, trt, ci = FALSE){
        fit <- summary(coxph(Surv(time, status) ~ trt), conf.int = conf_level)
        hr <- fit$conf.int[1]
        result <- hr
        if(isTRUE(ci)) {
            result <- c(result, fit$conf.int[3], fit$conf.int[4])
        }
        return(result)
    }
    Nt <- unique(n_pts_trt)
    Nc <- unique(n_pts_ctr)
    Surv_arr <- array(NA, dim = c((Nt + Nc), 3, unique(total_pt)))
    Surv_arr[1:Nt, 1, ] <- surv_trt
    Surv_arr[(Nt + 1):(Nt + Nc), 1, ] <- surv_ctr
    Surv_arr[1:Nt, 2, ] <- status_trt
    Surv_arr[(Nt + 1):(Nt + Nc), 2, ] <- status_ctr
    Surv_arr[, 3, ] <- rep(c(1, 0), c(Nt, Nc))
    
    if(isTRUE(ci)) {
        res <- apply(
            Surv_arr, 3, function(x) ternary(all(!is.na(x[, 1])), (hratio(x[, 1], x[, 2], x[, 3], ci = ci)), c(NA, NA, NA))
        )
        result <- list(estimate = res[1, ])
        ci_colname <- paste0(c('Lower ', 'Upper '), conf_level * 100, '%')
        result$ci <- `colnames<-`(t(res[-1, , drop = FALSE]), ci_colname)
        return(result)
    } else {
        res <- apply(
            Surv_arr, 3, function(x) ternary(all(!is.na(x[, 1])), (hratio(x[, 1], x[, 2], x[, 3], ci = ci)), NA)
        )
        result <- list(estimate = c(res))
        return(result)
    }
}
























