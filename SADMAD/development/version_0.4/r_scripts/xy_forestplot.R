#-----------------------------------------------------------------------------
# Purpose:  Draw forest plot with xyplot in lattice package
# Author:   Feiyang Niu
# Date:     August 30, 2016
#-----------------------------------------------------------------------------


# load required r scripts
use_package('dplyr')
use_package('lazyeval')
use_package('lattice')
use_package('latticeExtra')
source('r_scripts/common_statistics.R')


xy_forestplot <- function(data, xvar, yvar1, yvar2 = NULL, group = NULL,
                          xlab = xvar, ylab1 = yvar1, ylab2 = yvar2,
                          xlog = FALSE, ylog1 = FALSE, ylog2 = FALSE,
                          title = NULL, method = 'mean_se', type = '') {
    
    xlog <- isTRUE(xlog)
    ylog1 <- isTRUE(ylog1)
    ylog2 <- isTRUE(ylog2)
    method <- match.arg(tolower(method), c('mean_se', 'mean_sd', 'median_iqr'))
    
    if(method == 'mean_se' || method == 'mean_sd') {
        if(method == 'mean_se') error_func <- std_err
        else error_func <- sd_na
        expr <- list(
            interp(~mean_na(var), var = as.name(yvar1)),
            interp(~(mean_na(var) - error_func(var)), var = as.name(yvar1)),
            interp(~(mean_na(var) + error_func(var)), var = as.name(yvar1))
        )
        if(!is_blank(yvar2)) {
            expr <- append(expr, list(
                interp(~mean_na(var), var = as.name(yvar2)),
                interp(~(mean_na(var) - error_func(var)), var = as.name(yvar2)),
                interp(~(mean_na(var) + error_func(var)), var = as.name(yvar2))
            ))
        }
    } else if(method == 'median_iqr') {
        expr <- list(
            interp(~median_na(var), var = as.name(yvar1)),
            interp(~q1_na(var), var = as.name(yvar1)),
            interp(~q3_na(var), var = as.name(yvar1))
        )
        if(!is_blank(yvar2)) {
            expr <- append(expr, list(
                interp(~median_na(var), var = as.name(yvar2)),
                interp(~q1_na(var), var = as.name(yvar2)),
                interp(~q3_na(var), var = as.name(yvar2))
            ))
        }
    }
    if(!is_blank(yvar2)) {
        dots <- setNames(
            expr,
            c('yavg1', 'lower1', 'upper1', 'yavg2', 'lower2', 'upper2')
        )
    } else dots <- setNames(expr, c('yavg1', 'lower1', 'upper1'))
    
    if(!is_blank(group)) {
        smmry <- data %>% arrange_(group, xvar) %>% group_by_(group, xvar)
    } else {
        smmry <- data %>% arrange_(xvar) %>% group_by_(xvar)
    }
    smmry <- smmry %>% summarise_(.dots = dots)
    
    ylim1 <- range_na(smmry$yavg1, smmry$lower1, smmry$upper1)
    ylim1 <- extendrange(r = ylim1)
    if(!is_blank(yvar2)) {
        ylim2 <- range_na(smmry$yavg2, smmry$lower2, smmry$upper2)
        ylim2 <- extendrange(r = ylim2)
    }
    
    scales_list <- list()
    if(xlog) scales_list$x <- list(log = 10)
    scales_list1 <- scales_list
    if(ylog1) scales_list1$y <- list(log = 10)
    groups_ <- NULL
    auto_key <- FALSE
    if(!is_blank(group)) {
        smmry[[group]] <- as.factor(smmry[[group]])
        levels(smmry[[group]]) <- paste(group, '=', levels(smmry[[group]]))
        groups_ <- smmry[[group]]
        auto_key <- list(columns = nlevels(groups_))
    }
    final_plot <- xyplot(
        formula(paste('yavg1 ~', xvar)), smmry, pch = 20, xlab = xlab,
        ylab = ylab1, ylim = ylim1, main = title, scales = scales_list1,
        groups = groups_, auto.key = auto_key,
        lower = smmry$lower1, upper = smmry$upper1,
        panel = function(x, y, lower, upper, ...) {
            panel.xyplot(x,y, type = type, ...)
            panel.arrows(x, lower, x, upper, angle = 90, code = 3,
                         length = 0.05, ...)
        }
    )
    if(!is_blank(yvar2)) {
        scales_list2 <- scales_list
        if(ylog2) scales_list2$y <- list(log = 10)
        if(!is.null(group)) {
            formula_2 <- formula(paste('yavg2 ~', xvar, '|', group))
        } else {
            formula_2 <- formula(paste('yavg2 ~', xvar))
        }
        extra_plot <- xyplot(
            formula(paste('yavg2 ~', xvar)), smmry, pch = 20, xlab = xlab,
            ylab = ylab2, ylim = ylim2, main = title, scales = scales_list2,
            groups = groups_, auto.key = auto_key,
            lower = smmry$lower2, upper = smmry$upper2,
            panel = function(x, y, lower, upper, ...) {
                panel.xyplot(x,y, type = type, ...)
                panel.arrows(x, lower, x, upper, angle = 90, code = 3, length = 0.05)
            }
        )
        final_plot <- doubleYScale(final_plot, extra_plot, add.ylab2 = TRUE)
    }
    return(final_plot)
}






























