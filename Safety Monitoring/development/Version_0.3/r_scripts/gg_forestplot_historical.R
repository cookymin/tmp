#-----------------------------------------------------------------------------
# Purpose:  Draw dotplot with ggplot2
# Author:   Feiyang Niu
# Date:     March 6, 2017
#-----------------------------------------------------------------------------


# load required r scripts
use_package('ggplot2')
source('r_scripts/common_statistics.R')
source('r_scripts/plot_utils.R')


# a number of summary functions
mean_ci_asymp_normal <- function(x, conf.int = 0.95, na.rm = TRUE) {
    na.rm <- isTRUE(na.rm)
    if(na.rm) x <- x[!is.na(x)]
    n <- length(x)
    xbar <- mean(x)
    if(n < 2L) return(data.frame(y = xbar, ymin = NA, ymax = NA))
    std_error <- stats::sd(x) / sqrt(n)
    error <- std_error * qnorm((1 + conf.int) / 2)
    return(data.frame(y = xbar, ymin = xbar - error, ymax = xbar + error))
}
mean_sd <- function(x, mult = 1, na.rm = TRUE) {
    na.rm <- isTRUE(na.rm)
    if(na.rm) x <- x[!is.na(x)]
    n <- length(x)
    xbar <- mean(x)
    if(n < 2L) return(data.frame(y = xbar, ymin = NA, ymax = NA))
    error <- stats::sd(x) * mult
    return(data.frame(y = xbar, ymin = xbar - error, ymax = xbar + error))
}


gg_forestplot <- function(data, x_var, y_var,
                          lower_var = NULL, upper_var = NULL,
                          facet_r_var = NULL, facet_c_var = NULL,
                          color_var = NULL, fill_var = NULL, shape_var = NULL,
                          draw_x_axis = TRUE, draw_y_axis = TRUE,
                          x_lab = x_var, y_lab = y_var, title = '',
                          x_limit = NULL, y_limit = NULL,
                          x_log = FALSE, y_log = FALSE,
                          summarize_method = 'mean',
                          ci_method = 'mean_ci_asymp_normal',
                          add_legend = TRUE, legend_pos = 'bottom',
                          reference_hline = NULL, reference_vline = NULL,
                          strip_title = NULL, strip_color = '#ffe5cc',
                          label_align = 1, grids = 'y') {
    
    #-----------------------------
    # argument match & error catch
    #-----------------------------
    if(!is.data.frame(data)) {
        tryCatch(
            data <- as.data.frame(data),
            error = function(err) {stop(
                'data must be a data frame or data frame convertable'
            )}
        )
    }
    all_columns <- names(data)
    if(!all(x_var %in% all_columns, y_var %in% all_columns))
        stop('x_var & y_var both must be columns of data')
    is_x_continuous <- is.numeric(data[[x_var]])
    is_y_continuous <- is.numeric(data[[y_var]])
    if(is_x_continuous == is_y_continuous) {
        stop('x_var & y_var must be one continuous and another categorical')
    }
    if(!is_blank(lower_var)) {
        if(!(lower_var %in% all_columns))
            stop('lower_var must be a column of data')
    }
    if(!is_blank(upper_var)) {
        if(!(upper_var %in% all_columns))
            stop('upper_var must be a column of data')
    }
    if(!is_blank(facet_r_var)) {
        if(!(facet_r_var %in% all_columns))
            stop('facet_r_var must be a column of data')
    }
    if(!is_blank(facet_c_var)) {
        if(!(facet_c_var %in% all_columns))
            stop('facet_c_var must be a column of data')
    }
    if(!is_blank(color_var)) {
        if(!(color_var %in% all_columns))
            stop('color_var must be a column of data')
    }
    if(!is_blank(fill_var)) {
        if(!(fill_var %in% all_columns))
            stop('fill_var must be a column of data')
    }
    if(!is_blank(shape_var)) {
        if(!(shape_var %in% all_columns))
            stop('shape_var must be a column of data')
    }
    draw_x_axis <- isTRUE(draw_x_axis)
    draw_y_axis <- isTRUE(draw_y_axis)
    x_log <- isTRUE(x_log)
    y_log <- isTRUE(y_log)
    if(!is_blank(summarize_method)) {
        if(!is.function(summarize_method)) {
            if(!summarize_method %in% c('mean', 'median', 'min', 'max'))
            {
                stop('summarize_method must be one of `c("mean", ',
                     '"median", "min", "max")`')
            }
        }
    }
    if(!is_blank(ci_method)) {
        if(!is.function(ci_method)) {
            if(!ci_method %in% c('mean_ci_normal', 'mean_ci_asymp_normal',
                                 'mean_ci_boot', 'mean_sd', 'mean_se'))
            {
                stop('ci_method must be one of `c("mean_ci_normal", ',
                     '"mean_ci_asymp_normal", "mean_ci_boot", "mean_sd", ',
                     '"mean_se")`')
            }
        }
    }
    add_legend <- isTRUE(add_legend)
    if(!legend_pos %in% c('left', 'right', 'bottom', 'top')) {
        stop('legend_pos must be one of `c("left", "right", "bottom", "top")`')
    }
    if(!is_blank(label_align)) {
        if(is.numeric(label_align)) {
            if(label_align > 1 || label_align < 0) {
                stop('label_align must be between [0, 1]')
            }
        } else if(is.character(label_align)) {
            if(!label_align %in% c('up', 'bottom', 'left', 'right', 'center')) {
                stop(paste0('label_align must be one of `c("up", "bottom", ',
                            '"left", "right", "center")`'))
            }
        } else {
            stop('label_align must be either numeric or character')
        }
    }
    if(!is_blank(strip_color)) {
        if(!all(areColors(strip_color))) {
            stop('strip_color must be valid color representation')
        }
    }
    if(!is_blank(grids)) {
        if(!grids %in% c('on', 'x', 'y', 'off')) {
            stop('grids must be one of `c("on", "x", "y", "off")`')
        }
    }
    
    #-----------------------------
    # define constants and vars
    #-----------------------------
    alignment_dict <- list(
        'left' = 0, 'center' = 0.5, 'right' = 1,
        'up' = 1, 'bottom' = 0
    )
    if(!is_blank(shape_var)) {
        num_shapes <- length(unique(na.omit(data[[shape_var]])))
        all_shapes <- c(0:25, 32:255)[seq_len(num_shapes)]
    } else all_shapes <- NULL
    con_var <- ifelse(is_x_continuous, x_var, y_var)
    cat_var <- ifelse(is_x_continuous, y_var, x_var)
    if(is_blank(summarize_method)) summarize_method <- 'none'
    if(summarize_method == 'none') summarize_method <- 'identity'
    if(!is_blank(strip_title)) {
        data[['strip_title']] <- factor(rep(strip_title, nrow(data)))
        if(is_blank(facet_c_var)) {
            facet_c_var <- 'strip_title'
        }
    }
    if(!is_blank(lower_var) || !is_blank(upper_var)) {
        if(any(is_blank(lower_var), is_blank(upper_var)))
            stop('Please provide both `lower_var` and `upper_var`')
    }
    if(is_blank(ci_method)) ci_method <- 'mean_ci_asymp_normal'
    if(ci_method == 'mean_ci_normal')
        ci_method <- ggplot2::mean_cl_normal
    else if(ci_method == 'mean_ci_asymp_normal')
        ci_method <- mean_ci_asymp_normal
    else if(ci_method == 'mean_ci_boot')
        ci_method <- ggplot2::mean_cl_boot
    else if(ci_method == 'mean_sd')
        ci_method <- mean_sd
    else if(ci_method == 'mean_se')
        ci_method <- ggplot2::mean_se
    flip_coord <- is_x_continuous
    if(flip_coord) {
        swap_holder <- reference_hline
        reference_hline <- reference_vline
        reference_vline <- swap_holder
        
        swap_holder <- x_log
        x_log <-y_log
        y_log <- swap_holder
    }
    
    #-----------------------------
    # make the plot
    #-----------------------------
    plot_ <- gg_wrapper(
        data = data,
        aes_string(x = paste0('`', cat_var, '`'),
                   y = paste0('`', con_var, '`')),
        facet_r = facet_r_var, facet_c = facet_c_var,
        facet_scale = 'free', facet_space = 'free',
        x_lab = x_lab, y_lab = y_lab, title = title,
        x_limit = x_limit, y_limit = y_limit,
        x_log = x_log, y_log = y_log,
        add_legend = add_legend, legend_pos = legend_pos,
        color_var = color_var, fill_var = fill_var,
        shape_var = shape_var, all_shapes = all_shapes,
        reference_hline = reference_hline, reference_vline = reference_vline,
        grids = grids, strip_background_color = strip_color
    )
    
    # add points
    plot_ <- plot_ +
        stat_summary(fun.y = summarize_method, geom = 'point',
                     position = position_dodge(0.2))
    if(!is_blank(lower_var) && !is_blank(upper_var)) {
        plot_ <- plot_ +
            geom_errorbar(aes_string(ymin = lower_var, ymax = upper_var),
                          width = 0.2, position = position_dodge(0.2))
    } else {
        plot_ <- plot_ +
            stat_summary(fun.data = ci_method, geom = 'errorbar', width = 0.2,
                         position = position_dodge(0.2))
    }
    
    # draw x-/y- axis and adjust the labels
    if(is_blank(label_align)) {
        label_align <- 1
    } else if(is.character(label_align)){
        label_align <- alignment_dict[[label_align]]
    }
    if(draw_x_axis) {
        if(!is_x_continuous) {
            plot_ <- plot_ +
                theme(axis.text.x = element_text(angle = 90,
                                                 hjust = label_align))
        }
    } else {
        plot_ <- plot_ +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
    }
    if(draw_y_axis) {
        if(!is_y_continuous) {
            plot_ <- plot_ +
                theme(axis.text.y = element_text(hjust = label_align))
        }
    } else {
        plot_ <- plot_ +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
    }
    
    # flip coordinate
    if(flip_coord) plot_ <- plot_ + coord_flip()
    
    return(plot_)
    
}






















