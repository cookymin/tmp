#-----------------------------------------------------------------------------
# Purpose:  Draw line plot with ggplot2
# Author:   Feiyang Niu
# Date:     July 11, 2016
#-----------------------------------------------------------------------------


# load required r scripts
use_package('ggplot2')
source('r_scripts/common_statistics.R')
source('r_scripts/plot_utils.R')


# gg_lineplot function
gg_lineplot <- function(data, xvar, yvar, group = NULL,
                        log_x = FALSE, log_y = FALSE,
                        xtick_angle = 0, ytick_angle = 0, hjust = 0.5,
                        x_lab = xvar, y_lab = yvar, title = '',
                        bw_theme = TRUE, strip_col = 'bisque1', alpha = 0.5,
                        facet_fml = NULL, reference_line = NULL,
                        summary_method = 'mean + sd',
                        with_points = FALSE, point_size = 2, rand_seed = 12345) {
    set.seed(rand_seed)
    if(is.null(data) || nrow(data) == 0) stop('There are no data to plot!')
    if(!is_blank(group)) {
        if(!all(c(xvar, yvar, group) %in% colnames(data)))
            stop('xvar, yvar, group should all match column names of data')
        else {
            if(!is.factor(data[[group]]))
                data[[group]] <- factor(data[[group]])
            num_groups <- length(unique(data[[group]]))
        }
    } else {
        if(!all(c(xvar, yvar) %in% colnames(data)))
            stop('xvar, yvar should all match column names of data')
    }
    if(isTRUE(log_x)) data <- data[data[[xvar]] > 0, ]
    if(isTRUE(log_y)) data <- data[data[[yvar]] > 0, ]
    xvar_class <- class(data[[xvar]])
    x_levels <- NULL
    if(xvar_class != 'numeric') {
        x_levels <- levels(factor(data[[xvar]]))
        data[[xvar]] <- as.numeric(factor(data[[xvar]]))
    }
    ggxyplot <- ggplot(data, aes_string(x = xvar, y = yvar))
    if(!is_blank(group)) {
        ggxyplot <- ggxyplot +
            aes_string(colour = group, group = group, fill = group)
    }
    if(bw_theme) ggxyplot <- ggxyplot + theme_bw()
    if(xvar_class != 'numeric') {
        ggxyplot <- ggxyplot +
            scale_x_continuous(breaks = 1:length(x_levels), labels = x_levels)
    }
    separation <- 0.03 * diff(range_na(data[[xvar]]))
    dodge_ <- position_dodge(separation)
    position_jitter_ <- position_jitter(width = 1.5 * separation)
    position_jitterdodge_ <- position_jitterdodge(dodge.width = separation,
                                                  jitter.width = 1.5*separation)
    errorbar_width <- 0.025 * diff(range_na(data[[xvar]]))
    if(is.logical(with_points) && with_points) {
        if(is_blank(group)) {
            ggxyplot <- ggxyplot +
                geom_point(position = position_jitter_, size = point_size)
        } else {
            ggxyplot <- ggxyplot +
                geom_point(pch = 21, size = point_size,
                           position = position_jitterdodge_)
        }
    }
    if(!is_blank(summary_method)) {
        summary_method <- tolower(summary_method)
        summary_method <- match.arg(
            summary_method, c('mean + se','mean + sd','median + iqr')
        )
        fun_y_ <- if(summary_method=='median + iqr') stats::median else base::mean
        if(summary_method == 'mean + se') fun_data_ <- mean_se
        else if(summary_method == 'mean + sd') fun_data_ <- mean_sd
        else fun_data_ <- median_iqr
        ggxyplot <- ggxyplot +
            stat_summary(fun.y = fun_y_, geom = 'line', position = dodge_) +
            stat_summary(fun.y = fun_y_, geom = 'point', position = dodge_) +
            stat_summary(fun.data = fun_data_, geom = 'errorbar',
                         width = errorbar_width, position = dodge_)
    }
    ggxyplot <- ggxyplot +
        scale_shape_manual(values = seq_len(num_groups)) +
        labs(x = x_lab, y = y_lab, title = title) +
        theme(axis.text.x = element_text(angle = xtick_angle, hjust = hjust),
              axis.text.y = element_text(angle = ytick_angle, hjust = hjust),
              plot.title = element_text(hjust = 0.5))
    if(!is.null(facet_fml))
        ggxyplot <- ggxyplot + facet_grid(facet_fml)
    # if(!is.null(strip_col) && areColors(strip_col)) {
    #     ggxyplot <- ggxyplot +
    #         theme(strip.background = element_rect(fill = strip_col))
    # }
    if(isTRUE(log_x)) {
        # ggxyplot <- ggxyplot + scale_x_log10()
        ggxyplot <- ggxyplot +
            scale_x_continuous(trans = 'log10',
                               breaks = base_breaks(),
                               labels = prettyNum)
    }
    if(isTRUE(log_y)) {
        # ggxyplot <- ggxyplot + scale_y_log10()
        ggxyplot <- ggxyplot +
            scale_y_continuous(trans = 'log10',
                               breaks = base_breaks(),
                               labels = prettyNum)
    }
    if(!is.null(reference_line)) {
        if(is.numeric(reference_line)) {
            ggxyplot <- ggxyplot +
                geom_hline(yintercept = reference_line, linetype = 'dashed')
        }
        if(is.logical(reference_line) && reference_line) {
            ggxyplot <- ggxyplot +
                geom_hline(yintercept = 0, linetype = 'dashed')
        }
    }
    return(ggxyplot)
}































