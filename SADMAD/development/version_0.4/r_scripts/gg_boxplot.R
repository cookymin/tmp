#-----------------------------------------------------------------------------
# Purpose:  Draw box plot with ggplot2
# Author:   Feiyang Niu
# Date:     July 11, 2016
#-----------------------------------------------------------------------------


# load required r scripts
use_package('ggplot2')
source('r_scripts/common_statistics.R')
source('r_scripts/plot_utils.R')


# gg_boxplot function
gg_boxplot <- function(data, xvar, yvar, group = NULL,
                       log_x = FALSE, log_y = FALSE,
                       xtick_angle = 0, ytick_angle = 0, hjust = 0.5,
                       x_lab = xvar, y_lab = yvar, title = '',
                       bw_theme = TRUE, strip_col = 'bisque1', alpha = 0.5,
                       facet_fml = NULL, reference_line = NULL,
                       with_points = TRUE, point_size = 2, with_line = FALSE,
                       rand_seed = 12345) {
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
    if(!is.factor(data[[xvar]]))
        data[[xvar]] <- factor(data[[xvar]])
    ggxyplot <- ggplot(data, aes_string(x = xvar, y = yvar))
    if(!is_blank(group))
        ggxyplot <- ggxyplot + aes_string(colour = group)
    if(isTRUE(bw_theme)) ggxyplot <- ggxyplot + theme_bw()
    outlier_shape <- ifelse(with_points, NA, 19)
    ggxyplot <- ggxyplot + geom_boxplot(outlier.shape = outlier_shape)
    if(isTRUE(with_points)) {
        if(is_blank(group)) {
            ggxyplot <- ggxyplot +
                geom_point(position = position_jitter(width = 0.5),
                           size = point_size)
        } else {
            ggxyplot <- ggxyplot +
                geom_point(aes_string(fill = group), pch = 21, size = point_size,
                           position = position_jitterdodge(jitter.width=0.5))
        }
    }
    if(isTRUE(with_line)) {
        data$xvar_line <- as.numeric(data[[xvar]])
        if(is_blank(group)) {
            ggxyplot <- ggxyplot +
                stat_summary(data = data, aes_string(x = 'xvar_line', y = yvar), 
                             fun.y = mean_na, geom = 'line') +
                stat_summary(data = data, aes_string(x = 'xvar_line', y = yvar),
                             fun.y = mean_na, geom = 'point', shape = 2)
        } else {
            ggxyplot <- ggxyplot +
                stat_summary(
                    data = data,
                    aes_string(x = 'xvar_line', y = yvar, colour = group),
                    fun.y = mean_na, geom = 'line',
                    position = position_dodge(width = 0.7)
                ) +
                stat_summary(
                    data = data,
                    aes_string(x = 'xvar_line', y = yvar, colour = group),
                    fun.y = mean_na, geom = 'point', shape = 2,
                    position = position_dodge(width = 0.7)
                )
        }
    }
    ggxyplot <- ggxyplot +
        scale_shape_manual(values = seq_len(num_groups)) +
        labs(x = x_lab, y = y_lab, title = title)+
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




























