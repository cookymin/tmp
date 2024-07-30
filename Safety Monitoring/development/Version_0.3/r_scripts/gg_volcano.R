#-----------------------------------------------------------------------------
# Purpose:  Draw volcano with ggplot2
# Author:   Feiyang Niu
# Date:     March 6, 2017
#-----------------------------------------------------------------------------


# load required r scripts
use_package('ggplot2')
# using is_blank(), ternary(), intersection()
source('r_scripts/common_statistics.R')
source('r_scripts/plot_utils.R')


#' Draw volcano plot
#'
#' Function that generates
#' \href{https://en.wikipedia.org/wiki/Volcano_plot_(statistics)}{volcano plot}
#' with ggplot2 package.
#'
#' @param data Data frame: default dataset to use for plot. If not already a
#' data.frame, will be converted to one by fortify.
#' @param x_var Character: name of a data column mapped to x-axis
#' @param y_var Character: name of a data column mapped to y-axis
#' @param color_var Character: name of a data column mapped to color scale.
#'                  Default is set to `NULL`
#' @param fill_var Character: name of a data column mapped to fill scale.
#'                 Default is set to `NULL`
#' @param shape_var Character: name of a data column mapped to shape scale.
#'                  Default is set to `NULL`
#' @param add_label Logical: whether or not to add labels for the points.
#'                  Default is set to `FALSE`
#' @param label_var Character: name of a data column mapped to point labels
#'                  if `add_label == TRUE`. Default is set to `NULL`
#' @param label_xlim Numeric vector of length 2: only label those points whose
#'                   x coordinate falls into `label_xlim`. Default is set to
#'                   `(-Inf, Inf)` so that all the points are to be labelled.
#'                   Note that `label_xlim` should always be in ascending order
#' @param label_xloc Character: location of the points to be labelled along
#'                   x-axis. Must be either `'middle'` or `'sides'` with
#'                   `'middle'` indicating points between `label_xlim` while
#'                   `'sides'` indicating points outside `'label_xlim'`. Default
#'                   is set to `'middle'`
#' @param label_yloc Character: location of the points to be labelled along
#'                   y-axis. Must be either `'middle'` or `'sides'` with
#'                   `'middle'` indicating points between `label_ylim` while
#'                   `'sides'` indicating points outside `'label_ylim'`. Default
#'                   is set to `'middle'`
#' @param label_ylim Numeric vector of length 2: only label those points whose
#'                   y coordinate falls into `label_ylim`. Default is set to
#'                   `(-Inf, Inf)` so that all the points are to be labelled,
#'                   Note that `label_ylim` should always be in ascending order
#' @param x_log Logical: whether or not to use log scale for x-axis. Default is
#'              set to `TRUE`
#' @param y_log Logical: whether or not to use log scale for y-axis. Default is
#'              set to `TRUE`
#' @param x_lab Character: x-axis label. Default is set to `x_var`
#' @param y_lab Character: y-axis label. Default is set to `y_var`
#' @param title Character: plot title. Default is set of empty string
#' @param x_reverse Logical: whether or not to make an x-axis in a descending
#'                  order. Default is set to `FALSE`
#' @param y_reverse Logical: whether or not to make a y-axis in a descending
#'                  order. Default is set to `TRUE`
#' @param x_limit Numeric vector of length 2: x-axis limit. Default is set to
#'                `NULL`. Note that `x_limit` should always be in ascending
#'                order
#' @param y_limit Numeric vector of length 2: y-axis limit. Default is set to
#'                `NULL`. Note that `y_limit` should always be in ascending
#'                order
#' @param x_axis_breaks Numeric vector: positions of x-axis ticks. Default is
#'                      set to `NULL`. Note that `x_axis_breaks` should always
#'                      be in ascending order
#' @param y_axis_breaks Numeric vector: positions of y-axis ticks. Default is
#'                      set to `NULL`. Note that `y_axis_breaks` should always
#'                      be in ascending order
#' @param x_axis_labels Character vector: labels of x-axis ticks. Default is
#'                      set to be the same as x_axis_breaks. Note that
#'                      `x_axis_labels` should always be in ascending order
#' @param y_axis_labels Character vector: labels of y-axis ticks. Default is
#'                      set to be the same as y_axis_breaks. Note that
#'                      `y_axis_labels` should always be in ascending order
#' @param add_legend Logical: whether or not to add legend. Default is set to
#'                   `TRUE`
#' @param legend_pos Character/Numeric vector: the position of legends. ("left",
#'                   "right", "bottom", "top", or two-element numeric vector)
#' @param reference_hline Numeric vector: the position(s) of horizontal
#'                        reference line(s)
#' @param reference_vline Numeric vector: the position(s) of vertical
#'                        reference line(s)
#' @param add_x_axis_text Logical: whether or not to add helper text for x-axis.
#'                       Default is set to `TRUE`
#' @param x_axis_text Character vector: content of the helper text for x-axis.
#'                   Default is to show: "<- Favors Treatment" (x = -0.5);
#'                   "Favors Placebo ->" (x = 0.5)
#' @param x_axis_text_pos Numeric vector: x-axis coordinates of the helper text
#'                       right above x-axis. Default is set to `NULL`
#' @param x_axis_text_align Character/Numeric: position alignment of the helper
#'                         text for x-axis. If character, be one of `c('left',
#'                         'center', 'right')`. If numeric, be between [0, 1]
#'                         with 0 being left-justified, 0.5 being
#'                         center-justified, and 1 being right-justified.
#'                         Default is set to 'center'
#' @param add_y_axis_text Logical: whether or not to add helper text for y-axis.
#'                       Default is set to `FALSE`
#' @param y_axis_text Character vector: content of the helper text for y-axis.
#'                   Default is set to `NULL`
#' @param y_axis_text_pos Numeric vector: y-axis coordinates of the helper text
#'                       right beside y-axis. Default is set to `NULL`
#' @param y_axis_text_align Character/Numeric: position alignment of the helper
#'                         text for y-axis. If character, be one of `c('left',
#'                         'center', 'right')`. If numeric, be between [0, 1]
#'                         with 0 being left-justified, 0.5 being
#'                         center-justified, and 1 being right-justified.
#'                         Default is set to 'center'
#' @param add_canvas_text Logical: whether or not to add helper text in the
#'                        plot. Default is set to `FALSE`
#' @param canvas_text Character vector: content of the helper text in the plot.
#'                    Default is set to `NULL`
#' @param canvas_text_pos Character/Numeric vector: x-&y-axis coordinates of
#'                        the helper text to show in the plot. If character, be
#'                        one of `c('topleft', 'topright', 'bottomleft',
#'                        'bottomright')`. If numeric, be a numeric vector of
#'                        length 2, e.g. `(x_coordinate, y_coordinate)`.
#'                        Default is set to 'topleft
#' @param canvas_text_align Character/Numeric vector: position alignment of the
#'                          helper text in the plot. If character, be vector of
#'                          length two with each component being one of
#'                          `c('left', 'center', 'right')`. If numeric, be
#'                          vector of length two with each component being
#'                          between [0, 1] where 0 is left-justified, 0.5
#'                          center-justified, and 1 is right-justified
#' @param grids Character: grids option. Must be one of `c('on', 'major',
#'              'off')` with 'on' having both major and minor grids, 'major'
#'              having only major grids, and 'off' having no grids
gg_volcano <- function(data, x_var, y_var, color_var = NULL, fill_var = NULL,
                       shape_var = NULL, add_label = FALSE, label_var = NULL,
                       repel_label = FALSE, label_size = 3, 
                       label_xlim = c(-Inf, Inf), label_ylim = c(-Inf, Inf),
                       label_xloc = 'middle', label_yloc = 'middle',
                       x_log = TRUE, y_log = TRUE,
                       x_lab = x_var, y_lab = y_var, title = '',
                       x_reverse = FALSE, y_reverse = TRUE,
                       x_limit = NULL, y_limit = NULL,
                       x_axis_breaks = NULL, y_axis_breaks = NULL,
                       x_axis_labels = x_axis_breaks,
                       y_axis_labels = y_axis_breaks,
                       add_legend = TRUE, legend_pos = 'bottom',
                       legend_font_size = 10,
                       reference_hline = NULL, reference_vline = NULL,
                       add_x_axis_text = TRUE,
                       x_axis_text = c(sprintf('\u2190 Favors Treatment'),
                                      sprintf('Favors Placebo \u2192')),
                       x_axis_text_pos = NULL, x_axis_text_align = 'center',
                       add_y_axis_text = FALSE, y_axis_text = NULL,
                       y_axis_text_pos = NULL, y_axis_text_align = 'center',
                       add_canvas_text = FALSE, canvas_text = NULL,
                       canvas_text_pos = 'topleft', canvas_text_align = NULL,
                       grids = 'off') {

    #---------------------------
    # argument match
    #---------------------------
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
    add_label <- isTRUE(add_label)
    repel_label <- isTRUE(repel_label)
    x_log <- isTRUE(x_log)
    y_log <- isTRUE(y_log)
    x_reverse <- isTRUE(x_reverse)
    y_reverse <- isTRUE(y_reverse)
    add_legend <- isTRUE(add_legend)
    if(is.character(legend_pos))
        legend_pos <- match.arg(tolower(legend_pos),
                                c('left', 'right', 'bottom', 'top'))
    add_x_axis_text <- isTRUE(add_x_axis_text)
    add_y_axis_text <- isTRUE(add_y_axis_text)
    add_canvas_text <- isTRUE(add_canvas_text)


    #---------------------------
    # define constants
    #---------------------------
    alignment_dict <- list(
        'left' = 0, 'center' = 0.5, 'right' = 1
    )
    position_dict <- list(
        'topleft' = c(0, 1), 'topright' = c(1, 1),
        'bottomleft' = c(0, 0), 'bottomright' = c(1, 0)
    )
    
    
    #---------------------------
    # data manipulation
    #---------------------------
    if(x_reverse && !is.null(x_limit))
        x_limit <- sort(x_limit, decreasing = TRUE)
    if(y_reverse && !is.null(y_limit))
        y_limit <- sort(y_limit, decreasing = TRUE)
    if(x_reverse && !is.null(x_axis_breaks))
        x_axis_breaks <- sort(x_axis_breaks, decreasing = TRUE)
    if(y_reverse && !is.null(y_axis_breaks))
        y_axis_breaks <- sort(y_axis_breaks, decreasing = TRUE)
    if(x_reverse && !is.null(x_axis_labels))
        x_axis_labels <- sort(x_axis_labels, decreasing = TRUE)
    if(y_reverse && !is.null(y_axis_labels))
        y_axis_labels <- sort(y_axis_labels, decreasing = TRUE)
    if(!is_blank(shape_var)) {
        num_shapes <- length(unique(na.omit(data[[shape_var]])))
        all_shapes <- c(0:25, 32:255)[seq_len(num_shapes)]
    } else all_shapes <- NULL

    #---------------------------
    # make the plot
    #---------------------------
    plot_ <- gg_wrapper(
        data = data,
        aes_string(x = paste0('`', x_var, '`'), y = paste0('`', y_var, '`')),
        is_x_continuous = TRUE, is_y_continuous = TRUE,
        x_lab = x_lab, y_lab = y_lab, title = title,
        x_limit = x_limit, y_limit = y_limit,
        x_log = x_log, y_log = y_log,
        x_reverse = x_reverse, y_reverse = y_reverse,
        x_axis_breaks = x_axis_breaks, y_axis_breaks = y_axis_breaks,
        x_axis_labels = x_axis_labels, y_axis_labels = y_axis_labels,
        add_legend = add_legend, legend_pos = legend_pos,
        legend_font_size = legend_font_size,
        color_var = color_var, fill_var = fill_var,
        shape_var = shape_var, all_shapes = all_shapes,
        reference_hline = reference_hline, reference_vline = reference_vline,
        grids = grids
    )

    # add points
    plot_ <- plot_ + geom_point()
    
    # add labels
    data_label <- data
    if(!is_blank(label_xlim) && all(is.numeric(label_xlim))) {
        if(!label_xloc %in% c('middle', 'sides')) {
            stop('label_xloc must be either "middle" or "sides"')
        }
        if(label_xloc == 'middle') {
            cond_x <- data_label[[x_var]] >= label_xlim[1] &
                data_label[[x_var]] <= label_xlim[2]
        } else {
            cond_x <- data_label[[x_var]] <= label_xlim[1] |
                data_label[[x_var]] >= label_xlim[2]
        }
        data_label <- data_label[cond_x, , drop = FALSE]
    }
    if(!is_blank(label_ylim) && all(is.numeric(label_ylim))) {
        if(!label_yloc %in% c('middle', 'sides')) {
            stop('label_yloc must be either "middle" or "sides"')
        }
        if(label_yloc == 'middle') {
            cond_y <- data_label[[y_var]] >= label_ylim[1] &
                data_label[[y_var]] <= label_ylim[2]
        } else {
            cond_y <- data_label[[y_var]] <= label_ylim[1] |
                data_label[[y_var]] >= label_ylim[2]
        }
        data_label <- data_label[cond_y, , drop = FALSE]
    }
    if(add_label && !is_blank(label_var)) {
        if(repel_label) {
            plot_ <- plot_ +
                ggrepel::geom_text_repel(
                    data = data_label,
                    aes_string(label = paste0('`', label_var, '`')),
                    size = label_size, show.legend = FALSE
                )
        } else {
            plot_ <- plot_ +
                geom_text(data = data_label,
                          aes_string(label = paste0('`', label_var, '`')),
                          hjust = 'inward', vjust = 0.5, size = label_size,
                          show.legend = FALSE)
        }
    }
    
    
    # add annotations
    has_x_axis_text <- add_x_axis_text && !is_blank(x_axis_text)
    has_y_axis_text <- add_y_axis_text && !is_blank(y_axis_text)
    has_canvas_text <- add_canvas_text &&
        !is_blank(canvas_text) &&
        !is_blank(canvas_text_pos)
    if(has_x_axis_text || has_y_axis_text || has_canvas_text) {
        if(!is_blank(x_limit)) {
            plot_range_x <- sort(ternary(x_log, log(x_limit, 10), x_limit))
        } else {
            plot_range_x <- ggplot_build(plot_)$layout$panel_ranges[[1]]$x.range
            if(x_reverse) plot_range_x <- -rev(plot_range_x)
        }
        if(!is_blank(y_limit)) {
            plot_range_y <- sort(ternary(y_log, log(y_limit, 10), y_limit))
        } else {
            plot_range_y <- ggplot_build(plot_)$layout$panel_ranges[[1]]$y.range
            if(y_reverse) plot_range_y <- -rev(plot_range_y)
        }
        if(x_reverse) {
            min_x <- plot_range_x[2]; max_x <- plot_range_x[1]
        } else {
            min_x <- plot_range_x[1]; max_x <- plot_range_x[2]
        }
        if(y_reverse) {
            min_y <- plot_range_y[2]; max_y <- plot_range_y[1]
        } else {
            min_y <- plot_range_y[1]; max_y <- plot_range_y[2]
        }
    }
    if(has_x_axis_text) {
        if(is_blank(x_axis_text_pos)) {
            text_pos <- seq(min_x, max_x, length.out = 2 + length(x_axis_text))
            x_axis_text_pos <- text_pos[2:(length(text_pos) - 1)]
            if(x_log) x_axis_text_pos <- 10^x_axis_text_pos
        }
        if(is.character(x_axis_text_align)) {
            if(!x_axis_text_align %in% c('left', 'center', 'right')) {
                stop(paste0('x_axis_text_align must be one of `c("left",',
                            ' "center", "right")`'))
            }
            x_axis_text_align <- alignment_dict[[x_axis_text_align]]
        } else if(is.numeric(x_axis_text_align)) {
            if(x_axis_text_align > 1 || x_axis_text_align < 0) {
                stop('x_axis_text_align must be between [0, 1]')
            }
        } else {
            stop('x_axis_text_align must be either character or numeric')
        }
        x_axis_text_y <- min_y
        if(y_log) x_axis_text_y <- 10^x_axis_text_y
        plot_ <- plot_ + annotate(
            'text', x = x_axis_text_pos, y = x_axis_text_y,
            label = x_axis_text, hjust = x_axis_text_align
        )
    }
    if(has_y_axis_text) {
        if(is_blank(y_axis_text_pos)) {
            text_pos <- seq(min_y, max_y, length.out = 2 + length(y_axis_text))
            y_axis_text_pos <- text_pos[2:(length(text_pos) - 1)]
            if(y_log) y_axis_text_pos <- 10^y_axis_text_pos
        }
        if(is.character(y_axis_text_align)) {
            if(!y_axis_text_align %in% c('left', 'center', 'right')) {
                stop(paste0('y_axis_text_align must be one of `c("left",',
                            ' "center", "right")`'))
            }
            y_axis_text_align <- alignment_dict[[y_axis_text_align]]
        } else if(is.numeric(y_axis_text_align)) {
            if(y_axis_text_align > 1 || y_axis_text_align < 0) {
                stop('y_axis_text_align must be between [0, 1]')
            }
        } else {
            stop('y_axis_text_align must be either character or numeric')
        }
        y_axis_text_x <- min_x
        if(x_log) y_axis_text_x <- 10^y_axis_text_x
        plot_ <- plot_ + annotate(
            'text', x = y_axis_text_x, y = y_axis_text_pos,
            label = y_axis_text, vjust = y_axis_text_align, angle = 90
        )
    }
    if(has_canvas_text) {
        if(is.character(canvas_text_pos)) {
            if(!canvas_text_pos %in% names(position_dict)) {
                stop(paste0('canvas_text_pos must be one of `c("topleft",',
                            ' "topright", "bottomleft", "bottomright")`'))
            }
            if(is_blank(canvas_text_align))
                canvas_text_align <- position_dict[[canvas_text_pos]]
            if(canvas_text_pos == 'topleft')
                canvas_text_pos <- c(min_x, max_y)
            else if(canvas_text_pos == 'topright')
                canvas_text_pos <- c(max_x, max_y)
            else if(canvas_text_pos == 'bottomleft')
                canvas_text_pos <- c(min_x, min_y)
            else canvas_text_pos <- c(max_x, min_y)
            if(x_log) canvas_text_pos[1] <- 10^canvas_text_pos[1]
            if(y_log) canvas_text_pos[2] <- 10^canvas_text_pos[2]
        } else if(!is.numeric(canvas_text_pos)) {
            stop('canvas_text_pos must be either character or numeric')
        }
        if(all(is.character(canvas_text_align))) {
            if(!all(canvas_text_align %in% c('left', 'center', 'right'))) {
                stop(paste0('canvas_text_align must be a vector of length two',
                            ' with both components being one of `("left", ',
                            '"center", "right")`'))
            }
            canvas_text_align <- c(
                alignment_dict[[canvas_text_align[1]]],
                alignment_dict[[canvas_text_align[2]]]
            )
        } else if(all(is.numeric(canvas_text_align))) {
            if(!all(canvas_text_align >= 0 & canvas_text_align <= 1)) {
                stop(paste0('canvas_text_align must be a vector of length two',
                            ' with both components being between [0, 1]'))
            }
        } else {
            stop(paste0('canvas_text_align must be a character/numeric vector',
                        'of length two'))
        }
        plot_ <- plot_ + annotate(
            'text', x = canvas_text_pos[1], y = canvas_text_pos[2],
            label = canvas_text, hjust = canvas_text_align[1],
            vjust = canvas_text_align[2]
        )
    }

    return(plot_)
}



































