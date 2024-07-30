#-------------------------------------------------------------------------------
#   Produce duration plot using ggplot2
#
#   Author: Feiyang Niu
#   Date:   April 25, 2017
#-------------------------------------------------------------------------------



# load libraries and r scripts
use_package('ggplot2')
use_package('dplyr')
source('r_scripts/function_utils.R')
source('r_scripts/plot_utils.R')


#' Gantt chart
#' 
#' Creates a \href{https://en.wikipedia.org/wiki/Gantt_chart}{Gantt chart} with
#' horizontal or vertical lines
#' 
#' This function relies on \href{http://ggplot2.org/}{ggplot2} package to
#' create a Gantt chart with vertical or horizontal lines. The function supports
#' panel plot layout. Points can be added at the start and end of the lines
#' and the point shape specification is supported through \code{status} and
#' \code{point_shape_map} arguments together. Refer to the description of the
#' two arguments for more details
#' 
#' @param data Data frame: default dataset to use for plot
#' @param var Character: name of a \code{data} column mapped to events/projects
#' @param var_levels Character vector: levels of \code{var}, i.e. unique names
#'  of all the events/projects
#' @param time Character: name of a \code{data} column mapped to the start and
#'  end dates of an event/project. For each event/project, both start and end
#'  dates should be present in order to draw a line segment, representing the
#'  duration of the event/project
#' @param status Character: name of a \code{data} column mapped to the status
#'  of the date in the same row. This argument, together with
#'  \code{point_shape_map}, is used to add points and specify the shape of the
#'  points at the start and end dates of each event/project. The column of
#'  \code{status} contains at most three status of an event/project: start,
#'  end, and ongoing. Custom phrasing of these three status are permitted as
#'  long as \code{\link[base]{names}} of \code{point_shape_map}, if not
#'  \code{NULL}, match those status phrases.
#' @param group Character: name of a \code{data} column mapped to the color of
#'  the lines
#' @param facet_r Character: name of a \code{data} column mapped to the facet
#'  row in panel plot layout. Check \code{\link[ggplot2]{facet_grid}} for more
#'  details
#' @param facet_c Character: name of a \code{data} column mapped to the facet
#'  column in panel plot layout. Check \code{\link[ggplot2]{facet_grid}} for
#'  more details
#' @param facet_scale Character: Are scales shared across all facets. Refer to
#'  the `scale` argument in \code{\link[ggplot2]{facet_grid}}. Default `free`
#'  means that scales are not shared
#' @param facet_space Character: Refer to the `space` argument in
#'  \code{\link[ggplot2]{facet_grid}}. Default `free` means both height and
#'  width will vary
#' @param x_lab Character: x-axis label
#' @param y_lab Character: y-axis label
#' @param title Character: barplot title
#' @param label_align Character: alignment of the event/project labels. If
#'  `left` (default), labels will be left-aligned; if `center`, labels will be
#'  center-aligned; if `right`, labels will be right-aligned
#' @param bar_label_angle Character: the orientation angle (in [0, 360]) of the
#'  bar labels on the axis. By default, the label will be perpendicular to the
#'  axis
#' @param label_angle Character: the orientation angle (in [0, 360]) of the
#'  event/project labels on the axis. By default, the label will be
#'  perpendicular to the axis
#' @param grids Character: control the grids. If `on` (default), grids will be
#'  drawn; if `x`, only grids on x-axis will be drawn; if `y`, only grids on
#'  y-axis will be drawn; if `off`, no grids will be drawn
#' @param bw_theme Logical: If \code{TRUE} (default), black-and-white theme
#'  will be used. Refer to \code{\link[ggplot2]{theme_bw}} for more details
#' @param horizontal Logical: If \code{TRUE} (default), horizontal gantt chart
#'  is drawn; if `FALSE`, vertical version is drawn instead
#' @param point_shape_map List/Character vector/Numeric vector: specify points
#'  shape mapping to \code{status} column. If unnamed, levels of \code{status}
#'  are used as the names. Names of the list/vector must match the unique
#'  values of \code{status} column. Values of the list/vector must be valid
#'  shape representation. Refer to
#'  \href{http://sape.inf.usi.ch/quick-reference/ggplot2/shape}{ggplot2 Quick Reference: shape}
#'  for more details
#' @param point_size Numeric: size of the points. Must be positive value
#' @param point_legend Logical: If \code{TRUE} (default), legend of point shape
#'  is included; otherwise, omitted
#' 
#' @return An object of class ggplot. Can be directly sent to plot with
#'  \code{\link{print}}
#'  
#' @examples
#' test_df <- data.frame(
#'     project = rep(paste('Project', 1:10), each = 2),
#'     time = c(rbind(sample(1:5, 10, replace = TRUE),
#'                    sample(6:8, 10, replace = TRUE))),
#'     status = factor(c(rbind(rep('start', 10),
#'                             c(rep('end', 5), rep('ongoing', 5)))),
#'                     levels = c('start', 'ongoing', 'end'))
#' )
#' windows(); gg_gantt_chart(test_df, var = 'project', time = 'time', status = 'status', grids = 'y')
#' windows(); gg_gantt_chart(test_df, var = 'project', time = 'time', status = 'status', point_legend = TRUE)
#' 
#' @export
#'  
#' @author Feiyang Niu (Feiyang.Niu@gilead.com)
gg_gantt_chart <- function(data, var, var_levels = NULL, time, status = NULL,
                           group = NULL, facet_r = NULL, facet_c = NULL,
                           facet_scale = 'free', facet_space = 'free',
                           x_lab = NULL, y_lab = NULL, title = NULL,
                           label_align = 'left', label_angle = NULL,
                           grids = 'on', bw_theme = TRUE, horizontal = TRUE,
                           point_shape_map = if(horizontal) {
                               list('start' = 15, 'end' = 16, 'ongoing' = 45)
                           } else {
                               list('start' = 15, 'end' = 16, 'ongoing' = 45)
                           },
                           point_size = 3, point_legend = FALSE) {
    
    #---------------------------
    # argument match
    #---------------------------
    check_var_class(data, is.data.frame)
    column_in_dataframe(data, var)
    column_in_dataframe(data, time)
    if(!is_blank(status)) column_in_dataframe(data, status)
    if(!is_blank(group)) column_in_dataframe(data, group)
    if(!is_blank(facet_r)) column_in_dataframe(data, facet_r)
    if(!is_blank(facet_c)) column_in_dataframe(data, facet_c)
    arg_in_choices(facet_scale, c('free', 'free_x', 'free_y', 'fixed'))
    arg_in_choices(facet_space, c('free', 'free_x', 'free_y', 'fixed'))
    arg_in_choices(label_align, c('right', 'center', 'left'))
    arg_in_choices(grids, c('on', 'x', 'y', 'off'))
    bw_theme <- isTRUE(bw_theme)
    horizontal <- isTRUE(horizontal)
    point_legend <- isTRUE(point_legend)
    if(!is.null(label_angle)) check_var_class(label_angle, is.numeric)
    if(!is_blank(status)) {
        if(!is.factor(data[[status]])) data[[status]] <- factor(data[[status]])
        status_levels <- levels(data[[status]])
        if(length(point_shape_map) < length(status_levels)) {
            all_shapes <- (0:25)[seq_along(status_levels)]
        }
        if(is.null(names(point_shape_map)))
            names(point_shape_map) <- status_levels
        else if(!all(status_levels %in% names(point_shape_map))) {
            stop(paste('Names of `point_shape_map` must match the unique',
                       'values in `status` column'))
        }
        shape_values <- unname(unlist(point_shape_map[status_levels]))
    }
        
    
    #---------------------------
    # data manipulation
    #---------------------------
    
    # sort the data
    group_by_vars <- c()
    if(!is_blank(facet_r)) group_by_vars <- c(group_by_vars, facet_r)
    if(!is_blank(facet_c)) group_by_vars <- c(group_by_vars, facet_c)
    if(!is_blank(group)) group_by_vars <- c(group_by_vars, group)
    group_by_vars <- c(group_by_vars, var)
    group_dots <- lapply(group_by_vars, as.symbol)
    data <- group_by_(data, .dots = group_dots)
    
    # make both var and group a factor
    if(!is_blank(var_levels)) unique_var <- var_levels
    else unique_var <- unique(na.omit(data[[var]]))
    if(horizontal) unique_var <- rev(unique_var)
    data[[var]] <- factor(data[[var]], levels = unique_var)
    if(!is_blank(group)) data[[group]] <- factor(data[[group]])
    
    if(is.null(x_lab)) x_lab <- ifelse(horizontal, 'Duration', var)
    if(is.null(y_lab)) y_lab <- ifelse(horizontal, var, 'Duration')
    if(is.null(title)) title <- ''
    
    x_ <- paste0('`', ifelse(horizontal, time, var), '`')
    y_ <- paste0('`', ifelse(horizontal, var, time), '`')
    
    
    #---------------------------
    # make the plot
    #---------------------------
    plot_ <- gg_wrapper(
        data = data, aes_string(x = x_, y = y_),
        facet_r = facet_r, facet_c = facet_c,
        facet_scale = facet_scale, facet_space = facet_space,
        x_lab = x_lab, y_lab = y_lab, title = title,
        color_var = group, bw_theme = bw_theme, grids = grids
    )
    plot_ <- plot_ + geom_line(aes_string(group = var))
    if(!is_blank(status)) {
        plot_ <- plot_ +
            geom_point(aes_string(shape = status), size = point_size,
                       show.legend = point_legend) +
            scale_shape_manual(name = '', values = shape_values)
    }
    
    if(!is.null(label_angle)) angle <- label_angle
    else angle <- ifelse(horizontal, 0, 90)
    adjust <- list('right' = 1, 'center' = 0.5, 'left' = 0)[[label_align]]
    if(horizontal) {
        plot_ <- plot_ +
            theme(axis.text.y = element_text(angle = angle, hjust = adjust))
    } else {
        plot_ <- plot_ +
            theme(axis.text.x = element_text(angle = angle,
                                             hjust = adjust,
                                             vjust = 0.5))
    }
    
    return(plot_)
    
}


































