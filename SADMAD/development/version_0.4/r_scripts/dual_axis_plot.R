#-----------------------------------------------------------------------------
# Purpose:  Draw dual y axis plot with base R graphic
# Author:   Feiyang Niu
# Date:     September 23, 2016
#-----------------------------------------------------------------------------


# load required r scripts
use_package('ggplot2')
use_package('grid')
use_package('gtable')
use_package('dplyr')
use_package('lazyeval')
source('r_scripts/common_statistics.R')
source('r_scripts/plot_utils.R')


dual_y_axis_sumline <- function(data, var_x, var_y1, var_y2, group = NULL,
                                xlab = var_x, ylab1 = var_y1, ylab2 = var_y2,
                                xlog = FALSE, ylog1 = FALSE, ylog2 = FALSE,
                                title = '', footnote = '', fnote_size = 0.8,
                                method = NULL, type = 'p', color1 = NULL,
                                color2 = NULL, xlim = NULL, ylim = NULL,
                                same_y_axis = FALSE, xaxis = TRUE, x_tick = NULL,
                                x_tick_lab = NULL, mar = NULL, legend_txt = NULL,
                                legend_pos = NULL, save_plot = FALSE) {
    
    xlog <- isTRUE(xlog)
    ylog1 <- isTRUE(ylog1)
    ylog2 <- isTRUE(ylog2)
    same_y_axis <- isTRUE(same_y_axis)
    save_plot <- isTRUE(save_plot)
    if(is_blank(color1)) color1 <- 'red4'
    if(is_blank(color2)) color2 <- 'blue4'
    
    missing_x <- is.na(data[[var_x]])
    missing_y1 <- is.na(data[[var_y1]])
    missing_y2 <- is.na(data[[var_y2]])
    data <- data[!missing_x & (!missing_y1 | !missing_y2), , drop = FALSE]
    
    if(same_y_axis) stopifnot(!is_blank(var_y2))
    has_group <- !is_blank(group)
    if(!has_group) data[['group']] <- factor(as.character(1))
    else data[['group']] <- factor(data[[group]])
    ngroups <- length(unique(data[['group']]))
    all_levels <- levels(data[['group']])
    has_method <- !is_blank(method)
    if(has_method) {
        if(method == 'mean_se' || method == 'mean_sd') {
            if(method == 'mean_se') error_func <- std_err
            else error_func <- sd_na
            expr <- list(
                interp(~mean_na(var), var = as.name(var_y1)),
                interp(~(mean_na(var) - error_func(var)), var = as.name(var_y1)),
                interp(~(mean_na(var) + error_func(var)), var = as.name(var_y1))
            )
            if(!is_blank(var_y2)) {
                expr <- append(expr, list(
                    interp(~mean_na(var), var = as.name(var_y2)),
                    interp(~(mean_na(var) - error_func(var)), var = as.name(var_y2)),
                    interp(~(mean_na(var) + error_func(var)), var = as.name(var_y2))
                ))
            }
        } else if(method == 'median_iqr') {
            expr <- list(
                interp(~median_na(var), var = as.name(var_y1)),
                interp(~q1_na(var), var = as.name(var_y1)),
                interp(~q3_na(var), var = as.name(var_y1))
            )
            if(!is_blank(var_y2)) {
                expr <- append(expr, list(
                    interp(~median_na(var), var = as.name(var_y2)),
                    interp(~q1_na(var), var = as.name(var_y2)),
                    interp(~q3_na(var), var = as.name(var_y2))
                ))
            }
        }
        if(!is_blank(var_y2)) {
            dots <- setNames(
                expr,
                c('var_y1', 'lower1', 'upper1', 'var_y2', 'lower2', 'upper2')
            )
        } else dots <- setNames(expr, c('var_y1', 'lower1', 'upper1'))
        data <- data %>% arrange_('group', var_x) %>% group_by_('group', var_x)
        data <- data %>% summarise_(.dots = dots)
    }
    
    data[['plot_x']] <- data[[var_x]]
    if(!is.numeric(data[[var_x]])) {
        data[['plot_x']] <- as.numeric(as.factor(data[[var_x]]))
    }
    unique_x <- !duplicated(data[['plot_x']])
    order_x <- order(data[['plot_x']][unique_x])
    if(is.null(x_tick))
        x_tick <- data[['plot_x']][unique_x][order_x]
    if(is.null(x_tick_lab))
        x_tick_lab <- data[[var_x]][unique_x][order_x]
    xrange <- range(data[['plot_x']], na.rm = TRUE)
    min_xleap <- min(diff(sort(unique(data[['plot_x']]))), na.rm = TRUE)
    error_bar_width <- min(diff(xrange) / 120, min_xleap / (4 * (2 * ngroups - 1)))
    if(is.null(xlim)) {
        xlim <- xrange + c(-(2 * ngroups - 1), 2 * ngroups - 1) * error_bar_width
    }
    if(has_method) {
        if(is.null(ylim)) {
            ylim1 <- range_na(data[['var_y1']], data[['lower1']], data[['upper1']])
            ylim2 <- range_na(data[['var_y2']], data[['lower2']], data[['upper2']])
        } else {
            ylim1 <- ylim2 <- ylim
        }
    } else {
        if(is.null(ylim)) {
            ylim1 <- range_na(data[[var_y1]])
            ylim2 <- range_na(data[[var_y2]])
        } else {
            ylim1 <- ylim2 <- ylim
        }
    }
    if(same_y_axis) ylim1 <- ylim2 <- range_na(ylim1, ylim2)
    log_opt1 <- ''
    log_opt2 <- ''
    if(xlog) {
        log_opt1 <- paste0(log_opt1, 'x')
        log_opt2 <- paste0(log_opt2, 'x')
    }
    if(ylog1) log_opt1 <- paste0(log_opt1, 'y')
    if(ylog2) log_opt2 <- paste0(log_opt2, 'y')
    if(is.null(mar)) mar = c(5.1, 4.1, 4.1, 4.1)
    fnote_space <- 0.8
    if(is_blank(footnote)) {
        par_opt <- par(mar = mar)
    } else {
        fnote_lines <- length(strsplit(trimws(footnote), '\n')[[1]])
        par_opt <- par(mar = mar,
                       oma = c(fnote_space + fnote_lines*fnote_size + 0.2,0,0,0))
    }
    
    if(save_plot) {
        win.metafile()
        dev.control('enable')
    }
    
    colorfunc1 <- colorRampPalette(c(color1, 'white'))
    colorfunc2 <- colorRampPalette(c(color2, 'white'))
    colors1 <- colorfunc1(3 * ngroups)
    colors2 <- colorfunc2(3 * ngroups)
    plot(NA, NA, xlim = xlim, ylim = ylim1, log = log_opt1, xlab = xlab,
         ylab = NA, main = title, xaxt = 'n', yaxt = 'n', type = 'n')
    for(idx in seq_len(ngroups)) {
        data_idx <- data[data[['group']] == all_levels[idx], , drop = F]
        x1_idx <- data_idx[['plot_x']] - (2 * idx - 1) * error_bar_width
        order_x1 <- order(x1_idx)
        col1 <- colors1[1 + (idx - 1) * ngroups]
        if(has_method) {
            y1_idx <- data_idx[['var_y1']]
            lower_idx <- data_idx[['lower1']]
            upper_idx <- data_idx[['upper1']]
            segments(x1_idx, lower_idx, x1_idx, upper_idx, col = col1)
            segments(x1_idx - error_bar_width, lower_idx,
                     x1_idx + error_bar_width, lower_idx, col = col1)
            segments(x1_idx - error_bar_width, upper_idx,
                     x1_idx + error_bar_width, upper_idx, col = col1)
        } else {
            y1_idx <- data_idx[[var_y1]]
        }
        lines(x1_idx[order_x1], y1_idx[order_x1], type = type, col = col1)
        if(isTRUE(xaxis)) {
            axis(1, at = x_tick, labels = FALSE)
            mtext(side = 1, text = x_tick_lab, at = x_tick, line = 1)
        }
        axis(2, labels = FALSE)
        at1 = axTicks(2)
        mtext(side = 2, text = at1, at = at1, col = color1, line = 1)
        mtext(side = 2, text = ylab1, line = 3, col = color1)
    }
    if(!is.null(legend_txt)) {
        if(is.null(legend_pos)) legend_pos <- 'top'
        legend(legend_pos, legend = legend_txt, bty = 'n')
    }
    
    par(new = TRUE)
    plot(NA, NA, xlim = xlim, ylim = ylim2, log = log_opt2, xlab = NA,
         ylab = NA, axes = FALSE, type = 'n')
    for(idx in seq_len(ngroups)) {
        data_idx <- data[data[['group']] == all_levels[idx], , drop = F]
        x2_idx <- data_idx[['plot_x']] + (2 * idx - 1) * error_bar_width
        order_x2 <- order(x2_idx)
        col2 <- colors2[1 + (idx - 1) * ngroups]
        if(has_method) {
            y2_idx <- data_idx[['var_y2']]
            lower_idx <- data_idx[['lower2']]
            upper_idx <- data_idx[['upper2']]
            segments(x2_idx, lower_idx, x2_idx, upper_idx, col = col2)
            segments(x2_idx - error_bar_width, lower_idx,
                     x2_idx + error_bar_width, lower_idx, col = col2)
            segments(x2_idx - error_bar_width, upper_idx,
                     x2_idx + error_bar_width, upper_idx, col = col2)
        } else {
            y2_idx <- data_idx[[var_y2]]
        }
        lines(x2_idx[order_x2], y2_idx[order_x2], type = type, col = col2)
        axis(4, labels = FALSE)
        at2 = axTicks(4)
        mtext(side = 4, text = at2, at = at2, col = color2, line = 1)
        mtext(side = 4, text = ylab2, line = 3, col = color2)
    }
    
    if(!is_blank(footnote)) {
        mtext(footnote, side = 1, adj = 0, cex = fnote_size, outer = TRUE,
              line = fnote_space + (fnote_lines - 1) * fnote_size)
    }
    
    if(save_plot) {
        the_plot <- recordPlot()
        dev.off()
        par <- par(par_opt)
        return(the_plot)
    }
    par <- par(par_opt)
}




ggplot_dual_axis = function(plot1, plot2, which.axis = "x") {
    
    # Update plot with transparent panel
    plot2 = plot2 + theme(panel.background = element_rect(fill = NA))
    
    grid.newpage()
    
    # Increase right margin if which.axis == "y"
    if(which.axis == "y") plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
    
    # Extract gtable
    g1 = ggplot_gtable(ggplot_build(plot1))
    
    g2 = ggplot_gtable(ggplot_build(plot2))
    
    # Overlap the panel of the second plot on that of the first
    pp = c(subset(g1$layout, name == "panel", se = t:r))
    
    g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
    
    # Steal axis from second plot and modify
    axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")
    
    ia = which(g2$layout$name == axis.lab)
    
    ga = g2$grobs[[ia]]
    
    ax = ga$children[[2]]
    
    # Switch position of ticks and labels
    if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
    
    ax$grobs = rev(ax$grobs)
    
    if(which.axis == "x") 
        
        ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm") else
            
            ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    
    # Modify existing row to be tall enough for axis
    if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]
    
    # Add new row or column for axis label
    if(which.axis == "x") {
        
        g = gtable_add_grob(g, ax, 2, 4, 2, 4) 
        
        g = gtable_add_rows(g, g2$heights[1], 1)
        
        g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
        
    } else {
        
        g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
        
        g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
        
        g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
        
    }
    
    # Draw it
    grid.draw(g)
    
}


























