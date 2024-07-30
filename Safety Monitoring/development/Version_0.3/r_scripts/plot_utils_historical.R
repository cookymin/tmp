#-----------------------------------------------------------------------------
# Purpose:  Utility functions for plotting
# Author:   Feiyang Niu
# Date:     April 19, 2016
#-----------------------------------------------------------------------------


# load required r scripts
use_package('ggplot2')
use_package('grid')
use_package('gridExtra')
use_package('scales')
source('r_scripts/common_statistics.R')


# Emulate ggplot2 default color palette
gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    return(hcl(h = hues, l = 65, c = 100)[1:n])
}


add_alpha <- function(color, alpha = 1) {
    #------------------------------------------------------------
    # Add an alpha value to a R color value/vector
    #
    # @Arguments:
    #   color: a scalar or vector of any of the three kinds of R
    #          color specifications:
    #               1. color name
    #               2. a hexadecimal string of the form "#rrggbb"
    #                  or "#rrggbbaa"
    #               3. a positive integer i meaning palette()[i]
    #   alpha: an alpha value between 0 and 1
    #
    #------------------------------------------------------------
    if(alpha < 0 | alpha > 1)
        stop('alpha value must be in [0, 1]')
    return(apply(unname(sapply(color, col2rgb)) / 255, 2,
                 FUN = function(x) {rgb(x[1], x[2], x[3], alpha = alpha)}))
}


# Reproduce the base graphics behaviour
base_log_breaks <- function(n = 10){
    function(x) {
        axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
}


# combine reverse and log transformations
reverselog_trans <- function(base = 10) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0('reverselog-', format(base)), trans, inv, 
              breaks = base_log_breaks(),
              domain = c(.Machine$double.xmin, Inf))
}


# check whether the obj is a valid color representation
areColors <- function(obj) {
    sapply(obj, function(X) {
        tryCatch(is.matrix(col2rgb(X)),
                 error = function(e) FALSE)
    })
}


# remove elements of a ggplot2 object
gg_remove <- function(ggplot_obj, elements_to_remove = 'none') {
    require(ggplot2)
    if(!is.ggplot(ggplot_obj)) stop('ggplot_obj must be a ggplot2 object')
    all_elements_to_remove <- c(
        'none', 'title', 'xlab', 'ylab', 'xaxis', 'yaxis', 'legend', 'grid',
        'grid.x', 'grid.y', 'xticks', 'yticks'
    )
    if(!all(elements_to_remove %in% all_elements_to_remove)) {
        stop(paste0(
            'elements_to_remove must be combination of the following:',
            '\n', paste(paste('*', all_elements_to_remove), collapse = '\n')
        ))
    }
    if(length(elements_to_remove) == 1 && elements_to_remove == 'none')
        return(ggplot_obj)
    if('title' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(plot.title = element_blank())
    }
    if('xlab' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(axis.title.x = element_blank())
    }
    if('ylab' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(axis.title.y = element_blank())
    }
    if('xaxis' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
    }
    if('yaxis' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
    }
    if('legend' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(legend.position = 'none')
    }
    if('grid' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank())
    }
    if('grid.x' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank())
    }
    if('grid.y' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj +
            theme(panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank())
    }
    if('xticks' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(axis.ticks.x = element_blank())
    }
    if('yticks' %in% elements_to_remove) {
        ggplot_obj <- ggplot_obj + theme(axis.ticks.y = element_blank())
    }
    return(ggplot_obj)
}


# wrapper function for setting up ggplot environment
gg_wrapper <- function(..., facet_r = NULL, facet_c = NULL,
                       facet_scale = 'fixed', facet_space = 'fixed',
                       is_x_continuous = TRUE, is_y_continuous = TRUE,
                       x_lab = NULL, y_lab = NULL, title = NULL,
                       x_limit = NULL, y_limit = NULL,
                       x_log = FALSE, y_log = FALSE,
                       x_reverse = FALSE, y_reverse = FALSE,
                       x_axis_breaks = NULL, y_axis_breaks = NULL,
                       x_axis_labels = x_axis_breaks,
                       y_axis_labels = y_axis_breaks,
                       xtick_angle = 0, ytick_angle = 0,
                       x_hjust = 0.5, y_hjust = 0.5,
                       add_legend = TRUE, legend_pos = 'bottom',
                       color_var = NULL, all_colors = NULL,
                       fill_var = NULL, all_fills = NULL,
                       size_var = NULL, all_sizes = NULL,
                       shape_var = NULL, all_shapes = NULL,
                       linetype_var = NULL, all_linetypes = NULL,
                       alpha_var = NULL, all_alphas = NULL,
                       reference_hline = NULL, reference_vline = NULL,
                       grids = 'on', bw_theme = TRUE,
                       strip_background_color = NULL,
                       x_expand = NULL, y_expand = NULL) {
    
    #---------------------------
    # argument match
    #---------------------------
    is_x_continuous <- isTRUE(is_x_continuous)
    is_y_continuous <- isTRUE(is_y_continuous)
    x_log <- isTRUE(x_log)
    y_log <- isTRUE(y_log)
    x_reverse <- isTRUE(x_reverse)
    y_reverse <- isTRUE(y_reverse)
    bw_theme <- isTRUE(bw_theme)
    add_legend <- isTRUE(add_legend)
    legend_pos <- match.arg(tolower(legend_pos),
                            c('left', 'right', 'bottom', 'top'))
    if(is.null(x_lab)) x_lab <- waiver()
    if(is.null(y_lab)) y_lab <- waiver()
    if(is_blank(x_limit)) x_limit <- NULL
    if(is_blank(y_limit)) y_limit <- NULL
    if(!grids %in% c('on', 'x', 'y', 'off')) {
        stop('grids must be one of `c("on", "x", "y", "off")`')
    }
    
    # `environment = parent.frame()` sets the environment to the calling env
    # so that ggplot is able to access the local variable defined in the
    # function that calls `gg_wrapper`
    base_plot <- ggplot2::ggplot(..., environment = parent.frame())
    if(bw_theme) base_plot <- base_plot + theme_bw()
    
    
    # add aesthetics - color
    if(!is_blank(color_var)) {
        base_plot <- base_plot + aes_string(colour = paste0('`', color_var, '`'))
        if(!is_blank(all_colors)) {
            if(!all(areColors(all_colors)))
                stop('all_colors must be valid color representation')
            base_plot <- base_plot + scale_colour_manual(values = all_colors)
        }
    } else {
        if(!is_blank(all_colors)) {
            if(!all(areColors(all_colors)))
                stop('all_colors must be valid color representation')
            base_plot <- base_plot + aes(colour = factor(1)) +
                scale_colour_manual(values = all_colors, guide = FALSE)
        }
    }
    
    # add aesthetics - fill
    if(!is_blank(fill_var)) {
        base_plot <- base_plot + aes_string(fill = paste0('`', fill_var, '`'))
        if(!is_blank(all_fills)) {
            if(!all(areColors(all_fills)))
                stop('all_fills must be valid color representation')
            base_plot <- base_plot + scale_fill_manual(values = all_fills)
        }
    } else {
        if(!is_blank(all_fills)) {
            if(!all(areColors(all_fills)))
                stop('all_fills must be valid color representation')
            base_plot <- base_plot + aes(fill = factor(1)) +
                scale_fill_manual(values = all_fills, guide = FALSE)
        }
    }
    
    # add aesthetics - size
    if(!is_blank(size_var)) {
        base_plot <- base_plot + aes_string(size = paste0('`', size_var, '`'))
        if(!is_blank(all_sizes)) {
            if(!all(all_sizes >= 0))
                stop('all_sizes must be >= 0')
            base_plot <- base_plot + scale_size_manual(values = all_sizes)
        }
    } else {
        if(!is_blank(all_sizes)) {
            if(!all(all_sizes >= 0))
                stop('all_sizes must be >= 0')
            base_plot <- base_plot + aes(size = factor(1)) +
                scale_size_manual(values = all_sizes, guide = FALSE)
        }
    }
    
    # add aesthetics - shape
    if(!is_blank(shape_var)) {
        base_plot <- base_plot + aes_string(shape = paste0('`', shape_var, '`'))
        if(!is_blank(all_shapes)) {
            if(!(all(all_shapes %% 1 == 0 & all_shapes >= 0)))
                stop('all_shapes must be an integer >= 0')
            base_plot <- base_plot + scale_shape_manual(values = all_shapes)
        }
    } else {
        if(!is_blank(all_shapes)) {
            if(!(all(all_shapes %% 1 == 0 & all_shapes >= 0)))
                stop('all_shapes must be an integer >= 0')
            base_plot <- base_plot + aes(shape = factor(1)) +
                scale_shape_manual(values = all_shapes, guide = FALSE)
        }
    }
    
    # add aesthetics - linetype
    if(!is_blank(linetype_var)) {
        base_plot <- base_plot +
            aes_string(linetype = paste0('`', linetype_var, '`'))
        if(!is_blank(all_linetypes)) {
            valid_linetypes <- c(
                'blank' = 0, 'solid' = 1, 'dashed' = 2,
                'dotted' = 3, 'dotdash' = 4, 'longdash' = 5,
                'twodash' = 6
            )
            if(!(all(all_linetypes %in% names(valid_linetypes)) |
                 all(all_linetypes %in% valid_linetypes)))
                stop('all_linetypes must be valid linetype value')
            base_plot <- base_plot + scale_linetype_manual(values = all_linetypes)
        }
    } else {
        if(!is_blank(all_linetypes)) {
            valid_linetypes <- c(
                'blank' = 0, 'solid' = 1, 'dashed' = 2,
                'dotted' = 3, 'dotdash' = 4, 'longdash' = 5,
                'twodash' = 6
            )
            if(!(all(all_linetypes %in% names(valid_linetypes)) |
                 all(all_linetypes %in% valid_linetypes)))
                stop('all_linetypes must be valid linetype value')
            base_plot <- base_plot + aes(linetype = factor(1)) +
                scale_linetype_manual(values = all_linetypes, guide = FALSE)
        }
    }
    
    # add aesthetics - alpha
    if(!is_blank(alpha_var)) {
        base_plot <- base_plot + aes_string(alpha = paste0('`', alpha_var, '`'))
        if(!is_blank(all_alphas)) {
            if(!(all(all_alphas >= 0 & all_alphas <= 1)))
                stop('all_alphas must be a number in [0, 1]')
            base_plot <- base_plot + scale_alpha_manual(values = all_alphas)
        }
    } else {
        if(!is_blank(all_alphas)) {
            if(!(all(all_alphas >= 0 & all_alphas <= 1)))
                stop('all_alphas must be a number in [0, 1]')
            base_plot <- base_plot + aes(alpha = factor(1)) +
                scale_alpha_manual(values = all_alphas, guide = FALSE)
        }
    }
    
    # add faceting
    if(!is_blank(facet_r) | !is_blank(facet_c)) {
        facets <- paste(ternary(is_blank(facet_r), '.', facet_r), '~',
                        ternary(is_blank(facet_c), '.', facet_c))
        base_plot <- base_plot +
            facet_grid(facets, scales = facet_scale, space = facet_space)
    }
    
    # draw x-/y-axis
    if(!is_x_continuous && x_log) {
        stop('discrete x-axis does not take log scale')
    }
    if(!is_x_continuous && x_reverse) {
        stop('discrete x-axis does not take descending order')
    }
    if(!is_y_continuous && y_log) {
        stop('discrete y-axis does not take log scale')
    }
    if(!is_y_continuous && y_reverse) {
        stop('discrete y-axis does not take descending order')
    }
    if(!x_log) {
        if(!is_blank(x_axis_breaks) || x_reverse) {
            if(is_blank(x_axis_breaks)) x_axis_breaks <- waiver()
            if(is.null(x_axis_labels)) x_axis_labels <- waiver()
            x_axis_draw <- ternary(x_reverse, scale_x_reverse,
                                   ternary(is_x_continuous, scale_x_continuous,
                                           scale_x_discrete))
            if(is_blank(x_expand)) {
                x_expand <- ternary(is_x_continuous, c(0.017, 0), c(0, 0.6))
            }
            base_plot <- base_plot +
                x_axis_draw(name = x_lab, breaks = x_axis_breaks,
                            labels = x_axis_labels, limits = x_limit,
                            expand = x_expand)
        }
    } else {
        trans <- ternary(x_reverse, reverselog_trans(10), 'log10')
        if(is.null(x_axis_breaks)) {
            x_axis_breaks <- ternary(x_reverse, waiver(), base_log_breaks())
        }
        if(is.null(x_axis_labels)) x_axis_labels <- base::prettyNum
        if(is_blank(x_expand)) x_expand <- c(0.017, 0)
        base_plot <- base_plot +
            scale_x_continuous(name = x_lab, breaks = x_axis_breaks,
                               labels = x_axis_labels, limits = x_limit,
                               trans = trans, expand = x_expand)
    }
    if(!y_log) {
        if(!is_blank(y_axis_breaks) || y_reverse) {
            if(is_blank(y_axis_breaks)) y_axis_breaks <- waiver()
            if(is.null(y_axis_labels)) y_axis_labels <- waiver()
            y_axis_draw <- ternary(y_reverse, scale_y_reverse,
                                   ternary(is_y_continuous, scale_y_continuous,
                                           scale_y_discrete))
            if(is_blank(y_expand)) {
                y_expand <- ternary(is_y_continuous, c(0.025, 0), c(0, 0.6))
            }
            base_plot <- base_plot +
                y_axis_draw(name = y_lab, breaks = y_axis_breaks,
                            labels = y_axis_labels, limits = y_limit,
                            expand = y_expand)
        }
    } else {
        trans <- ternary(y_reverse, reverselog_trans(10), 'log10')
        if(is.null(y_axis_breaks)) {
            y_axis_breaks <- ternary(y_reverse, waiver(), base_log_breaks())
        }
        if(is.null(y_axis_labels)) y_axis_labels <- base::prettyNum
        if(is_blank(y_expand)) y_expand <- c(0.025, 0)
        base_plot <- base_plot +
            scale_y_continuous(name = y_lab, breaks = y_axis_breaks,
                               labels = y_axis_labels, limits = y_limit,
                               trans = trans, expand = y_expand)
    }
    
    # add horizontal reference line
    if(!is_blank(reference_hline)) {
        if(is.numeric(reference_hline)) {
            base_plot <- base_plot +
                geom_hline(yintercept = reference_hline, linetype = 'dashed')
        }
        if(isTRUE(reference_hline)) {
            base_plot <- base_plot +
                geom_hline(yintercept = 0, linetype = 'dashed')
        }
    }
    
    # add vertical reference line
    if(!is_blank(reference_vline)) {
        if(is.numeric(reference_vline)) {
            base_plot <- base_plot +
                geom_vline(xintercept = reference_vline, linetype = 'dashed')
        }
        if(isTRUE(reference_vline)) {
            base_plot <- base_plot +
                geom_hline(xintercept = 0, linetype = 'dashed')
        }
    }
    
    # add xlab, ylab and title
    base_plot <- base_plot + labs(x = x_lab, y = y_lab, title = title)
    
    # specify theme elements
    if(grids == 'on') {
        panel.grid.major.x = element_line()
        panel.grid.major.y = element_line()
        panel.grid.minor = element_blank()
    } else if(grids == 'x') {
        panel.grid.major.x = element_line()
        panel.grid.major.y = element_blank()
        panel.grid.minor = element_blank()
    } else if(grids == 'y') {
        panel.grid.major.x = element_blank()
        panel.grid.major.y = element_line()
        panel.grid.minor = element_blank()
    } else {
        panel.grid.major.x = element_blank()
        panel.grid.major.y = element_blank()
        panel.grid.minor = element_blank()
    }
    base_plot <- base_plot +
        theme(axis.text.x = element_text(angle = xtick_angle, hjust = x_hjust),
              axis.text.y = element_text(angle = ytick_angle, hjust = y_hjust),
              panel.grid.major.x = panel.grid.major.x,
              panel.grid.major.y = panel.grid.major.y,
              panel.grid.minor = panel.grid.minor,
              legend.position = legend_pos,
              plot.title = element_text(hjust = 0.5))
    if(!add_legend) base_plot <- base_plot + theme(legend.position = 'none')
    if(all(areColors(strip_background_color))) {
        base_plot <- base_plot +
            theme(strip.background = element_rect(fill = strip_background_color))
    }
    
    return(base_plot)
}


add_footnote <- function(ggplot_, footnote, fontface = 'italic',
                         fontsize = 8) {
    result <- gridExtra::arrangeGrob(
        ggplot_,
        bottom = grid::textGrob(footnote, x = 0,  hjust = 0, vjust = 0.1,
                          gp = gpar(fontface = fontface, fontsize = fontsize))
    )
    return(result)
}



plot_dev <- function(device, filename, dpi = 300) {
    if (is.function(device))
        return(device)
    
    eps <- function(...) {
        grDevices::postscript(..., onefile = FALSE, horizontal = FALSE,
                              paper = "special")
    }
    devices <- list(
        eps =  eps,
        ps =   eps,
        tex =  function(...) grDevices::pictex(...),
        pdf =  function(..., version = "1.4") grDevices::pdf(..., version = version),
        svg =  function(...) svglite::svglite(...),
        emf =  function(...) grDevices::win.metafile(...),
        wmf =  function(...) grDevices::win.metafile(...),
        png =  function(...) grDevices::png(..., res = dpi, units = "in"),
        jpg =  function(...) grDevices::jpeg(..., res = dpi, units = "in"),
        jpeg = function(...) grDevices::jpeg(..., res = dpi, units = "in"),
        bmp =  function(...) grDevices::bmp(..., res = dpi, units = "in"),
        tiff = function(...) grDevices::tiff(..., res = dpi, units = "in")
    )
    
    if (is.null(device)) {
        device <- tolower(tools::file_ext(filename))
    }
    
    if (!is.character(device) || length(device) != 1) {
        stop("`device` must be NULL, a string or a function.", call. = FALSE)
    }
    
    dev <- devices[[device]]
    if (is.null(dev)) {
        stop("Unknown graphics device '", device, "'", call. = FALSE)
    }
    dev
}

















