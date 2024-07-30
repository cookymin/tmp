#-------------------------------------------------------------------------------
#   Produce barplot using ggplot2
#
#   Author: Feiyang Niu
#   Date:   Feb 15, 2017
#-------------------------------------------------------------------------------



# load libraries and r scripts
use_package('ggplot2')
use_package('dplyr')
source('r_scripts/common_statistics.R')
source('r_scripts/plot_utils.R')



# gg_barplot function
gg_barplot <- function(data, x, x_levels = NULL, group = NULL,
                       facet_r = NULL, facet_c = NULL,
                       facet_r_title_angle = 0, facet_c_title_angle = 90,
                       facet_scale = 'free', facet_space = 'free',
                       xlab = x, ylab = '', title = '', bar_label = TRUE,
                       xtick_angle = 0, bw_theme = TRUE, flip_coord = TRUE) {
    
    #---------------------------
    # argument match
    #---------------------------
    all_columns <- names(data)
    if((!is.character(x)) | (!x %in% all_columns))
        stop('x must be a column of data')
    if(!is_blank(group)) {
        if(!(group %in% all_columns))
            stop('group must be a column of data')
    }
    if(!is_blank(facet_r)) {
        if(!(facet_r %in% all_columns)) stop('facet_r must be a column of data')
    }
    if(!is_blank(facet_c)) {
        if(!(facet_c %in% all_columns)) stop('facet_c must be a column of data')
    }
    bar_label <- isTRUE(bar_label)
    bw_theme <- isTRUE(bw_theme)
    flip_coord <- isTRUE(flip_coord)
    
    
    #---------------------------
    # data manipulation
    #---------------------------
    
    # sort the data
    group_by_vars <- c()
    if(!is_blank(facet_r)) group_by_vars <- c(group_by_vars, facet_r)
    if(!is_blank(facet_c)) group_by_vars <- c(group_by_vars, facet_c)
    if(!is_blank(group)) group_by_vars <- c(group_by_vars, group)
    group_by_vars <- c(group_by_vars, x)
    group_dots <- lapply(group_by_vars, as.symbol)
    data <- data %>%
        arrange_(.dots = group_dots) %>%
        group_by_(.dots = group_dots)
    
    # make variable x a factor
    x_unique <- unique_na(data[[x]])
    if(flip_coord) x_unique <- rev(x_unique)
    if(!is_blank(x_levels)) x_unique <- x_levels
    data[[x]] <- factor(data[[x]], levels = x_unique)
    
    
    #---------------------------
    # make the plot
    #---------------------------
    plot_ <- gg_wrapper(
        data = data, aes_string(x = paste0('`', x, '`')),
        facet_r = facet_r, facet_c = facet_c,
        facet_scale = facet_scale, facet_space = facet_space,
        xlab = xlab, ylab = ylab, title = title,
        xtick_angle = xtick_angle, bw_theme = bw_theme, fill_var = group
    )
    plot_ <- plot_ + geom_bar()
    if(bar_label) {
        if(flip_coord) {
            plot_ <- plot_ +
                geom_text(stat = 'count', aes(label = ..count..), hjust = -.1)
        } else {
            plot_ <- plot_ +
                geom_text(stat = 'count', aes(label = ..count..), vjust = -.3)
        }
    }
    plot_ <- plot_ + theme(strip.text.x = element_text(angle = facet_c_title_angle),
                           strip.text.y = element_text(angle = facet_r_title_angle))
    if(flip_coord) {
        plot_ <- plot_ + coord_flip()
    }
    
    return(plot_)
}



























