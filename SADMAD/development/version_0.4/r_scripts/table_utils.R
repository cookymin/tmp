#-----------------------------------------------------------------------------
# Purpose:  Define utility functions for dealing with tables in Shiny
# Author:   Feiyang Niu
# Date:     July 26, 2016
#-----------------------------------------------------------------------------


# load packaegs and necessary R script files
use_package('rtf')
source('r_scripts/common_statistics.R')
source('r_scripts/RTF2/RTF_functions.R')
source('r_scripts/RTF2/RTF_misc.R')


# define summary table
summary_table <- function(data_, rowlabel = ' ', collabel = NULL,
                          caption = '', footnote = '',
                          func_list = c('Min' = min, 'Median' = median,
                                        'Mean' = mean, 'Max' = max),
                          func_names = NULL, format = 'html') {
    if(is.null(func_names)) func_names <- names(func_list)
    summary_table <- fapply(func_list, data_, transpose = TRUE)
    if(!is.null(collabel)) colnames(summary_table) = collabel
    format <- match.arg(format, c('html', 'rtf'))
    if(format == 'html') {
        html <- invisible(
            htmlTable::htmlTable(
                summary_table, rowlabel = rowlabel,
                caption = caption, tfoot = footnote,
                css.cell = paste(c('padding-top: 0.5em', 'padding-right: 2em',
                                   'padding-bottom: 0.5em', 'padding-left: 2em'),
                                 sep = '', collapse = ';')
            )
        )
        return(html)
    } else if(format == 'rtf') {
        summary_df <- as.data.frame(summary_table, stringsAsFactors = FALSE,
                                    check.names = FALSE)
        summary_df <- cbind(
            setNames(data.frame(func_names, stringsAsFactors = F), rowlabel),
            summary_df
        )
        dimnames(summary_df) <- list(
            seq_len(nrow(summary_df)), c(rowlabel, colnames(summary_table))
        )
        return(summary_df)
    }
}


# define one-dim summary table by row
summary_table_row <- function(data_, row_var, row_names = NULL,
                             rowlabel = ' ', caption = '', footnote = '',
                             func_list = c('Min' = min, 'Median' = median,
                                           'Mean' = mean, 'Max' = max),
                             func_names = NULL, format = 'html',
                             header = 'Value') {
    format <- match.arg(format, c('html', 'rtf'))
    all_columns <- names(data_)
    stopifnot(row_var %in% all_columns)
    split_result <- split_2d(data_, row_var)
    if(is.null(row_names)) row_names <- names(split_result)
    if(is.null(func_names)) func_names <- names(func_list)
    summary_table <- matrix(fapply(
        func_list, split_result, transpose = TRUE
    ), ncol = 1)
    n_rgroup <- rep(length(func_list), length(row_names))
    rnames <- rep(func_names, length(row_names))
    html <- invisible(
        htmlTable::htmlTable(
            summary_table, header = header, rnames = rnames,
            rgroup = row_names, n.rgroup = n_rgroup, rowlabel = rowlabel,
            caption = caption, tfoot = footnote,
            css.cell = paste(c('padding-top: 0.5em', 'padding-right: 2em',
                               'padding-bottom: 0.5em', 'padding-left: 2em'),
                             sep = '', collapse = ';')
        )
    )
    return(html)
}


# define one-dim summary table by column
summary_table_col <- function(data_, col_var, col_names = NULL, rowlabel = ' ',
                              caption = '', footnote = '',
                              func_list = c('Min' = min, 'Median' = median,
                                            'Mean' = mean, 'Max' = max),
                              func_names = NULL, format = 'html') {
    all_columns <- names(data_)
    stopifnot(col_var %in% all_columns)
    split_result <- split_2d(data_, col_var)
    if(is_blank(col_names)) col_names <- levels(data_[[col_var]])
    if(is.null(func_names)) func_names <- names(func_list)
    summary_table <- fapply(func_list, split_result, transpose = TRUE)
    format <- match.arg(format, c('html', 'rtf'))
    if(format == 'html') {
        html <- invisible(
            htmlTable::htmlTable(
                summary_table, header = col_names, rowlabel = rowlabel,
                caption = caption, tfoot = footnote,
                css.cell = paste(c('padding-top: 0.5em', 'padding-right: 2em',
                                   'padding-bottom: 0.5em', 'padding-left: 2em'),
                                 sep = '', collapse = ';')
            )
        )
        return(html)
    } else if (format == 'rtf') {
        summary_df <- as.data.frame(summary_table, stringsAsFactors = FALSE,
                                    check.names = FALSE)
        summary_df <- cbind(
            setNames(data.frame(func_names, stringsAsFactors = F), rowlabel),
            summary_df
        )
        dimnames(summary_df) <- list(
            seq_len(nrow(summary_df)), c(rowlabel, col_names)
        )
        return(summary_df)
    }
}


# define two-dim summary table
summary_table_2d <- function(data_, row_var, col_var,
                             row_names = NULL, col_names = NULL,
                             rowlabel = ' ', caption = '', footnote = '',
                             func_list = c('Min' = min, 'Median' = median,
                                           'Mean' = mean, 'Max' = max),
                             func_names = NULL) {
    all_columns <- names(data_)
    stopifnot(any(c(row_var, col_var) %in% all_columns))
    split_result <- split_2d(data_, row_var, col_var)
    if(is.null(row_names)) row_names <- rownames(split_result)
    if(is_blank(col_names)) col_names <- levels(data_[[col_var]])
    if(is.null(func_names)) func_names <- names(func_list)
    if(is.null(dim(split_result))) {
        split_result <- array(
            split_result,
            dim = c(length(unique(data_[[row_var]])),
                    length(unique(data_[[col_var]]))),
            dimnames = list(row_names, col_names)
        )
    }
    summary_table <- apply(
        split_result, 2, fapply, func_list = func_list, transpose = TRUE
    )
    n_rgroup <- rep(length(func_list), length(row_names))
    rnames <- rep(func_names, length(row_names))
    html <- invisible(
        htmlTable::htmlTable(
            summary_table, header = col_names, rnames = rnames,
            rgroup = row_names, n.rgroup = n_rgroup, rowlabel = rowlabel,
            caption = caption, tfoot = footnote,
            css.cell = paste(c('padding-top: 0.5em', 'padding-right: 2em',
                               'padding-bottom: 0.5em', 'padding-left: 2em'),
                             sep = '', collapse = ';')
        )
    )
    return(html)
}


# function that calculate summary matrix
summary_matrix <- function(data_, row_var, col_var = NULL, val_var = NULL,
                           col_totals = NULL, name_totals = NULL,
                           func_list = c('Min' = min, 'Median' = median,
                                         'Mean' = mean, 'Max' = max)) {
    all_columns <- names(data_)
    stopifnot(row_var %in% all_columns)
    if(!is.factor(data_[[row_var]]))
        data_[[row_var]] <- factor(data_[[row_var]])
    vars <- row_var
    if(!is_blank(col_var)) {
        vars <- c(vars, col_var)
        stopifnot(col_var %in% all_columns)
        if(!is.factor(data_[[col_var]]))
            data_[[col_var]] <- factor(data_[[col_var]])
    }
    if(!is_blank(val_var)) {
        vars <- c(vars, val_var)
        stopifnot(val_var %in% all_columns)
        data_ <- data_[, vars, drop = FALSE]
    }
    
    if(is_blank(col_var)) {
        if(is_blank(val_var)) val_var <- base::setdiff(all_columns, row_var)
        data_split <- split(data_[[val_var]], data_[[row_var]])
        summary_table <- matrix(fapply(
            func_list, data_split, transpose = TRUE
        ), ncol = 1)
    } else {
        if(is_blank(val_var))
            val_var <- base::setdiff(all_columns, c(row_var, col_var))
        temp <- split(data_[, c(val_var, row_var)], data_[[col_var]])
        data_list <- lapply(lapply(temp, `[`, val_var), as.matrix)
        factor_list <- lapply(temp, `[`, row_var)
        data_split <- mapply(split, data_list, factor_list)
        if(!is_blank(col_totals)) {
            if(is_blank(name_totals))
                name_totals <- paste('Total', seq_along(col_totals))
            start_idx <- 1
            data_split_all <- NULL
            for(idx in seq_along(col_totals)) {
                col_total <- col_totals[idx]
                to_append_idx <- start_idx:col_total
                start_idx <- col_total + 1
                col_to_add <- array(
                    lapply(apply(data_split[, 1:col_total, drop = F],
                                 1, cbind), unlist),
                    dim = c(nrow(data_split), 1),
                    dimnames = list(rownames(data_split), name_totals[idx])
                )
                data_split_all <- cbind(data_split_all,
                                        data_split[, to_append_idx, drop = F],
                                        col_to_add)
            }
            if(max(col_totals) < ncol(data_split)) {
                data_split_all <- cbind(
                    data_split_all,
                    data_split[, start_idx:ncol(data_split), drop = F]
                )
            }
            data_split <- data_split_all
        }
        if(is.null(dim(data_split))) {
            data_split <- array(
                data_split,
                dim = c(length(unique(data_[[row_var]])),
                        length(unique(data_[[col_var]])))
            )
        }
        summary_table <- apply(
            data_split, 2, fapply, func_list = func_list, transpose = TRUE
        )
    }
    
    return(summary_table)
}


# function that outputs rtf or html summary table
summary_table_all <- function(data_, row_var, row_names = '',
                              col_var = NULL, col_names = '', val_var = NULL,
                              col_totals = NULL, name_totals = NULL,
                              n_in_header = TRUE, subj_col = NULL,
                              baseline_name = NULL,
                              add_cfb = FALSE, cfb_var = NULL,
                              func_list = c('Min' = min, 'Median' = median,
                                            'Mean' = mean, 'Max' = max),
                              func_names = names(func_list),
                              caption = '', footnote = '', header = 'Value',
                              rowlabel = ' ', visit_header_space = 4,
                              format = 'html') {
    n_in_header <- isTRUE(n_in_header)
    add_cfb <- isTRUE(add_cfb)
    format <- match.arg(format, choices = c('rtf', 'html', 'csv'))
    if(is.null(func_names)) stop('Please provide func_names')
    
    all_columns <- names(data_)
    stopifnot(row_var %in% all_columns)
    if(!is.factor(data_[[row_var]]))
        data_[[row_var]] <- factor(data_[[row_var]])
    row_nlevels <- nlevels(data_[[row_var]])
    row_levels <- levels(data_[[row_var]])
    if(is_blank(row_names)) row_names <- row_levels
    if(is_blank(baseline_name)) baseline_name <- row_names[1]
    n_funcs <- length(func_list)
    if(!is_blank(col_var)) {
        stopifnot(col_var %in% all_columns)
        if(!is.factor(data_[[col_var]]))
            data_[[col_var]] <- factor(data_[[col_var]])
        if(is_blank(col_names)) col_names <- levels(data_[[col_var]])
        col_nlevels <- nlevels(data_[[col_var]])
        if(!is_blank(col_totals)) {
            if(is_blank(name_totals))
                name_totals <- paste('Total', seq_along(col_totals))
            col_names <- vector_insert(col_names, name_totals, after = col_totals)
        }
        if(n_in_header) {
            if(is_blank(subj_col))
                stop('Please provide subj_col')
            subj_split <- split(data_[[subj_col]], data_[[col_var]])
            subj_split <- lapply(subj_split, unique)
            nsubj <- unlist(lapply(subj_split, length))
            if(!is_blank(col_totals)) {
                n_totals <- unlist(lapply(
                    col_totals,
                    function(n) {length(unique(unlist(subj_split[seq_len(n)])))}
                ))
                nsubj <- vector_insert(nsubj, n_totals, after = col_totals)
            }
            col_names <- paste0(
                col_names, '\n', '(N=', nsubj, ')'
            )
        }
    }
    
    summary_tbl <- summary_matrix(
        data_, row_var, col_var = col_var, val_var = val_var,
        col_totals = col_totals, name_totals = name_totals,
        func_list = func_list
    )
    if(!is_blank(col_var)) colnames(summary_tbl) <- col_names
    
    # add change from baseline block
    if(add_cfb && row_nlevels > 1) {
        if(is_blank(cfb_var) || !cfb_var %in% all_columns)
            stop('Please provide a valid column name for cfb_var')
        data_cfb <- data_[!(data_[[row_var]] %in% baseline_name), , drop = F]
        data_cfb[[row_var]] <- factor(
            data_cfb[[row_var]],
            levels = row_names[row_names != baseline_name]
        )
        summary_tbl_cfb <- summary_matrix(
            data_cfb, row_var, col_var = col_var, val_var = cfb_var,
            col_totals = col_totals, name_totals = name_totals,
            func_list = func_list
        )
        
        # combine summary_tbl and summary_tbl_cfb
        summary_tbl_combined <- rbind(summary_tbl, summary_tbl_cfb)
        idx_val <- c(
            seq_len(n_funcs),
            unlist(lapply(seq(n_funcs + 1,
                              nrow(summary_tbl_combined),
                              by = 2 * n_funcs),
                          `+`, 0:(n_funcs - 1)))
        )
        idx_cfb <- unlist(lapply(
            seq(2 * n_funcs + 1, nrow(summary_tbl_combined), by = 2 * n_funcs),
            `+`, 0:(n_funcs - 1)))
        summary_tbl_combined[idx_val, ] <- summary_tbl
        summary_tbl_combined[idx_cfb, ] <- summary_tbl_cfb
        summary_tbl <- summary_tbl_combined
    }
    
    if(format == 'html') {
        header <- ternary(is_blank(col_var), header, col_names)
        if(add_cfb && row_nlevels > 1) {
            idx_bs <- row_names == baseline_name
            rnames <- rep(func_names, 2 * row_nlevels - sum(idx_bs))
            rgroup<- c(
                row_names[idx_bs],
                c(rbind(row_names[!idx_bs],
                        paste('Change from baseline,', row_names[!idx_bs])))
            )
            n_rgroup <- rep(n_funcs, 2 * row_nlevels - sum(idx_bs))
        } else {
            rnames <- rep(func_names, row_nlevels)
            rgroup <- row_names
            n_rgroup <- rep(n_funcs, row_nlevels)
        }
        html <- invisible(
            htmlTable::htmlTable(
                summary_tbl, header = header, rnames = rnames,
                rgroup = rgroup, n.rgroup = n_rgroup, rowlabel = rowlabel,
                caption = caption, tfoot = footnote,
                css.cell = paste(c('padding-top: 0.5em', 'padding-right: 2em',
                                   'padding-bottom: 0.5em', 'padding-left: 2em'),
                                 sep = '', collapse = ';')
            )
        )
        return(html)
    } else if(format == 'rtf') {
        if(add_cfb && row_nlevels > 1) {
            summary_tbl_rtf <- do.call(
                rbind,
                c(list(summary_tbl), as.list(rep('', 2 * row_nlevels - 1)))
            )
            idx_na <- seq(1, nrow(summary_tbl_rtf), by = n_funcs + 1)
            idx_rtf <- sort(setdiff(seq_len(nrow(summary_tbl_rtf)), idx_na))
            summary_tbl_rtf[idx_rtf, ] <- summary_tbl
            summary_tbl_rtf[idx_na, ] <- ''
            add_col <- rep('', nrow(summary_tbl_rtf))
            visits <- c(
                baseline_name,
                rep(row_names[-match(baseline_name, row_names)], each = 2)
            )
            idx_cfb <- seq(3, length(visits), by = 2)
            visits[idx_cfb] <- paste('Change from baseline,', visits[idx_cfb])
            add_col[idx_na] <- visits
            add_col[idx_rtf] <- paste(rep(' ', visit_header_space), func_names)
        } else {
            summary_tbl_rtf <- do.call(
                rbind,
                c(list(summary_tbl), as.list(rep('', row_nlevels)))
            )
            idx_na <- seq(1, nrow(summary_tbl_rtf), by = n_funcs + 1)
            idx_rtf <- sort(setdiff(seq_len(nrow(summary_tbl_rtf)), idx_na))
            summary_tbl_rtf[idx_rtf, ] <- summary_tbl
            summary_tbl_rtf[idx_na, ] <- ''
            add_col <- rep('', nrow(summary_tbl_rtf))
            add_col[idx_na] <- row_names
            add_col[idx_rtf] <- paste(rep(' ', visit_header_space), func_names)
        }
        summary_tbl_rtf <- cbind(
            ' ' = add_col,
            summary_tbl_rtf
        )
        dimnames(summary_tbl_rtf) <- list(
            seq_len(nrow(summary_tbl_rtf)),
            c(' ', ternary(is_blank(col_var), header, col_names))
        )
        return(summary_tbl_rtf)
    } else if(format == 'csv') {
        summary_tbl_csv <- data.frame(
            Statistics = func_names,
            Value = c(summary_tbl),
            stringsAsFactors = FALSE, check.names = FALSE
        )
        visits <- rep(row_levels, each = n_funcs)
        if(is_blank(col_var)) {
            if(add_cfb && row_nlevels > 1) {
                visits_cfb <- rep(row_levels[-match(baseline_name, row_levels)],
                                  each = n_funcs)
                summary_tbl_csv$Visits <- c(visits, visits_cfb)
                summary_tbl_csv$YVar <- c(
                    rep('AVAL', length(visits)), rep('CHG', length(visits_cfb))
                )
            } else {
                summary_tbl_csv$Visits <- visits
            }
        } else {
            group_levels <- rep(col_names, each = length(visits))
            visits_col <- rep(visits, length(col_names))
            if(add_cfb && row_nlevels > 1) {
                visits_cfb <- rep(row_levels[-match(baseline_name, row_levels)],
                                  each = n_funcs)
                visits_cfb_col <- rep(visits_cfb, length(col_names))
                group_levels_cfb <- rep(col_names, each = length(visits_cfb))
                summary_tbl_csv$Visits <- c(visits_col, visits_cfb_col)
                summary_tbl_csv$Group <- c(group_levels, group_levels_cfb)
                summary_tbl_csv$YVar <- c(
                    rep('AVAL', length(visits_col)),
                    rep('CHG', length(visits_cfb_col))
                )
            } else {
                summary_tbl_csv$Visits <- visits_col
                summary_tbl_csv$Group <- group_levels
            }
        }
        return(summary_tbl_csv)
    }
}



# rtf table wrapper
rtf_table_wrapper <- function(file, tbl, width = 11, height = 8.5,
                              fontsize = 8, omi = NULL, cell1 = NULL,
                              cell2 = 1, nheader = NULL, 
                              nline.table = 42, nline.body = NULL,
                              block_break = FALSE, nline_block = NULL, 
                              caption = '', footnote = '',
                              addSpaceHeader = 0, addSpaceFoot = 0) {
    block_break <- isTRUE(block_break)
    caption <- trimws(caption)
    footnote <- trimws(footnote)
    num_wd <- min(
        max(nchar(tbl[, -1]), nchar(colnames(tbl)[-1])
    ) * fontsize * 0.01, 2)
    firstcol_wd <- min(
        max(nchar(tbl[, 1]), nchar(colnames(tbl)[1])
    ) * fontsize * 0.01, 2)
    width_content <- firstcol_wd + num_wd * (ncol(tbl) - 1)
    if(!is.null(omi)) omi <- omi
    else {
        left_omi <- (width - width_content) / 2
        right_omi <- left_omi
        omi = c(1, left_omi, 1, right_omi)
    }
    rtf <- rtf::RTF(file, width = width, height = height, omi = omi,
                    font.size = fontsize)
    if(is.null(nline.body)) {
        caption_lines <- length(unlist(strsplit(caption, '\n')))
        footnote_lines <- length(unlist(strsplit(footnote, '\n')))
        header_lines <- 1
        if(!is.null(nheader)) {
            if(is.null(cell2)) header_lines <- cell2
            header_lines <- header_lines + nheader - 1
        }
        nline.body <- nline.table - caption_lines - footnote_lines - header_lines
    }
    if(nrow(tbl) < nline.body) nline.body <- nrow(tbl)
    else {
        if(block_break) {
            stopifnot(!is.null(nline_block))
            nline.body <- nline_block * (nline.body %/% nline_block)
        }
    }
    rtf.table.out(
        rtf, tb = tbl,
        cell1 = cell1, cell2 = cell2, nheader = nheader,
        colFormat = c('L', rep('C', ncol(tbl) - 1)), 
        cw = c(firstcol_wd, rep(num_wd, ncol(tbl) - 1)),
        width = width, height = height, omi = omi,
        varName = NULL, var.ul = NULL,
        titles = caption, prd.status = '', footns = footnote,
        nline.body = nline.body, addSpaceHeader = addSpaceHeader,
        addSpaceFoot = addSpaceFoot
    )
    done(rtf)
}





























