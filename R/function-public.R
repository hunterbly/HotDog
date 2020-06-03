GetSignalPerformance <- function(code, local = TRUE, verbose = FALSE){

    ## df = GetSignalPerformance(700, FALSE, FALSE)

    df.performance = get_signal_performance(code, local, verbose)

    # Get selected columns
    col.all = colnames(df.performance)
    col.price = col.all[grepl("day\\.\\d*$", col.all, perl=TRUE)]
    col.return = col.all[grepl("day\\.\\d*\\.return$", col.all, perl=TRUE)]
    col.selected = do.call(c, list(c('date', 'code', 'signal', 'signal_index'),
                               col.price,
                               col.return,
                               c('success')))

    df.res = df.performance[, col.selected, with = FALSE][order(code, signal, -date)]

    # Derive return column
    df.res = df.res[, `:=`(return = get_first_from_list( c(day.1.return,
                                                           day.2.return,
                                                           day.3.return,
                                                           day.4.return,
                                                           day.5.return) )),
                    by = seq_len(nrow(df.res))]  # by each row

    df.res = df.res[, `:=`(return = ifelse(success, return, NA))]
    return(df.res)
}

LoadHitSignal <- function(ref.date, option.only = TRUE, local = TRUE){

    # Selected column only
    df.signal = load_hit_signal(ref.date, format = 'long', option.only, local)
    df.signal.selected = df.signal[, c('code', 'date', 'signal', 'signal_index'), with=FALSE]

    return(df.signal.selected)
}
