GetSignalPerformance <- function(code, local = TRUE){

    # Get selected columns
    col.all = colnames(df.performance)
    col.price = all.col[grepl("day\\.\\d*$", all.col, perl=TRUE)]
    col.selected = do.call(c, list(c('date', 'code', 'signal', 'signal_index'),
                               col.price,
                               c('success')))
        
    df.performance = get_signal_performance(code, local)
    df.res = df.performance[, col.selected, with = FALSE]
    
    return(df.res)
}

LoadHitSignal <- function(ref.date, option.only = TRUE, local = TRUE){

    # Selected column only
    df.signal = load_hit_signal(ref.date, format = 'long', option.only, local)
    df.signal.selected = df.signal[, c('code', 'date', 'signal'), with=FALSE]

    return(df.signal.selected)
}
