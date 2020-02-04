GetSignalPerformance <- function(code, local = TRUE){

    df.performance = get_signal_performance(code, local)
    
    # Get selected columns
    col.all = colnames(df.performance)
    col.price = col.all[grepl("day\\.\\d*$", col.all, perl=TRUE)]
    col.selected = do.call(c, list(c('date', 'code', 'signal', 'signal_index'),
                               col.price,
                               c('success')))
        
    df.res = df.performance[, col.selected, with = FALSE][order(code, signal, -date)]
    
    return(df.res)
}

LoadHitSignal <- function(ref.date, option.only = TRUE, local = TRUE){

    # Selected column only
    df.signal = load_hit_signal(ref.date, format = 'long', option.only, local)
    df.signal.selected = df.signal[, c('code', 'date', 'signal'), with=FALSE]

    return(df.signal.selected)
}
