GetSignalPerformance <- function(){
    return("ABC")
}

LoadHitSignal <- function(ref.date, option.only = TRUE, local = TRUE){

    # Selected column only
    df.signal = load_hit_signal(ref.date, format = 'long', option.only, local)
    df.signal.selected = df.signal[, c('code', 'date', 'signal'), with=FALSE]

    return(df.signal.selected)
}
