GetSignalPerformance <- function(){
    return("ABC")
}

LoadHitSignal <- function(ref_date, option_only = TRUE, local = TRUE){

    # Variable name conversion
    ref.date = ref_date
    option.only = option_only

    # Selected column only
    df.sign     al = load_hit_signal(ref.date, format = 'long', option.only, local)
    df.signal.selected = df.signal[, c('code', 'date', 'signal'), with=FALSE]

    return(df.signal.selected)
}
