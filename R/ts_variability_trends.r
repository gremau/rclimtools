library('xts')

get_rolling_CV <- function(ts_in, window, positivize=FALSE, plots=FALSE,
                           site='NULL'){
    # Calculate rolling window mean and std dev of series
    ts_rmean <- rollapply(ts_in, window, mean, align='right')
    ts_rsd <- rollapply(ts_in, window, sd, align='right')
    if (positivize==TRUE){
        # Calculate an adjusted ts (>0 for calculating CV) and get rolling mean
        # and standard deviation
        ts_adj <- ts_in - min(ts_in, na.rm=TRUE)
        ts_adj_rmean <- rollapply(ts_adj, window, mean, align='right')
        ts_adj_rsd <- rollapply(ts_adj, window, sd, align='right')
        # Calculate CV
        ts_rcv <- ts_adj_rsd/ts_adj_rmean
    } else {
        ts_rcv <- ts_rsd/ts_rmean
    }

    if (plots){
        plot(ts_rmean, main=site,ylab='Rolling mean')
        plot(ts_rsd, main=site,ylab='Rolling stdev')
        if (positivize==TRUE){
            lines(ts_adj_rsd, col='blue', lty=2)}
        plot(ts_rcv, main=site, ylab='Rolling CV')
    }
    return(ts_rcv)
}
