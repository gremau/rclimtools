#' Get climatic water difference from precip and pet data
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param precip (xts object) monthly precipitation sums
#' @param pet (xts object) monthly potential ET sums
#' @return cwdiff (xts object) climatic water differential
#' @export
get_cwdiff <- function(precip, pet){
    cwdiff <- precip - pet
    return(cwdiff)
}

#' Get SPEI values from climatic water differential at given frequency and
#' integration period. 
#'
#' Optionally make a plot of the data
#'
#' Default frequency is monthly (freq=12) but there may be ways to make
#' weekly frequency work (freq=52) if weekly PET is available.
#'
#' @param cwdiff (xts obj) monthly climatic water differential time series
#' @param scale (int) time scale, or number of months used to calculate SPEI
#' @param na.rm (bool) flag to mask NA values from the calculation (default=F)
#' @param plot (bool) Flag for making diagnostic plot of SPEI
#' @param locname (string) Name of the site, which appears in the plotc
#' @return spei_int (xts obj) spei time series at given integration period
#' @return spei_int_interp (xts obj) spei_int with NA values interpolated over
#' @export
get_spei <- function(cwdiff, scale=6,
                     na.rm = FALSE, plot=TRUE, locname='no name'){

    #Get last year and month of timeseries to anchor the timeseries
    # that the spei function will take
    endmon <- zoo::as.yearmon(zoo::index(cwdiff[length(cwdiff)]))
    endyr <- floor(as.numeric(startmon))
    endmon <- as.numeric(format(startmon, '%m'))

    # Get spei for that integration period
    spei_int <- SPEI::spei(
      ts(cwdiff, frequency=12, end=c(endyr, endmon)),
      scale, na.rm=na.rm)

    # Check for invalid values
    values <- spei_int$fitted
    values[!is.finite(values)] <- NA
    if (sum(is.na(values)) > (scale - 1)){
        print('WARNING!!! - there are invalid values in the SPEI series')
    }

    # Note that this will say 'SPI' on the y axis label, which is incorrect
    # The plot.spei function doesn't deal with the `SPEI::spei()` call well and
    # seems to set the label wrong. Passing ylab doesn't help either.
    if (plot){
        plot(spei_int, main=paste0(locname, ' - ', scale, ' month SPEI'),
             ylab='SPEI')
    }
    # Extract the spei values from the returned object and make xts
    #spei_xts <- xts(as.vector(spei_int$fitted),  order.by=index(cwdiff))
    
    
    # There may be both infinite and NA values in the output
    # Convert -Inf to NA
    #print(sum(is.na(spei_xts)))
    #spei_xts[!is.finite(spei_xts)] <- NA
    #print(sum(is.na(spei_xts)))
    
    # Interpolate over NA values
    #spei_xts_interp <- na.approx(spei_xts)

    return(spei_int)
}
