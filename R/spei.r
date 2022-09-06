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
#' monthly integration period. 
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
#' @param locname (string) Name of the site, which appears in the plot
#' @return spei_int (xts obj) spei time series at given integration period
#' @return spei_int_interp (xts obj) spei_int with NA values interpolated over
#' @export
get_spei <- function(cwdiff, scale=6,
                     na.rm = FALSE, plot=TRUE, locname='station'){

    #Get last year and month of timeseries to anchor the timeseries
    # that the spei function will take
    endmon <- zoo::as.yearmon(zoo::index(cwdiff[length(cwdiff)]))
    endyr <- floor(as.numeric(endmon))
    endmon <- as.numeric(format(endmon, '%m'))

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


#' Add calculated PET (thornthwaite) and SPEI columns to a dataframe . 
#'
#' Optionally make a plot of the data
#'
#' Default frequency is monthly (freq=12) but there may be ways to make
#' weekly frequency work (freq=52) if weekly PET is available.
#'
#' @param df (dataframe) dataframe with a date index, temp, ppt and lat
#' (latitude) columns
#' @param tcol (string) column name for monthly temperature
#' @param pcol (string) column name for monthly precipitation total
#' @param scale_mo (int) time scale, or number of months used to calculate SPEI
#' @param na.rm (bool) flag to mask NA values from the calculation (default=F)
#' @param plot (bool) Flag for making diagnostic plot of SPEI
#' @param loc (string) Name of the site, which appears in the plot & varname
#' @return df_out (dataframe) df with PET and SPEI columns added
#' @export
add_spei_cols <- function(df, tcol, pcol, scale_mo=12,
                     na.rm = FALSE, plot=TRUE, loc=NULL){
  #Calculate PET using thornthwaite method
  pet <- SPEI::thornthwaite(df[tcol], unique(df$lat))
  # Prepare datetime index
  df['month'] <- lubridate::month(df$date)
  df['year'] <- lubridate::year(df$date)
  dateidx <- zoo::as.yearmon(paste(df$month, '/', df$year, sep=''), "%m/%Y")
  # Precip and PET timeseries
  prcp_xts <- xts::xts(df[pcol], order.by=dateidx)
  pet_xts <- xts::xts(as.numeric(pet), order.by=dateidx)
  
  # Get spei tools, if not already installed use:
  # devtools::install_github("gremau/rclimtools")
  ## Climatic water differential
  cwdiff <- get_cwdiff(prcp_xts, pet_xts)
  
  ## Now get spei at specified scale (and use name if given)
  if(!is.null(loc)){
    spei_scale <- get_spei(cwdiff, scale=scale_mo, locname=loc)
    pet_vname <- paste0('pet_tho_', loc)
    spei_vname <- paste0('spei', as.character(scale_mo), 'mo_', loc)
  } else {
    spei_scale <- get_spei(cwdiff, scale=scale_mo)
    pet_vname <- 'pet_tho'
    spei_vname <- paste0('spei', as.character(scale_mo), 'mo')
  }
  # Extract values
  spei_xts <- xts::xts(as.vector(spei_scale$fitted),
                       order.by=zoo::index(cwdiff))
  
  # Add PET and SPEI values to dataframe and return
  df[pet_vname] <- as.vector(pet_xts)
  df[spei_vname] <- as.vector(spei_xts)
  return(df)
}
  
