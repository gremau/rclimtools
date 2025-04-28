library(httr2)

# See here for possible API calls:
# https://www.ncei.noaa.gov/support/access-data-service-api-user-documentation
# 'https://www.ncei.noaa.gov/access/services/data/v1?dataset=global-summary-of-the-year&dataTypes=TMAX,TMIN,TAVG,PRCP&stations=USC00267369&startDate=2017-01-01&endDate=2020-01-31&includeAttributes=true&format=csv'
# Other possible services here: https://www.ncei.noaa.gov/access
base_url <- 'https://www.ncei.noaa.gov/access/services/data/v1'

#' Get a table of summary data for a GHCN station
#'
#' This function fetches a csv of climate observations selected from
#' a GHCN weather station and dataset and returns data as a tibble.
#'
#' GHCN-daily data: https://doi.org/10.7289/V5D21VHZ
#' GHCN-monthly data: https://doi.org/10.7289/V5QV3JJ5
#' GHCN-annual data: https://doi.org/10.7289/JWPF-Y430
#'
#' API documentation:
#' https://www.ncei.noaa.gov/support/access-data-service-api-user-documentation
#'
#' @param stationlist (char vector) comma delimited list of station identifiers
#' @param startdt (char) query start date string of format YYYY-MM-DD
#' @param enddt (char) query end date string of format YYYY-MM-DD
#' @param varnames (char) comma delimited list of variable names to query for
#' @param summarytype (char) string for selecting a summary dataset to query
#'  (daily, monthly, or annual)
#' @return ghcn_df (tibble) table of query results
#' @export
ghcn_summary_data <- function(stationIds, startdt, enddt,
    varnames="TMAX,TMIN,TAVG,PRCP", summarytype='annual',
    parsedts=TRUE){

        # Get string identifiers for the dataset
        if (summarytype=='annual'){
            dataset_name <- "global-summary-of-the-year"
        } else if (summarytype=='monthly'){
            dataset_name <- "global-summary-of-the-month"
        } else if (summarytype=='daily'){
            dataset_name <- "daily-summaries"
        }

        # Construct the request
        req <- httr2::request(base_url) |>
            httr2::req_url_query(
                dataset = dataset_name,
                dataTypes = varnames,
                stations = stationIds,
                startDate = startdt,
                endDate = enddt,
                format = 'csv',
                units = 'metric',
                includeAttributes = 'false'
            )

        # Send the request
        resp <- req |> httr2::req_perform()
        print(resp)

        # Parse the body (a csv) and return as a tibble
        ghcn_df <- readr::read_csv(httr2::resp_body_raw(resp))

        # If parsing dates, create a valid datetime column with last day of
        # year or month.
        if (parsedts){
            if (dataset_name=="global-summary-of-the-year"){
                # Use last day of year in datetime
                tempdt <- paste0(ghcn_df$DATE, "-12-31")
                ghcn_df$DATE <- as.Date(tempdt, "%Y-%m-%d")
            } else if (dataset_name=="global-summary-of-the-month"){
                # Use last day of month in datetime
                tempdt <- as.Date(paste0(ghcn_df$DATE, "-01"), "%Y-%m-%d")
                tempdt2 <- paste(ghcn_df$DATE,
                    lubridate::days_in_month(tempdt), sep='-')
                ghcn_df$DATE <- as.Date(tempdt2, "%Y-%m-%d")
            } else if (dataset_name=="daily-summaries"){
                ghcn_df$DATE <- as.Date(ghcn_df$DATE, "%Y-%m-%d")
            }
        }

        return(ghcn_df)
    }

#' Get the station identifiers list for GHCN
#'
#' This function fetches a table of GHCN weather station identifiers and
#' other information and returns data as a tibble.
#'
#' Current location is:
#' https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt
#'
#' API documentation:
#' https://www.ncei.noaa.gov/support/access-data-service-api-user-documentation
#'
#' @param loc (char) location of the file at NCEI, or elsewhere
#' @return ghcn_inv (tibble) table of query results
#' @export
ghcn_inventory <- function(
    loc='https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt'){
        # Read inventory file from location
        ghcn_inv <- readr::read_fwf(loc,
            col_positions=readr::fwf_widths(c(12, 9, 10, 7, 3, 30, 4, 4, 6),
                col_names=c('id','lat','lon','elev','state','name', 'gsn_flag',
                    'hcn_crn_flag','wmo_id'))
        )
        return(ghcn_inv)
    }

#' Subset a GHCN dataframe by station
#'
#' This function drops all rows in a GHCN dataframe except for
#' the given station.
#'
#' @param ghcn_df (tibble) GHCN dataframe with (usually) multiple stations
#' @param stationId (char) GHCN station identifier to keep
#' @return ghcn_df2 (tibble) table of subsetted station data
#' @export
ghcn_df_subset <- function(ghcn_df, stationId){
    # Read inventory file from location
    ghcn_df2 <- subset(ghcn_df, STATION==stationId)
    return(ghcn_df2)
    }

#' Drop flag columns from a GHCN dataframe
#'
#' This function drops all flag columns in a GHCN dataframe.
#'
#' @param ghcn_df (tibble) GHCN dataframe with (usually) flag columns
#' @return ghcn_df2 (tibble) table of flag-free station data
#' @export
# ghcn_dropflags <- function(ghcn_df, stationId){
#     # Read inventory file from location
#     ghcn_df2 <- ghcn_df |>
#         dplyr::select()
#     return(ghcn_df2)
#     }
