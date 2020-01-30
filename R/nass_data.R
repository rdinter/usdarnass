#' @title Get data from the Quick Stats query
#' @description Sends query to Quick Stats API from given parameter values. Data
#'   request is limited to 50,000 records per the API. Use
#'   \code{\link{nass_count}} to determine number of records in query.
#' @inheritParams nass_count
#' @param format Output format from API call. Defaults to CSV as it is typically
#'   the smallest sized call. Other options are JSON and XML but these are not
#'   recommended. XML currently not supported.
#' @param numeric_vals Optional to convert the year, Value, and coefficient of
#'   variation (CV \%) to numerics as opposed to defaulted character values.
#'   Default is to FALSE as some Values have a suppression code. Converting to
#'   numeric will result in suppressed values to be NA.
#' @return A data frame containing query to API.
#' @export nass_data
#' @examples
#'
#' \dontrun{
#' # Get state values in 2012 for all of the values of agricultural land
#' nass_data(agg_level_desc = "STATE", year = "2012",
#' commodity_desc = "AG LAND", domain_desc = "VALUE")
#' }
#'
#' \dontrun{
#' # Get county level values in 2012 for the specific data item
#'   nass_data(year = 2012, agg_level_desc = "COUNTY",
#'    short_desc = "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $")
#' }

nass_data <- function(source_desc = NULL,
                      sector_desc = NULL,
                      group_desc = NULL,
                      commodity_desc = NULL,
                      class_desc = NULL,
                      prodn_practice_desc = NULL,
                      util_practice_desc = NULL,
                      statisticcat_desc = NULL,
                      unit_desc = NULL,
                      short_desc = NULL,
                      domain_desc = NULL,
                      domaincat_desc = NULL,
                      agg_level_desc = NULL,
                      state_ansi = NULL,
                      state_fips_code = NULL,
                      state_alpha = NULL,
                      state_name = NULL,
                      asd_code = NULL,
                      asd_desc = NULL,
                      county_ansi = NULL,
                      county_code = NULL,
                      county_name = NULL,
                      region_desc = NULL,
                      zip_5 = NULL,
                      watershed_code = NULL,
                      watershed_desc = NULL,
                      congr_district_code = NULL,
                      country_code = NULL,
                      country_name = NULL,
                      location_desc = NULL,
                      year = NULL,
                      freq_desc = NULL,
                      begin_code = NULL,
                      end_code = NULL,
                      reference_period_desc = NULL,
                      week_ending = NULL,
                      key = NULL,
                      format = c("CSV", "JSON", "XML"),
                      numeric_vals = FALSE){
  format = match.arg(format)
  if (format == "XML") stop("XML not supported yet.")
  
  # First check that this call will even fit into the count limit
  calls_count      <- match.call(expand.dots = TRUE)
  calls_count[[1]] <- as.name("nass_count")
  count            <- eval.parent(calls_count)
  # count <- do.call(nass_count, as.list(match.call(expand.dots = TRUE)[-1]))
  
  if (count > 50000) stop(paste0("Query returns ",
                                 prettyNum(count, big.mark = ","),
                                 " records. The limit is 50,000. Subset the ",
                                 "query to fit within limit. See nass_count()"))
  
  # Now, normal procedure.
  calls      <- match.call(expand.dots = TRUE)
  calls[[1]] <- as.name("args_list")
  arguments  <- eval.parent(calls)
  
  temp     <- httr::GET("http://quickstats.nass.usda.gov/api/api_GET",
                        query = arguments)
  tt       <- check_response(temp)
  
  
  if (methods::is(tt, "data.frame")) {
    nass <- tt
    if (numeric_vals) {
      nass$year     <- as.numeric(nass$year)
      nass$Value    <- suppressWarnings(as.numeric(gsub(",", "", nass$Value)))
      nass$`CV (%)` <- suppressWarnings(as.numeric(gsub(",", "", nass$`CV (%)`)))
    }
  } else {
    stop("Parameter entered is not valid")
  }

  return(nass)
}
