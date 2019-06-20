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
                      short_desc = NULL,
                      domain_desc = NULL,
                      domaincat_desc = NULL,
                      agg_level_desc = NULL,
                      statisticcat_desc = NULL,
                      state_name = NULL,
                      asd_desc = NULL,
                      county_name = NULL,
                      region_desc = NULL,
                      zip_5 = NULL,
                      watershed_desc = NULL,
                      year = NULL,
                      freq_desc = NULL,
                      reference_period_desc = NULL,
                      token = NULL,
                      format = c("CSV", "JSON", "XML"),
                      numeric_vals = FALSE){
  format = match.arg(format)
  if (format == "XML") stop("XML not supported yet.")
  
  token <- check_key(token)

  # Check to see if year used a logical operator
  year  <- trimws(year)
  punct <- grepl("[[:punct:]]", year)
  if (length(punct) == 0) punct <- FALSE
  punct_year <- as.numeric(gsub("[[:punct:]]", "", year))
  
  args <- list(source_desc = source_desc,
               sector_desc = sector_desc,
               group_desc = group_desc,
               commodity_desc = commodity_desc,
               short_desc = short_desc,
               domain_desc = domain_desc,
               domaincat_desc = domaincat_desc,
               agg_level_desc = agg_level_desc,
               statisticcat_desc = statisticcat_desc,
               state_name = state_name,
               asd_desc = asd_desc,
               county_name = county_name,
               region_desc = region_desc,
               zip_5 = zip_5,
               watershed_desc = watershed_desc,
               freq_desc = freq_desc,
               reference_period_desc = reference_period_desc,
               format = format)
  
  # Arguments to upper case
  args <- lapply(args, function(x) if (!is.null(x)) toupper(x))
  
  count <- do.call(nass_count, append(args, list(year = year)))
  
  if (count > 50000) stop(paste0("Query returns ",
                                 prettyNum(count, big.mark = ","),
                                 " records. The limit is 50,000. Subset the ",
                                 "query to fit within limit. See nass_count()"))
  
  # Conditional year values
  if (!punct) {
    args <- append(args, list(year = year))
  } else if (punct) {
    # __LE = <= 
    # __LT = < 
    # __GT = > 
    # __GE = >= 
    # __LIKE = like 
    # __NOT_LIKE = not like 
    # __NE = not equal 
    if (grepl("^=<|^<=", year) | grepl("=>$|>=$", year)) {
      args <- append(args, list(year__LE = punct_year))
    }
    if ((grepl("^<", year) | grepl(">$", year)) & !grepl("=", year)) {
      args <- append(args, list(year__LT = punct_year))
    }
    if (grepl("^=>|^>=", year) | grepl("=<$|<=$", year)) {
      args <- append(args, list(year__GE = punct_year))
    }
    if ((grepl("^>", year) | grepl("<$", year)) & !grepl("=", year)) {
      args <- append(args, list(year__GT = punct_year))
    }
  }
  
  base_url <- paste0("http://quickstats.nass.usda.gov/api/api_GET/?key=",
                     token, "&")

  full_url <- httr::modify_url(base_url, query = args)
  temp     <- httr::GET(full_url)
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
