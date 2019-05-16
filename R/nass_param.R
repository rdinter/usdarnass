#' @title Get all possible values of a parameter
#' @description All possible values of a parameter for a given query. Helps to
#'   understand the columns of data.frame from \code{\link{nass_data}}.
#' @param param A valid parameter value. Available names are: source_desc,
#'   sector_desc, group_desc, commodity_desc, short_desc, domain_desc,
#'   domaincat_desc, agg_level_desc, statisticcat_desc, state_name, asd_desc,
#'   county_name, region_desc, zip_5, watershed_desc, year, freq_desc, and
#'   reference_period_desc.
#' @inheritParams nass_count
#' @return Character vector of all possible parameter values.
#' @export nass_param
#' @examples
#'
#' \dontrun{
#' # Return the program sources for data
#' nass_param("source_desc")
#' }
#'
#' \dontrun{
#' # Return the group categories available in the CROPS sector
#' nass_param("group_desc", sector_desc = "CROPS")
#' }

nass_param <- function(param = NULL,
                       source_desc = NULL,
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
                       token = NULL){
  
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
               reference_period_desc = reference_period_desc)
  
  # Arguments to upper case
  args <- lapply(args, function(x) if (!is.null(x)) toupper(x))
  
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
  
  base_url <- paste0("http://quickstats.nass.usda.gov/api/get_param_values/",
                     "?key=", token, "&")
  temp_url <- httr::modify_url(base_url, query = args)

  if (!is.null(param)) {
    full_url <- paste0(temp_url, "&param=", tolower(param))
    temp     <- httr::GET(full_url)
    tt       <- check_response(temp)

    if (names(tt) == param) {
      param_data <- as.character(unlist(tt))
      } else {
        stop("Parameter entered is not valid")
        }
    }  else{
      stop("Please enter a parameter category")
    }

  return(param_data)
}
