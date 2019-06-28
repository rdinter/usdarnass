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
                       key = NULL){
  
  key <- check_key(key)
  
  calls <- as.list(match.call(expand.dots = FALSE)[-1])
  calls[["key"]] <- key
  
  # Pass the arguments through formatting
  args <- do.call(args_list, calls)
  
  
  base_url <- paste0("http://quickstats.nass.usda.gov/api/get_param_values/")
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
