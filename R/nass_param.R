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
                       key = NULL){
  
  # Pass the arguments through formatting
  calls      <- match.call(expand.dots = TRUE)
  calls[[1]] <- as.name("args_list")
  arguments  <- eval.parent(calls)
  
  
  base_url <- paste0("http://quickstats.nass.usda.gov/api/get_param_values/")
  temp_url <- httr::modify_url(base_url, query = arguments)

  if (!is.null(param)) {
    temp     <- httr::GET(temp_url)
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
