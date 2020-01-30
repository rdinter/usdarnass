#' @title Combine API arguments for query
#' @description Take a character vector for each argument and concatenate them
#'   for API calls to Quick Stats. Arguments can be single objects or a vector
#'   and are not case sensitive.
#' @inheritParams nass_count
#' @inheritParams nass_param
#' @param format Output format from API call. Defaults to CSV as it is typically
#'   the smallest sized call. Other options are JSON and XML but these are not
#'   recommended. XML currently not supported.
#' @param \\dots Not used.
#' @return A list containing arguments for Quick Stats API call
#' @export args_list
#' @examples
#'
#' \dontrun{
#' args_list(year = 2012,
#' short_desc = "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $",
#' county_name = c("Durham", "WAKE"),
#' state_name = "NORTH CAROLINA")
#' }

args_list <- function(key = NULL,
                      param = NULL,
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
                      format = NULL,
                      ...) {
  key <- check_key(key)
  
  arguments <- list(key = key,
               param = param,
               source_desc = source_desc,
               sector_desc = sector_desc,
               group_desc = group_desc,
               commodity_desc = commodity_desc,
               class_desc = class_desc,
               prodn_practice_desc = prodn_practice_desc,
               util_practice_desc = util_practice_desc,
               statisticcat_desc = statisticcat_desc,
               unit_desc = unit_desc,
               short_desc = short_desc,
               domain_desc = domain_desc,
               domaincat_desc = domaincat_desc,
               agg_level_desc = agg_level_desc,
               state_ansi = state_ansi,
               state_fips_code = state_fips_code,
               state_alpha = state_alpha,
               state_name = state_name,
               asd_code = asd_code,
               asd_desc = asd_desc,
               county_ansi = county_ansi,
               county_code = county_code,
               county_name = county_name,
               region_desc = region_desc,
               zip_5 = zip_5,
               watershed_code = watershed_code,
               watershed_desc = watershed_desc,
               congr_district_code = congr_district_code,
               country_code = country_code,
               country_name = country_name,
               location_desc = location_desc,
               freq_desc = freq_desc,
               begin_code = begin_code,
               end_code = end_code,
               reference_period_desc = reference_period_desc,
               week_ending = week_ending,
               format = format)
  
  # Arguments to upper case
  arguments <- lapply(arguments, function(x) if (!is.null(x)) toupper(x))
  
  # param needs to be lowercase
  if (!is.null(arguments[["param"]])) {
    arguments[["param"]] <- tolower(arguments[["param"]])
    }
  
  # Conditional year values
  arguments <- append(arguments, conditional_years(year))
  
  # Expand arguments if there is more than one parameter
  arg_stack <- suppressWarnings(utils::stack(arguments))
  arguments <- as.list(stats::setNames(arg_stack$values, arg_stack$ind))
  
  return(arguments)
}

# Conditional year values
conditional_years <- function(x) {
  # Check to see if year used a logical operator
  years <- trimws(x)
  punct <- grepl("[[:punct:]]", years)
  punct_years <- as.numeric(gsub("[[:punct:]]", "", years))
  
  # Conditional year values
  # __LE = <= 
  # __LT = < 
  # __GT = > 
  # __GE = >= 
  # __LIKE = like 
  # __NOT_LIKE = not like 
  # __NE = not equal 
  year     <- punct_years[!punct]
  year__LE <- punct_years[(grepl("^=<|^<=", years) | grepl("=>$|>=$", years))]
  year__LT <- punct_years[((grepl("^<", years) | grepl(">$", years)) &
                             !grepl("=", years))]
  year__GE <- punct_years[(grepl("^=>|^>=", years) | grepl("=<$|<=$", years))]
  year__GT <- punct_years[((grepl("^>", years) | grepl("<$", years)) &
                             !grepl("=", years))]
  
  results <- list(year = year,
                  year__LE = year__LE,
                  year__LT = year__LT,
                  year__GE = year__GE,
                  year__GT = year__GT)
  return(results)
}


# Check if there is an API key available on the system
check_key <- function(x){
  tmp <- if (is.null(x)) Sys.getenv("NASS_KEY") else x
  if (tmp == "") {
    getOption("nasskey",
              stop("need an API key to query Quick Stats, see ?nass_set_key"))
  } else tmp
}


# Check GET resposne from the page, decide if "count" (number) or "data"
check_response <- function(x){
  if (x[["status_code"]] == 400) {
    warning(paste0("Bad request - invalid query: error ", x[["status_code"]],
                   ". There is an error in the query string, such as wrong ",
                   "parameter name or value."))
  } else if (x[["status_code"]] == 413) {
    warning(paste0("Exceeds limit = 50,000: error ", x[["status_code"]],
                   ". The request would return more than 50,000 records."))
  } else if (x[["status_code"]] == 415) {
    warning(paste0("Bad request - unsupport media type: error ",
                   x[["status_code"]],
                   ". The request format parameter is not JSON or CSV or XML."))
  } else if (x[["status_code"]] == 401) {
    warning(paste0("Unauthorized: error ", x[["status_code"]],
                   ". There is no key or invalid key parameter."))
  } else if (x[["status_code"]] == 200) {
# NO ERRORS
    if (x[["headers"]][["content-type"]] == "application/json") {
# JSON COUNT OR PARAM
      if (names(httr::content(x)) != "data") {
        res <- httr::content(x, as = "text", encoding = "UTF-8")
        out <- jsonlite::fromJSON(res, simplifyVector = FALSE)
      } else if (names(httr::content(x)) == "data") {
# JSON DATA
        res <- httr::content(x, as = "text", encoding = "UTF-8")
        out <- jsonlite::fromJSON(res)
        out <- out[["data"]]
      } else {
        warning("Unknown JSON error.")
      }
    } else if (x[["headers"]][["content-type"]] == "text/xml; charset=UTF-8") {
# XML DATA, not yet supported
      out <- "XML"
      # out <- names(httr::content(x))
    } else if (x[["headers"]][["content-type"]] == "text/csv; charset=UTF-8") {
# CSV DATA
      out <- httr::content(x, col_types = readr::cols(.default = "c"))
      out <- as.data.frame(out)
    } else {
      warning("Unknown data request error.")
    }
    return(out)
  }
}

