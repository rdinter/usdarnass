# Argument List as defaulted null ....
args_list <- function(key = NULL,
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
                      format = NULL, ...) {
  
  # Check to see if year used a logical operator
  year  <- trimws(year)
  punct <- grepl("[[:punct:]]", year)
  if (length(punct) == 0) punct <- FALSE
  punct_year <- as.numeric(gsub("[[:punct:]]", "", year))
  
  args <- list(key = key,
               source_desc = source_desc,
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
  
  # Expand arguments if there is more than one parameter
  arg_stack <- suppressWarnings(stack(args))
  args      <- as.list(setNames(arg_stack$values, arg_stack$ind))
  
  return(args)
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

