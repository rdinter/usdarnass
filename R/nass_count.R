#' @title Get number of observations from Quick Stats query
#' @description Checks the number of observations that will be returned in a
#'   data request. All queries to the Quick Stats are limited to 50,000
#'   observations. This is a helpful function in determining how to structure a
#'   data request to fit within the 50,000 limit.
#' @param source_desc "Program" - Source of data ("CENSUS" or "SURVEY"). Census
#'   program includes the Census of Ag as well as follow up projects. Survey
#'   program includes national, state, and county surveys.
#' @param sector_desc "Sector" - Five high level, broad categories useful to
#'   narrow down choices. ("ANIMALS & PRODUCTS", "CROPS", "DEMOGRAPHICS",
#'   "ECONOMICS", or "ENVIRONMENTAL")
#' @param group_desc "Group" - Subsets within sector (e.g., under sector_desc =
#'   "CROPS", the groups are "FIELD CROPS", "FRUIT & TREE NUTS", "HORTICULTURE",
#'   and "VEGETABLES").
#' @param commodity_desc "Commodity" - The primary subject of interest (e.g.,
#'   "CORN", "CATTLE", "LABOR", "TRACTORS", "OPERATORS").
#' @param short_desc "Data Item" - A concatenation of six columns:
#'   commodity_desc, class_desc, prodn_practice_desc, util_practice_desc,
#'   statisticcat_desc, and unit_desc.
#' @param domain_desc "Domain" - Generally another characteristic of operations
#'   that produce a particular commodity (e.g., "ECONOMIC CLASS", "AREA
#'   OPERATED", "NAICS CLASSIFICATION", "SALES"). For chemical usage data, the
#'   domain describes the type of chemical applied to the commodity. The
#'   domain_desc = "TOTAL" will have no further breakouts; i.e., the data value
#'   pertains completely to the short_desc.
#' @param domaincat_desc "Domain Category" - Categories or partitions within a
#'   domain (e.g., under domain_desc = "SALES", domain categories include $1,000
#'   TO $9,999, $10,000 TO $19,999, etc).
#' @param agg_level_desc "Geographic Level" - Aggregation level or geographic
#'   granularity of the data. ("AGRICULTURAL DISTRICT", "COUNTY",
#'   "INTERNATIONAL", "NATIONAL", "REGION : MULTI-STATE", "REGION : SUB-STATE",
#'   "STATE", "WATERSHED", or "ZIP CODE")
#' @param statisticcat_desc "Category" - The aspect of a commodity being
#'   measured (e.g., "AREA HARVESTED", "PRICE RECEIVED", "INVENTORY", "SALES").
#' @param state_name "State" - State full name.
#' @param asd_desc "Ag District" - Ag statistics district name.
#' @param county_name "County" - County name.
#' @param region_desc "Region" - NASS defined geographic entities not readily
#'   defined by other standard geographic levels. A region can be a less than a
#'   state (SUB-STATE) or a group of states (MULTI-STATE), and may be specific
#'   to a commodity.
#' @param zip_5 "Zip Code" - US Postal Service 5-digit zip code.
#' @param watershed_desc "Watershed" - Name assigned to the HUC.
#' @param year "Year" - The numeric year of the data and can be either a
#'   character or numeric vector. Conditional values are also possible, for
#'   example a character vector of ">=1999" of "1999<=" will give years greater
#'   than or equal to 1999. Right now the logical values can either be
#'   greater/less than or equal to with the logical at either the beginning or
#'   end of a string with the year.
#' @param freq_desc "Period Type" - Length of time covered ("ANNUAL", "SEASON",
#'   "MONTHLY", "WEEKLY", "POINT IN TIME"). "MONTHLY" often covers more than one
#'   month. "POINT IN TIME" is as of a particular day.
#' @param reference_period_desc "Period" - The specific time frame, within a
#'   freq_desc.
#' @param token API key, default is to use the value stored in \code{.Renviron}
#'   which is stored from the \code{\link{nass_set_key}} function. If there is
#'   no API key stored in the environment, a character string can be provided.
#' @param \\dots Not used.
#' @return Number of observations.
#' @export nass_count
#' @examples
#'
#' \dontrun{
#' # Determine all the observations in NASS
#' nass_count()
#' }
#'
#' \dontrun{
#' # Find the number of observations for Wake County in North Carolina
#' nass_count(state_name = "NORTH CAROLINA", county_name = "WAKE")
#' }

nass_count <- function(source_desc = NULL,
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
                       token = NULL, ...){
  match.call(expand.dots = T)

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
  
  base_url <- paste0("http://quickstats.nass.usda.gov/api/get_counts/?key=",
                     token, "&")
  full_url <- httr::modify_url(base_url, query = args)
  temp     <- httr::GET(full_url)
  tt       <- check_response(temp)

  if (names(tt) == "count"){
    count_data <- as.numeric(tt[["count"]])
    } else {
      stop("Parameter entered is not valid")
      }

  return(count_data)
}
