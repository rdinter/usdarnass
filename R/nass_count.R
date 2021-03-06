#' @title Get number of observations from Quick Stats query
#' @description Checks the number of observations that will be returned in a
#'   data request. All queries to the Quick Stats are limited to 50,000
#'   observations. This is a helpful function in determining how to structure a
#'   data request to fit within the 50,000 limit.
#'
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
#' @param class_desc Generally a physical attribute (e.g., variety, size, color,
#'   gender) of the commodity.
#' @param prodn_practice_desc A method of production or action taken on the
#'   commodity (e.g., IRRIGATED, ORGANIC, ON FEED).
#' @param util_practice_desc  Utilizations (e.g., GRAIN, FROZEN, SLAUGHTER) or
#'   marketing channels (e.g., FRESH MARKET, PROCESSING, RETAIL).
#' @param statisticcat_desc "Category" - The aspect of a commodity being
#'   measured (e.g., "AREA HARVESTED", "PRICE RECEIVED", "INVENTORY", "SALES").
#' @param unit_desc The unit associated with the statistic category (e.g.,
#'   ACRES, $ / LB, HEAD, $, OPERATIONS).
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
#' @param state_ansi American National Standards Institute (ANSI) standard
#'   2-digit state codes.
#' @param state_fips_code NASS 2-digit state codes; include 99 and 98 for US
#'   TOTAL and OTHER STATES, respectively; otherwise match ANSI codes.
#' @param state_alpha State abbreviation, 2-character alpha code.
#' @param state_name "State" - State full name.
#' @param asd_code NASS defined county groups, unique within a state, 2-digit ag
#'   statistics district code.
#' @param asd_desc "Ag District" - Ag statistics district name.
#' @param county_ansi ANSI standard 3-digit county codes.
#' @param county_code NASS 3-digit county codes; includes 998 for OTHER
#'   (COMBINED) COUNTIES and Alaska county codes; otherwise match ANSI codes.
#' @param county_name "County" - County name.
#' @param region_desc "Region" - NASS defined geographic entities not readily
#'   defined by other standard geographic levels. A region can be a less than a
#'   state (SUB-STATE) or a group of states (MULTI-STATE), and may be specific
#'   to a commodity.
#' @param zip_5 "Zip Code" - US Postal Service 5-digit zip code.
#' @param watershed_code US Geological Survey (USGS) 8-digit Hydrologic Unit
#'   Code (HUC) for watersheds.
#' @param watershed_desc "Watershed" - Name assigned to the HUC.
#' @param congr_district_code US Congressional District 2-digit code.
#' @param country_code US Census Bureau, Foreign Trade Division 4-digit country
#'   code, as of April, 2007.
#' @param country_name Country name.
#' @param location_desc Full description for the location dimension.
#' @param year "Year" - The numeric year of the data and can be either a
#'   character or numeric vector. Conditional values are also possible, for
#'   example a character vector of ">=1999" of "1999<=" will give years greater
#'   than or equal to 1999. Right now the logical values can either be
#'   greater/less than or equal to with the logical at either the beginning or
#'   end of a string with the year.
#' @param freq_desc "Period Type" - Length of time covered ("ANNUAL", "SEASON",
#'   "MONTHLY", "WEEKLY", "POINT IN TIME"). "MONTHLY" often covers more than one
#'   month. "POINT IN TIME" is as of a particular day.
#' @param begin_code If applicable, a 2-digit code corresponding to the
#'   beginning of the reference period (e.g., for freq_desc = MONTHLY,
#'   begin_code ranges from 01 (January) to 12 (December)).
#' @param end_code If applicable, a 2-digit code corresponding to the end of the
#'   reference period (e.g., the reference period of JAN THRU MAR will have
#'   begin_code = 01 and end_code = 03).
#' @param reference_period_desc "Period" - The specific time frame, within a
#'   freq_desc.
#' @param week_ending Week ending date, used when freq_desc = WEEKLY.
#' @param key API key, default is to use the value stored in \code{.Renviron}
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
                       key = check_key(), ...){
  
  # Pass the arguments through formatting
  calls      <- match.call(expand.dots = TRUE)
  calls[[1]] <- as.name("args_list")
  arguments  <- eval.parent(calls)
  
  temp     <- httr::GET("http://quickstats.nass.usda.gov/api/get_counts/",
                        query = arguments)
  tt       <- check_response(temp)

  if (names(tt) == "count") {
    count_data <- as.numeric(tt[["count"]])
    } else {
      stop("Parameter entered is not valid")
      }

  return(count_data)
}
