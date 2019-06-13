#' @title Set a Quick Stats API key
#' @description This function will add your Quick Stats API key to your
#'   \code{.Renviron} file so it can be called securely without being stored in
#'   your code. After you have installed your key, it can be called any time by
#'   typing \code{Sys.getenv("NASS_KEY")} and can be used in package functions
#'   by simply typing NASS_KEY. If you do not have an \code{.Renviron} file, the
#'   function will create on for you. If you already have an \code{.Renviron}
#'   file, the function will append the key to your existing file, while making
#'   a backup of your original file for disaster recovery purposes.
#' @param key The API key provided to you from NASS formatted in quotes. A key
#'   can be acquired at \url{https://quickstats.nass.usda.gov/api}
#' @param overwrite If this is set to TRUE, it will write an existing
#'   NASS_KEY that you already have in your \code{.Renviron} file.
#' @export nass_set_key
#' @examples
#'
#' \dontrun{
#' set_nass_key("abcd012345678901234567890123456789")
#' # First time, relead your enviornment so you can use the key without
#' # restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("NASS_KEY")
#' }
#'
#' \dontrun{
#' # If you need to overwrite an existing key:
#' nass_set_key("abcd012345678901234567890123456789", overwrite = TRUE)
#' # First time, relead your enviornment so you can use the key without
#' # restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("NASS_KEY")
#' }

nass_set_key <- function(key = NULL, overwrite = FALSE) {
  key_env <- Sys.getenv("NASS_KEY")
  
  if (isFALSE(overwrite)) {
    
    if (is.null(key)) {
      if (key_env == "") {
        stop("need an API key to query Quick Stats, see ?nass_set_key")
      } else {
        return(key_env)
      }
    } else if (nchar(key) == 36) {
      # Key must be 36 characters in length, if not then it will not be valid
      Sys.setenv(NASS_KEY = key)
    } else {
      stop(paste0("please enter a valid API key to query Quick Stats,",
                  " see ?nass_set_key for details"))
    }
    
  } else if (isTRUE(overwrite)) {
    
    if (is.null(key)) {
      if (key_env == "") {
        stop("need an API key to query Quick Stats, see ?nass_set_key")
      } else {
        return(key_env)
      }
    } else if (nchar(key) == 36) {
      # Install into the main R profile
      if (file.exists(paste0(Sys.getenv("HOME"),"/.Renviron"))) {
        # Backup original .Renviron just in case
        file.copy(paste0(Sys.getenv("HOME"),"/.Renviron"),
                  paste0(Sys.getenv("HOME"),"/.Renviron_backup"))
      } else if (!file.exists(paste0(Sys.getenv("HOME"),"/.Renviron"))) {
        file.create(paste0(Sys.getenv("HOME"),"/.Renviron"))
      }
      message("Your original .Renviron will be backed up and stored in your ",
              "R HOME directory if needed.")
      oldenv <- utils::read.table(paste0(Sys.getenv("HOME"),"/.Renviron"),
                                  stringsAsFactors = FALSE)
      newenv <- oldenv[-grep("NASS_KEY", oldenv),]
      upenv <- append(newenv, paste("NASS_KEY=", "'", key, "'", sep = ""))
      
      utils::write.table(upenv, paste0(Sys.getenv("HOME"),"/.Renviron"),
                         quote = FALSE, sep = "\n",
                         col.names = FALSE, row.names = FALSE)
      message(paste0('Your API key has been stored in your .Renviron and can',
                     ' be accessed by Sys.getenv("NASS_KEY")'))
      
      Sys.setenv(NASS_KEY = key)
    } else {
      stop("please enter a valid API key to query Quick Stats, see ?nass_set_key")
    }
    
    
  }
  
  return(key)
}

