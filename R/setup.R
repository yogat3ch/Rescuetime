#' @title Setup Rescuetime by adding the RESCUETIME_KEY environmental variable
#' @description
#' @param key Your key. See \href{https://www.rescuetime.com/anapi/manage}{API Key Management}
#' @param .Renviron_path Path to the _.Renviron_ file to modify. Defaults to current directory. If one does not exist it will be created.
#' @return Messages or warnings as is suitable
#' @export
#'
#' @examples
#' setup(
#' "fake_key",
#' # to set a user level environmental variable (otherwise the default working_directory is used)
#' .Renviron_path = "~/.Renviron"
#' )
setup <- function(key = "[Your Key]", .Renviron_path = ".Renviron") {
  key_val <- "RESCUETIME_KEY"
  if (!file.exists(.Renviron_path)) {
    file.create(.Renviron_path)
    rlang::warn(paste0(.Renviron_path, " created"))
  }

  if (nchar(Sys.getenv(key_val)) == 0) {
    write(paste0(key_val," = '",key,"'"), .Renviron_path, append = TRUE)
    readRenviron(.Renviron_path)
    if (nchar(Sys.getenv(key_val)) != 0)
      message("Key successfully set.")
  } else
    rlang::warn(paste0(key_val, " already set."))

}
