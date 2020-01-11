

#' Download Zipped Data Files
#'
#' @param url URL to download data from
#' @param dir_name directory name to save output under in `data/`
#'
#' @return Nothing
#' @export
#'
#' @examples
#' 
#' download_data(url = "https://dataverse.harvard.edu/api/access/datafile/2725073",
#' dir_name = "wings")
download_data <- function(url, dir_name){
  # If directory already exists just issues warning - which we suppress
  dir.create("data/", showWarnings = FALSE)
  # Name of ZIP file
  zip_name <- paste0("data/", dir_name, ".zip")
  # Download
  download.file(url,
                destfile = zip_name)
  # Unzip
  unzip(zip_name,
        exdir = paste0("data/", dir_name, "/"),
        overwrite = TRUE)
  # Remove zipped folder
  file.remove(zip_name)
  # Create a results directory 
  dir.create(paste0("results/", dir_name, "/"),
             recursive = TRUE,
             showWarnings = FALSE)
}