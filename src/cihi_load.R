# ------------------------------------------------------------------------------
# Loads CIHI formulary coverage data
# ------------------------------------------------------------------------------

# Downloads CIHI formulary coverage data
.cihi_download <- function(data_dir, destfile) {
  cihi_base_url <- "https://www.cihi.ca/sites/default/files/document/formulary-coverage-data-tool-data-table-en.xlsx"
  
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Download
  response <- httr::GET(cihi_base_url, httr::write_disk(destfile, overwrite = TRUE))
  httr::stop_for_status(response)
  retrieved_at <- Sys.time()
  
  # Extract last updated date from cover page
  cover_page <- readxl::read_excel(destfile, sheet = 1, col_names = FALSE)
  last_updated <- cover_page %>%
    unlist(use.names = FALSE) %>%
    as.character() %>%
    na.omit() %>%
    stringr::str_extract("(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{4}") %>%
    na.omit() %>%
    unique() %>%
    lubridate::my() %>%
    min() %>%
    format("%Y-%m")
  
  # Save source and artifact metadata beside the downloaded data
  write_provenance(
    list(
      dataset = "cihi",
      source = list(
        name = "CIHI Formulary Coverage in the Pharmaceutical Data Tool",
        url = cihi_base_url,
        last_updated = as.character(last_updated),
        last_updated_precision = "month",
        last_updated_method = "Month and year reported on the workbook cover sheet"
      ),
      retrieval = list(
        retrieved_at = format(retrieved_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        timestamp_precision = "second",
        artifacts = list(
          list(
            name = basename(destfile),
            url = cihi_base_url,
            sha256 = digest::digest(destfile, algo = "sha256", file = TRUE)
          )
        )
      )
    ),
    file.path(data_dir, "provenance.yml")
  )
}

# Loads CIHI formulary coverage data and finds last updated date
cihi_load <- function(data_dir = "data/", download = TRUE) {
  destfile <- file.path(data_dir, paste0("formulary_coverage_data.xlsx"))
  
  if (download) {
    .cihi_download(data_dir = data_dir, destfile = destfile)
  }
  stopifnot(dir.exists(data_dir))
  
  data <- readxl::read_excel(destfile, sheet = 2, skip = 1) %>%
    dplyr::slice(-dplyr::n())
  
  provenance_path <- file.path(data_dir, "provenance.yml")
  last_updated <- tryCatch(
    read_provenance_field(provenance_path, "source", "last_updated"),
    error = function(e) NA_character_
  )
  
  cihi <- list(data = data, last_updated = last_updated)
  return(cihi)
}
