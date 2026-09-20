suppressPackageStartupMessages(library(dplyr))

project_root <- here::here()
if (!file.exists(file.path(project_root, "rdinexplore.R"))) {
  stop("Unable to locate the rdinexplore repository root", call. = FALSE)
}

source(file.path(project_root, "src", "atc_explore.R"), local = TRUE)
source(file.path(project_root, "src", "cihi_clean.R"), local = TRUE)
source(file.path(project_root, "src", "dpd_clean.R"), local = TRUE)
source(file.path(project_root, "src", "dpd_explore.R"), local = TRUE)
source(file.path(project_root, "src", "merged_clean.R"), local = TRUE)
source(file.path(project_root, "src", "read_files.R"), local = TRUE)
# The two loader files, cihi_load and dpd_load, are not tested because they
# require network access and external file-format dependencies. The tests above
# use synthetic tibbles and cached ATC data only.
