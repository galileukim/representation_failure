source_files <- fs::dir_ls(
  "scripts",
  regexp = "\\d+.*\\.R$"
)

source_files <- source_files[fs::path_file(source_files) != "00_main.R"]

purrr::walk(source_files, source)
