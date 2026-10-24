source_files <- fs::dir_ls(
  "scripts",
  regexp = "\\d+.*\\.R$"
)

purrr::walk(source_files, source)
