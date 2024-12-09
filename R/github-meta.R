#' Helpers for r-dcm development
#'
#' These helpers are adapted from [usethis::tidyverse] to apply common
#' conventions to r-dcm packages.
#'
#' @details
#'
#' * `use_rdcm_contributing()`: adds standard r-dcm contributing guidelines.
#'
#' * `use_rdcm_coc()`: equivalent to [usethis::use_code_of_conduct()], but puts
#'    the document in a `.github/` subdirectory.
#'
#' @name rdcm
NULL

#' @export
#' @rdname rdcm
use_rdcm_contributing <- function() {
  use_dot_github()
  data <- list(
    Package = project_name()
  )
  use_template(
    "rdcm-contributing.md",
    fs::path(".github", "CONTRIBUTING.md"),
    data = data,
    package = "userdcm"
  )
}

#' @export
#' @rdname rdcm
use_rdcm_coc <- function() {
  old_top_level_coc <- proj_path(c("CODE_OF_CONDUCT.md", "CONDUCT.md"))
  old <- fs::file_exists(old_top_level_coc)
  if (any(old)) {
    fs::file_delete(old_top_level_coc[old])
  }

  use_dot_github()
  use_code_of_conduct(contact = "wjakethompson@gmail.com", path = ".github")
}


# Github utilities -------------------------------------------------------------
use_dot_github <- function(ignore = TRUE) {
  use_directory(".github", ignore = ignore)
  use_git_ignore("*.html", directory = ".github")
}

project_name <- function(base_path = proj_get()) {
  if (is_package(base_path)) {
    proj_desc(base_path)$get_field("Package")
  } else {
    fs::path_file(base_path)
  }
}

proj_desc <- function(path = proj_get()) {
  desc::desc(file = path)
}

is_package <- function(base_path = proj_get()) {
  res <- tryCatch(
    rprojroot::find_package_root_file(path = base_path),
    error = function(e) NULL
  )
  !is.null(res)
}

proj_find <- function(path = ".") {
  tryCatch(
    rprojroot::find_root(proj_crit(), path = path),
    error = function(e) NULL
  )
}

read_utf8 <- function(path, n = -1L) {
  base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

proj <- new.env(parent = emptyenv())

proj_get_ <- function() proj$cur

proj_crit <- function() {
  rprojroot::has_file(".here") |
    rprojroot::is_rstudio_project |
    rprojroot::is_r_package |
    rprojroot::is_git_root |
    rprojroot::is_remake_project |
    rprojroot::is_projectile_project
}
