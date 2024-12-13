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
    Package = project_name(),
    github_spec = target_repo_spec(ask = FALSE)
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

#' @export
#' @rdname rdcm
use_rdcm_issue_template <- function() {
  use_dot_github()
  use_directory(path(".github", "ISSUE_TEMPLATE"))
  use_template(
    "rdcm-issue.md",
    path(".github", "ISSUE_TEMPLATE", "issue_template.md"),
    package = "userdcm"
  )
}

#' @export
#' @rdname rdcm
use_rdcm_github <- function() {
  use_dot_github()
  use_rdcm_contributing()
  use_rdcm_issue_template()
  use_rdcm_coc()
}

#' @export
#' @rdname rdcm
use_rdcm_pkgdown <- function() {
  tr <- target_repo(github_get = TRUE, ok_configs = c("ours", "fork"))
  check_can_push(tr = tr, "to turn on GitHub Pages")

  use_pkgdown()
  site <- use_github_pages()
  use_github_action("pkgdown")

  site_url <- rdcm_url(url = site$html_url, tr = tr)
  use_pkgdown_url(url = site_url, tr = tr)

  if (is_in_rdcm_org()) {
    proj_desc_field_update("Config/Needs/website", "r-dcm/rdcmtemplate",
                           append = TRUE)
  }
}



rdcm_url <- function(url, tr = NULL) {
  tr <- tr %||% target_repo(github_get = TRUE)
  if (!rlang::is_interactive() ||
        !tr$repo_owner %in% c("r-dcm")) {
    return(url)
  }

  custom_url <- glue::glue("https://{tr$repo_name}.{tr$repo_owner}.org")
  if (grepl(glue::glue("{custom_url}/?"), url)) {
    return(url)
  }
  if (ui_yep(c(
    "i" = "{.val {tr$repo_name}} is owned by the {.val {tr$repo_owner}} GitHub
           organization.",
    " " = "Shall we configure {.val {custom_url}} as the (eventual) pkgdown URL?"
  ))) {
    custom_url
  } else {
    url
  }
}

is_in_rdcm_org <- function() {
  if (!is_package()) {
    return(FALSE)
  }
  desc <- proj_desc()
  urls <- desc$get_urls()
  dat <- parse_github_remotes(urls)
  dat <- dat[dat$host == "github.com", ]
  purrr::some(dat$repo_owner, ~ .x %in% c("r-dcm"))
}
