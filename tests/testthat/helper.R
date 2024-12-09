## If session temp directory appears to be, or be within, a project, there
## will be large scale, spurious test failures.
## The IDE sometimes leaves .Rproj files behind in session temp directory or
## one of its parents.
## Delete such files manually.
session_temp_proj <- proj_find(path_temp())
if (!is.null(session_temp_proj)) {
  Rproj_files <- fs::dir_ls(session_temp_proj, glob = "*.Rproj")
  cli::cli_bullets(c(
    "x" = "Rproj {cli::qty(length(Rproj_files))} file{?s} found at or above session temp dir:",
    bulletize(usethis_map_cli(Rproj_files)),
    "!" = "Expect this to cause spurious test failures."
  ))
}

create_local_package <- function(dir = file_temp(pattern = "testpkg"),
                                 env = parent.frame(),
                                 rstudio = FALSE) {
  create_local_thing(dir, env, rstudio, "package")
}

create_local_thing <- function(dir = file_temp(pattern = pattern),
                               env = parent.frame(),
                               rstudio = FALSE,
                               thing = c("package", "project")) {
  thing <- match.arg(thing)
  if (fs::dir_exists(dir)) {
    ui_abort("Target {.arg dir} {.path {pth(dir)}} already exists.")
  }

  old_project <- proj_get_() # this could be `NULL`, i.e. no active project
  old_wd <- getwd()          # not necessarily same as `old_project`

  withr::defer(
    {
      cli::cli_bullets(c("Deleting temporary project: {.path {dir}}"))
      fs::dir_delete(dir)
    },
    envir = env
  )
  ui_silence(
    switch(
      thing,
      package = create_package(
        dir,
        # This is for the sake of interactive development of snapshot tests.
        # When the active usethis project is a package created with this
        # function, testthat learns its edition from *that* package, not from
        # usethis. So, by default, opt in to testthat 3e in these ephemeral test
        # packages.
        fields = list("Config/testthat/edition" = "3"),
        rstudio = rstudio,
        open = FALSE,
        check_name = FALSE
      ),
      project = create_project(dir, rstudio = rstudio, open = FALSE)
    )
  )

  withr::defer(proj_set(old_project, force = TRUE), envir = env)
  proj_set(dir)

  withr::defer(
    {
      cli::cli_bullets(c("Restoring original working directory: {.path {old_wd}}"))
      setwd(old_wd)
    },
    envir = env
  )
  setwd(proj_get())

  invisible(proj_get())
}

expect_proj_file <- function(...) expect_true(file_exists(proj_path(...)))

is_build_ignored <- function(pattern, ..., base_path = proj_get()) {
  lines <- read_utf8(path(base_path, ".Rbuildignore"))
  length(grep(pattern, x = lines, fixed = TRUE, ...)) > 0
}
