test_that("use_rdcm_GITHUB-STUFF() adds and Rbuildignores files", {
  rlang::local_interactive(FALSE)
  local_target_repo_spec("OWNER/REPO")

  create_local_package()
  use_git()
  use_rdcm_contributing()
  use_rdcm_issue_template()
  use_rdcm_coc()
  expect_proj_file(".github/CONTRIBUTING.md")
  expect_proj_file(".github/ISSUE_TEMPLATE/issue_template.md")
  expect_proj_file(".github/CODE_OF_CONDUCT.md")
  expect_true(is_build_ignored("^\\.github$"))
})

test_that("use_rdcm_github() adds and Rbuildignores files", {
  rlang::local_interactive(FALSE)
  local_target_repo_spec("OWNER/REPO")

  create_local_package()
  use_git()
  use_rdcm_github()
  expect_proj_file(".github/CONTRIBUTING.md")
  expect_proj_file(".github/ISSUE_TEMPLATE/issue_template.md")
  expect_proj_file(".github/CODE_OF_CONDUCT.md")
  expect_true(is_build_ignored("^\\.github$"))
})
