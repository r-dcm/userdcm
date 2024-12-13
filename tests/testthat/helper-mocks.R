local_target_repo_spec <- function(spec, .env = rlang::caller_env()) {
  local_mocked_bindings(target_repo_spec = function(...) spec, .env = .env)
}
