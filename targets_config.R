TAR_MAKE_WRAPPER <- function(...) {
  targets::tar_make(
    ...,
    reporter = "verbose",
    callr_arguments = list(
      "stderr" = "TargetsLog.md",
      "stdout" = "Targetsout.md"
    )
  )
}