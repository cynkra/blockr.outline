# CI hardening for the shinytest2 e2e (mirrors blockr.dock's setup). Chromote's
# 10s default command timeout is tight on a loaded runner; raise it. At the end
# of the test run, close the browser and sweep the chromium temp profiles it
# leaves behind, which otherwise trip the R CMD check "detritus in the temp
# directory" NOTE.
options(chromote.timeout = 30)

withr::defer(
  {
    if (requireNamespace("chromote", quietly = TRUE)) {
      try(chromote::default_chromote_object()$close(), silent = TRUE)
    }
    leftovers <- list.files(
      tempdir(),
      pattern = "^(com\\.google\\.Chrome|org\\.chromium\\.Chromium|chromote)",
      full.names = TRUE
    )
    unlink(leftovers, recursive = TRUE, force = TRUE)
  },
  envir = testthat::teardown_env()
)
