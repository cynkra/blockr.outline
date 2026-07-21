# Test helpers for the browser-driven (shinytest2) e2e tests. Mirrors the
# blockr.viz / blockr.dock helper: this container ships system chromium whose
# kernel does not support the sandbox, so point chromote at it with
# --no-sandbox. Process-global and harmless when a normal browser is present.
configure_chromote <- function() {
  if (!nzchar(Sys.getenv("CHROMOTE_CHROME"))) {
    for (cand in c("/usr/bin/chromium", "/usr/bin/chromium-browser",
                   "/usr/bin/google-chrome")) {
      if (file.exists(cand)) {
        Sys.setenv(CHROMOTE_CHROME = cand)
        break
      }
    }
  }
  if (requireNamespace("chromote", quietly = TRUE)) {
    try(
      chromote::set_chrome_args(
        c("--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage")
      ),
      silent = TRUE
    )
  }
  invisible(NULL)
}

# TRUE only if a headless chromium can actually launch here. Used to skip the
# e2e test where no browser exists rather than fail.
chromote_works <- function() {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    return(FALSE)
  }
  configure_chromote()
  ok <- tryCatch({
    sess <- chromote::ChromoteSession$new()
    on.exit(try(sess$close(), silent = TRUE), add = TRUE)
    sess$Page$navigate("about:blank")
    TRUE
  }, error = function(e) FALSE)
  isTRUE(ok)
}
