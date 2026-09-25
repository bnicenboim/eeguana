## Takes a screenshot of eeg_browse() while the vignette is built, so that it
## always shows the current app. eeg_browse() runs as usual, but the viewer
## that would open it in a browser opens it in headless Chrome instead, in a
## background process that sets the `inputs`, clicks the buttons in `clicks`,
## saves the screenshot, and clicks "Done". Needs callr, shinytest2, and
## chromote, and Chrome or chromote's chrome-headless-shell (see
## ?chromote::chrome_versions_add).
browse_screenshot <- function(data, file, inputs = list(), clicks = character(0),
                              width = 1400, height = 900, timeout = 300) {
  ## shinytest2 does not overwrite the screenshot of an earlier build
  unlink(file)
  driver <- NULL
  viewer <- function(url) {
    driver <<- callr::r_bg(function(url, file, inputs, clicks, width, height) {
      if (is.null(suppressMessages(chromote::find_chrome()))) {
        chromote::local_chrome_version("latest-installed", binary = "chrome-headless-shell")
      }
      Sys.setenv(NOT_CRAN = "true")
      app <- shinytest2::AppDriver$new(url,
        width = width, height = height, load_timeout = 60 * 1000, timeout = 60 * 1000
      )
      app$wait_for_idle()
      if (length(inputs) > 0) do.call(app$set_inputs, inputs)
      for (id in clicks) app$click(id)
      app$wait_for_idle()
      app$get_screenshot(file)
      try(app$click("done"), silent = TRUE)
      try(app$stop(), silent = TRUE)
      file
    }, args = list(url, file, inputs, clicks, width, height))
  }
  ## if the background process fails, nothing would click "Done"
  cancel <- later::later(function() shiny::stopApp(), timeout)
  marked <- suppressMessages(eeg_browse(data, .viewer = viewer))
  cancel()
  driver$wait()
  driver$get_result() # raises the error of the background process, if any
  marked
}
