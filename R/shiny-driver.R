#' Class to manage a shiny app and a phantom.js headless browser
#'
#' @section Usage: \preformatted{app <- ShinyDriver$new(path = ".", loadTimeout
#'   = 5000, checkNames = TRUE, debug = c("none", "all",
#'   ShinyDriver$debugLogTypes), phantomTimeout = 5000, seed = NULL, cleanLogs =
#'   TRUE, shinyOptions = list()), url=NULL) app$stop() app$getDebugLog(type =
#'   c("all", ShinyDriver$debugLogTypes))
#'
#'   app$getValue(name, iotype = c("auto", "input", "output"))
#'   app$setValue(name, value, iotype = c("auto", "input", "output"))
#'   app$sendKeys(name = NULL, keys)
#'
#'   app$getWindowSize() app$setWindowSize(width, height)
#'
#'   app$getUrl() app$goBack() app$refresh() app$getTitle() app$getSource()
#'   app$takeScreenshot(file = NULL)
#'
#'   app$findElement(css = NULL, linkText = NULL, partialLinkText = NULL, xpath
#'   = NULL)
#'
#'   app$findElements(css = NULL, linkText = NULL, partialLinkText = NULL, xpath
#'   = NULL)
#'
#'   app$waitFor(expr, checkInterval = 100, timeout = 3000)
#'
#'   app$waitForValue(name, ignore = list(NULL, ""), iotype = "input", timeout =
#'   10000, checkInterval = 400)
#'
#'   app$listWidgets()
#'
#'   app$checkUniqueWidgetNames()
#'
#'   app$findWidget(name, iotype = c("auto", "input", "output"))
#'
#'   app$expectUpdate(output, ..., timeout = 3000, iotype = c("auto", "input",
#'   "output")) }
#'
#' @section Arguments:
#' \describe{
#'   \item{app}{A \code{ShinyDriver} instance.}
#'   \item{path}{Path to a directory containing a Shiny app, i.e. a
#'      single \code{app.R} file or a \code{server.R} and \code{ui.R}
#'      pair.}
#'   \item{loadTimeout}{How long to wait for the app to load, in ms.
#'      This includes the time to start R.}
#'   \item{phantomTimeout}{How long to wait when connecting to phantomJS
#'      process, in ms.}
#'   \item{checkNames}{Whether to check if widget names are unique in the
#'      app.}
#'   \item{debug}{Whether to start the app in debugging mode. In debugging
#'      mode debug messages are printed to the console.}
#'   \item{seed}{An optional random seed to use before starting the
#'      application. For apps that use R's random number generator, this
#'      can make their behavior repeatable.}
#'   \item{cleanLogs}{Whether to remove the stdout and stderr logs when the
#'     Shiny process object is garbage collected.}
#'   \item{shinyOptions}{A list of options to pass to \code{runApp()}.}
#'   \item{url}{Optional URL where the shiny app is executing.} \item{name}{Name
#'   of a shiny widget. For \code{$sendKeys} it can be \code{NULL}, in which
#'   case the keys are sent to the active HTML element.} \item{iotype}{Type of
#'   the Shiny widget. Usually \code{shinytest} finds the widgets by their name,
#'   so this need not be specified, but Shiny allows input and output widgets
#'   with identical names.} \item{keys}{Keys to send to the widget or the app.
#'   See the \code{sendKeys} method of the \code{webdriver} package.}
#'   \item{width}{Scalar integer, the desired width of the browser window.}
#'   \item{height}{Scalar integer, the desired height of the browser window.}
#'   \item{file}{File name to save the screenshot to. If \code{NULL}, then it
#'   will be shown on the R graphics device.} \item{css}{CSS selector to find an
#'   HTML element.} \item{linkText}{Find \code{<a>} HTML elements based on their
#'   \code{innerText}.} \item{partialLinkText}{Find \code{<a>} HTML elements
#'   based on their \code{innerText}. It uses partial matching.}
#'   \item{xpath}{Find HTML elements using XPath expressions.} \item{expr}{A
#'   string scalar containing JavaScript code that evaluates to the condition to
#'   wait for.} \item{checkInterval}{How often to check for the condition, in
#'   milliseconds.} \item{ignore}{List of possible values that are to not be
#'   considered valid.  \code{app$waitForValue} will continue to poll until it
#'   finds a value not contained in \code{ignore}.} \item{timeout}{Timeout for
#'   the condition, in milliseconds.} \item{output}{Character vector, the
#'   name(s) of the Shiny output widgets that should be updated.}
#'   \item{allowInputNoBinding_}{When setting the value of an input, allow it to
#'   set the value of an input even if that input does not have an input
#'   binding.} \item{...}{For \code{expectUpdate} these can be named arguments.
#'   The argument names correspond to Shiny input widgets: each input widget
#'   will be set to the specified value.} }
#'
#' @section Details:
#'
#'   \code{ShinyDriver$new()} function creates a \code{ShinyDriver} object and
#'   starts a \code{phantomjs} headless browser that connects to the app at
#'   \code{url} if specified, if not, it starts the Shiny app located at
#'   \code{path} in a new R session. (Apps can be defined a single \code{app.R}
#'   file, in a \code{server.R} and \code{ui.R} pair, or a R Markdown
#'   (\code{Rmd}) document.) It then waits until the app is ready to use, at
#'   most \code{loadTimeout} milliseconds.  If the app is not then ready, it
#'   throws an error. You can increase \code{loadTimeout} for slow loading apps.
#'
#'   \code{app$stop()} stops the app, i.e. the external R process that runs the
#'   app, and also the phantomjs instance.
#'
#'   \code{app$getDebugLog()} queries one or more of the debug logs:
#'   \code{shiny_console}, \code{browser} or \code{shinytest}.
#'
#'   \code{app$getValue()} finds a widget and queries its value. See the
#'   \code{getValue} method of the \code{\link{Widget}} class.
#'
#'   \code{app$setInputs()} sets the value of inputs. The arguments must all be
#'   named; an input with each name will be assigned the given value.
#'
#'   \code{app$uploadFile()} uploads a file to a file input. The argument must
#'   be named and the value must be the path to a local file; that file will be
#'   uploaded to a file input with that name.
#'
#'   \code{app$getAllValues()} returns a named list of all inputs, outputs, and
#'   export values.
#'
#'   \code{app$setValue()} finds a widget and sets its value. See the
#'   \code{setValue} method of the \code{\link{Widget}} class.
#'
#'   \code{app$sendKeys} sends the specified keys to the HTML element of the
#'   widget.
#'
#'   \code{app$getWindowSize()} returns the current size of the browser window,
#'   in a list of two integer scalars named \sQuote{width} and \sQuote{height}.
#'
#'   \code{app$setWindowSize()} sets the size of the browser window to the
#'   specified width and height.
#'
#'   \code{app$getUrl()} returns the current URL.
#'
#'   \code{app$goBack()} \dQuote{presses} the browser's \sQuote{back} button.
#'
#'   \code{app$refresh()} \dQuote{presses} the browser's \sQuote{refresh}
#'   button.
#'
#'   \code{app$getTitle()} returns the title of the page. (More precisely the
#'   document title.)
#'
#'   \code{app$getSource()} returns the complete HTML source of the current
#'   page, in a character scalar.
#'
#'   \code{app$takeScreenshot()} takes a screenshot of the current page and
#'   writes it to a file, or (if \code{file} is \code{NULL}) shows it on the R
#'   graphics device. The output file has PNG format.
#'
#'   \code{app$findElement()} find an HTML element on the page, using a CSS
#'   selector or an XPath expression. The return value is an
#'   \code{\link[webdriver]{Element}} object from the \code{webdriver} package.
#'
#'   \code{app$findElements()} finds potentially multiple HTML elements, and
#'   returns them in a list of \code{\link[webdriver]{Element}} objects from the
#'   \code{webdriver} package.
#'
#'   \code{app$waitFor()} waits until a JavaScript expression evaluates to
#'   \code{true}, or a timeout happens. It returns \code{TRUE} is the expression
#'   evaluated to \code{true}, possible after some waiting.
#'
#'   \code{app$waitForValue()} waits until the current application's
#'   \code{input} (or \code{output}) value is not one of the supplied invalid
#'   values.  The function returns the value found if the time limit has not
#'   been reached (default is 10 seconds).  This function can be useful in
#'   helping determine if an application has initialized or finished processing
#'   a complex reactive situation.
#'
#'   \code{app$listWidgets()} lists the names of all input and output widgets.
#'   It returns a list of two character vectors, named \code{input} and
#'   \code{output}.
#'
#'   \code{app$checkUniqueWidgetNames()} checks if Shiny widget names are
#'   unique.
#'
#'   \code{app$findWidget()} finds the corresponding HTML element of a Shiny
#'   widget. It returns a \code{\link{Widget}} object.
#'
#'   \code{expectUpdate()} is one of the main functions to test Shiny apps. It
#'   performs one or more update operations via the browser, and then waits for
#'   the specified output widgets to update. The test succeeds if all specified
#'   output widgets are updated before the timeout. For updates that involve a
#'   lot of computation, you increase the timeout.
#'
#' @name ShinyDriver
#' @examples
#' \dontrun{
#' ## https://github.com/rstudio/shiny-examples/tree/master/050-kmeans-example
#' app <- ShinyDriver$new("050-kmeans-example")
#' expectUpdate(app, xcol = "Sepal.Width", output = "plot1")
#' expectUpdate(app, ycol = "Petal.Width", output = "plot1")
#' expectUpdate(app, clusters = 4, output = "plot1")
#' }
NULL

#' @importFrom R6 R6Class
#' @export
ShinyDriver <- R6Class(
  "ShinyDriver",

  public = list(

    #' @param path Path to a directory containing a Shiny app, i.e. a
    #'   single `app.R` file or a `server.R`-`ui.R` pair.
    #' @param loadTimeout How long to wait for the app to load, in ms.
    #'   This includes the time to start R. Defaults to 5s when running
    #'   locally and 10s when running on CI.
    #' @param phantomTimeout How long to wait when connecting to phantomJS
    #'  process, in ms
    #' @param checkNames Check if widget names are unique?
    #' @param debug Start the app in debugging mode? In debugging mode debug
    #'   messages are printed to the console.
    #' @param seed An optional random seed to use before starting the application.
    #'   For apps that use R's random number generator, this can make their
    #'   behavior repeatable.
    #' @param cleanLogs Whether to remove the stdout and stderr logs when the
    #'     Shiny process object is garbage collected.
    #' @param shinyOptions A list of options to pass to [shiny::runApp()].
    initialize = function(path = ".", loadTimeout = NULL, checkNames = TRUE,
      debug = c("none", "all", shinytest::ShinyDriver$debugLogTypes),
      phantomTimeout = 5000, seed = NULL, cleanLogs = TRUE,
      shinyOptions = list(), url=NULL)
    {
      sd_initialize(self, private, path, loadTimeout, checkNames, debug,
        phantomTimeout = phantomTimeout, seed = seed, cleanLogs = cleanLogs,
        shinyOptions = shinyOptions, url=url)
    },

    #' @description Stop app and clean up logs.
    finalize = function()
      sd_finalize(self, private),

    #' @description
    #' Stop the app, the terminate external R process that runs the app and
    #' the phantomjs instance.
    stop = function()
      sd_stop(self, private),

    #' @description
    #' Finds a widget and queries its value. See the `getValue()` method of
    #' [Widget] for more details.
    getValue = function(name, iotype = c("auto", "input", "output")) {
      "!DEBUG sd_getValue `name` (`iotype`)"
      self$findWidget(name, iotype)$getValue()
    },

    #' @description
    #' Finds a widget and sets its value. It's a shortcut for `findElement()`
    #' plus `setValue()`; see the [Widget] documentation for more details.
    #'
    #' @param value New value.
    #' @return Self, invisibly.
    setValue = function(name, value, iotype = c("auto", "input", "output")) {
      "!DEBUG sd_setValue `name`"
      self$findWidget(name, iotype)$setValue(value)
      invisible(self)
    },

    #' @description
    #' Returns a named list of all inputs, outputs, and export values.
    #'
    #' @param input,output,export Either `TRUE` to return all
    #'   input/output/exported values, or a character vector of specific
    #'   controls.
    #' @exclude Names of objects to exclude from input, output, and export.
    #' @param stop_on_error When `TRUE` (the default) error messages from the
    #'   shiny app are displayed and execution halts. When `FALSE` the HTTP
    #'   error code and message are included in the returned list.
    getAllValues = function(input = TRUE, output = TRUE, export = TRUE,
                            exclude=NULL, stop_on_error=TRUE)
      sd_getAllValues(self, private, input, output, export, exclude,
                      stop_on_error),

    #' @description
    #' Sends the specified keys to specific HTML element. Shortcut for
    #' `findWidget()` plus `sendKeys()`.
    #' @param keys Keys to send to the widget or the app. See [webdriver::key]
    #'   for how to specific special keys.
    #' @return Self, invisibly.
    sendKeys = function(name, keys) {
      "!DEBUG sd_sendKeys `name`"
      self$findWidget(name)$sendKeys(keys)
      invisible(self)
    },

    #' @description
    #' Sets size of the browser window.
    #' @param width,height Height and width of browser, in pixels.
    #' @return Self, invisibly.
    setWindowSize = function(width, height) {
      "!DEBUG sd_setWindowSize `width`x`height`"
      private$web$getWindow()$setSize(width, height)
      invisible(self)
    },

    #' @description
    #' Get current size of the browser window, as list of integer scalars
    #'   named `width` and `height`.
    getWindowSize = function() {
      "!DEBUG sd_getWindowSize"
      private$web$getWindow()$getSize()
    },

    ## Debugging

    #' @description
    #' Query one or more of the debug logs.
    #' @param type Log type: `"all"`, `"shiny_console"`, `"browser"`,
    #'   or `"shinytest"`.
    getDebugLog = function(type = c("all", ShinyDriver$debugLogTypes))
      sd_getDebugLog(self, private, match.arg(type, several.ok = TRUE)),

    #' @description
    #' Enable/disable debugging messages
    #' @param enable New value.
    enableDebugLogMessages = function(enable = TRUE)
      sd_enableDebugLogMessages(self, private, enable),

    ## Event logging

    #' @description Add event to log.
    #' @param event Event name
    #' @param ... Addition data to store for event
    logEvent = function(event, ...)
      sd_logEvent(self, private, event, ...),

    #' @description Retrieve event log.
    getEventLog = function()
      sd_getEventLog(self, private),

    #' @description Get current url
    getUrl = function() {
      "!DEBUG sd_getUrl"
      private$web$getUrl()
    },

    #' @description Get page title
    getTitle = function() {
      "!DEBUG sd_getTitle"
      private$web$getTitle()
    },

    #' @description Get complete source of current page.
    getSource = function() {
      "!DEBUG sd_getSource"
      private$web$getSource()
    },

    #' @description Return to previous page
    #' @return Self, invisibly.
    goBack = function() {
      "!DEBUG sd_goBack"
      private$web$goBack()
      invisible(self)
    },

    #' @description Refresh the browser
    #' @return Self, invisibly.
    refresh = function() {
      "!DEBUG refresh"
      private$web$refresh()
      invisible(self)
    },

    #' @description
    #' Takes a screenshot of the current page and writes it to a PNG file or
    #' shows on current graphics device.
    #' @param file File name to save the screenshot to. If `NULL`, then
    #'   it will be shown on the R graphics device.
    #' @return Self, invisibly.
    takeScreenshot = function(file = NULL) {
      "!DEBUG sd_takeScreenshot"
      self$logEvent("Taking screenshot")
      private$web$takeScreenshot(file)

      # On Windows, need to fix up the PNG resolution header to make it
      # consistent.
      if (is_windows()) {
        normalize_png_res_header(file)
      }

      invisible(self)
    },

    #' @description
    #' Find an HTML element on the page, using a CSS selector, XPath expression,
    #' or link text (for `<a>` tags). If multiple elements are matched, only
    #' the first is returned.
    #' @return A [webdriver::Element].
    findElement = function(css = NULL, linkText = NULL, partialLinkText = NULL, xpath = NULL) {
      "!DEBUG sd_findElement '`css %||% linkText %||% partialLinkText %||% xpath`'"
      private$web$findElement(css, linkText, partialLinkText, xpath)
    },

    #' @description
    #' Find all elements matching CSS selection, xpath, or link text.
    #' @return A list of [webdriver::Element]s.
    findElements = function(css = NULL, linkText = NULL, partialLinkText = NULL, xpath = NULL) {
      "!DEBUG sd_findElements '`css %||% linkText %||% partialLinkText %||% xpath`'"
      private$web$findElements(css, linkText, partialLinkText, xpath)
    },

    #' @description
    #' Waits until a JavaScript `expr`ession evaluates to `true` or the
    #' `timeout` is exceeded.
    #' @param expr A string containing JavaScript code. Will wait until the
    #'   condition returns `true`.
    #' @return `TRUE` if expression evaluates to `true` without error, before
    #'   timeout. Otherwise returns `NA`.
    waitFor = function(expr, checkInterval = 100, timeout = 3000)  {
      "!DEBUG sd_waitFor"
      private$web$waitFor(expr, checkInterval, timeout)
    },

    #' @description
    #' Waits until Shiny is not busy, i.e. the reactive graph has finished
    #' updating. This is useful, for example, if you've resized the window with
    #' `setWindowSize()` and want to make sure all plot redrawing is complete
    #' before take a screenshot.
    #' @return `TRUE` if done before before timeout; `NA` otherwise.
    waitForShiny = function()  {
      # Shiny automatically sets using busy/idle events:
      # https://github.com/rstudio/shiny/blob/e2537d/srcjs/shinyapp.js#L647-L655
      # Details of busy event: https://shiny.rstudio.com/articles/js-events.html
      private$web$waitFor("!$('html').first().hasClass('shiny-busy')")
    },

    #' @description
    #' Waits until the `input` or `output` with name `name` is not one of
    #' `ignore`d values, or the timeout is reached.
    #'
    #' This function can be useful in helping determine if an application
    #' has initialized or finished processing a complex reactive situation.
    #' @param ignore List of possible values to ignore when checking for
    #'   updates.
    waitForValue = function(name, ignore = list(NULL, ""), iotype = c("input", "output", "export"), timeout = 10000, checkInterval = 400) {
      sd_waitForValue(self, private, name = name, ignore = ignore, iotype = match.arg(iotype), timeout = timeout, checkInterval = checkInterval)
    },

    #' @description
    #' Lists the names of all input and output widgets
    #' @return A list of two character vectors, named `input` and `output`.
    listWidgets = function()
      sd_listWidgets(self, private),

    #' @description
    #' Check if Shiny widget names are unique.
    checkUniqueWidgetNames = function()
      sd_checkUniqueWidgetNames(self, private),

    #' @description Execute JS code
    #' @param script JS to execute.
    #' @param ... Additional arguments to script.
    #' @return Self, invisibly.
    executeScript = function(script, ...) {
      "!DEBUG sd_executeScript"
      private$web$executeScript(script, ...)
      invisible(self)
    },

    #' @description Execute JS code asynchronously.
    #' @param script JS to execute.
    #' @param ... Additional arguments to script.
    #' @return Self, invisibly.
    executeScriptAsync = function(script, ...) {
      "!DEBUG sd_executeScriptAsync"
      private$web$executeScriptAsync(script, ...)
      invisible(self)
    },

    ## Main methods

    #' @description
    #' Finds the a Shiny input or output control.
    #' @return A [Widget].
    findWidget = function(name, iotype = c("auto", "input", "output"))
      sd_findWidget(self, private, name, match.arg(iotype)),

    #' @description
    #' It performs one or more update operations via the browser, thens
    #' waits for the specified output(s) to update. The test succeeds if
    #' all specified output widgets are updated before the `timeout`.
    #' For updates that involve a lot of computation, increase the timeout.
    #'
    #' @param output Name of output control to check.
    #' @param ... Name-value pairs used to update inputs.
    expectUpdate = function(output, ..., timeout = 3000,
      iotype = c("auto", "input", "output"))
      sd_expectUpdate(self, private, output, ..., timeout = timeout,
                       iotype = match.arg(iotype)),

    #' @description
    #' Sets input values.
    #' @param ... Name-value pairs, `name1 = value1, name2 = value2` etc.
    #'   Enput with name `name1` will be assigned value `value1`.
    #' @param allowInputNoBinding_ When setting the value of an input, allow
    #'   it to set the value of an input even if that input does not have
    #'   an input binding.
    #' @param priority_ Sets the event priority. For expert use only: see
    #'   <https://shiny.rstudio.com/articles/communicating-with-js.html#values-vs-events> for details.
    #' @param values_ If `TRUE`, will return final updated values of inputs.
    #' @return Returns update values, invisibly.
    setInputs = function(..., wait_ = TRUE, values_ = TRUE, timeout_ = 3000,
      allowInputNoBinding_ = FALSE, priority_ = c("input", "event")) {
      sd_setInputs(self, private, ..., wait_ = wait_, values_ = values_,
                   timeout_ = timeout_, allowInputNoBinding_ = allowInputNoBinding_,
                   priority_ = priority_)
    },

    #' @description
    #' Uploads a file to a file input.
    #' @param ... Name-path pairs, e.g. `name1 = path1`. The file located at
    #' `path1` will be uploaded to file input with name `name1`.
    #' @param values_ If `TRUE`, will return final updated values of download
    #'   control.
    uploadFile = function(..., wait_ = TRUE, values_ = TRUE, timeout_ = 3000)
      sd_uploadFile(self, private, ..., wait_ = wait_, values_ = values_,
                     timeout_ = timeout_),

    #' @description
    #' Download a snapshot. Generally, you should not call this function
    #' yourself; it will be generated by [recordTest()] as needed.
    #' @param path Directory to save snapshots.
    #' @param screenshot Take screenshots for each snapshot?
    snapshotInit = function(path, screenshot = TRUE)
      sd_snapshotInit(self, private, path, screenshot),

    #' @description
    #' Take a snapshot. Generally, you should not call this function
    #' yourself; it will be generated by [recordTest()] as needed.
    #' @param items Elements to include in snapshot
    #' @param filename Filename to use
    #' @param screenshot Take a screenshot? Overrides value set by
    #'   `$snapshotInit()`
    #' @exclude Names of objects to exclude from `input`, `output`, and `export.`
    #' @param stop_on_error When `TRUE` (the default) error messages from the
    #'   shiny app are displayed and execution halts. When `FALSE` the HTTP
    #'   error code and message are included in the snapshot.
    snapshot = function(items = NULL,
                        filename = NULL,
                        screenshot = NULL,
                        exclude = NULL,
                        stop_on_error = TRUE)
      sd_snapshot(self, private, items, filename, screenshot, exclude,
                  stop_on_error),


    #' @description Deprecated
    #' @param ... Ignored
    snapshotCompare = function(...) {
      message("app$snapshotCompare() no longer used")
    },

    #' @description
    #' Snapshot a file download action. Generally, you should not call this
    #' function yourself; it will be generated by [recordTest()] as needed.
    #' @param id Output id of [shiny::downloadButton()]/[shiny::downloadLink()]
    #' @param filename File name to save file to. The default, `NULL`,
    #'   generates an ascending sequence of names: `001.download`,
    #'   `002.download`, etc.
    #' @param stop_on_error When `TRUE` (the default) error messages from the
    #'   shiny app are displayed and execution halts. When `FALSE` the HTTP
    #'   error code and message are stored in the download file.
    snapshotDownload = function(id, filename = NULL, stop_on_error=TRUE)
      sd_snapshotDownload(self, private, id, filename, stop_on_error),

    #' @description Directory where app is located
    getAppDir = function() {
      # path can be a directory (for a normal Shiny app) or path to a .Rmd
      if (self$isRmd()) dirname(private$path) else private$path
    },

    #' @description App file name, i.e. `app.R` or `server.R`. `NULL` for Rmds.
    getAppFilename = function() {
      if (!self$isRmd()) {
        NULL
      } else {
        basename(private$path)
      }
    },

    #' @description Directory where tests are located
    getTestsDir = function() {
      if (self$isRmd()) {
        path <- dirname(private$path)
      } else {
        path <- private$path
      }
      findTestsDir(path, quiet = TRUE)
    },

    #' @description Relative path to app from current directory.
    getRelativePathToApp = function() {
      # Get the relative path from the test directory to the parent. Since there
      # are currently only two supported test dir options, we can just cheat
      td <- self$getTestsDir()
      if (grepl("[/\\\\]shinytest[/\\\\]?", td, perl = TRUE)) {
        "../.."
      } else {
        ".."
      }
    },

    #' @description Directory where snapshots are located.
    getSnapshotDir = function() {
      testDir <- findTestsDir(self$getAppDir(), quiet = TRUE)
      file.path(testDir, private$snapshotDir)
    },

    #' @description Is this app an Shiny Rmd document?
    isRmd = function() {
      is_rmd(private$path)
    }
  ),

  private = list(

    state = "stopped",                  # stopped or running
    path = NULL,                        # Full path to app (including filename if it's a .Rmd)
    shinyUrlProtocol = NULL,            # "http" or "https"
    shinyUrlHost = NULL,                # usually 127.0.0.1
    shinyUrlPort = NULL,
    shinyUrlPath = NULL,
    shinyProcess = NULL,                # process object
    phantomPort = NULL,
    web = NULL,                         # webdriver session
    afterId = NULL,
    shinyTestSnapshotBaseUrl = NULL,   # URL for shiny's test API
    snapshotDir = "snapshot",          # Directory for storing test artifacts
    snapshotCount = 0,
    snapshotScreenshot = TRUE,         # Whether to take screenshots for each snapshot
    shinyWorkerId = NA_character_,
    eventLog = list(),
    cleanLogs = TRUE,                  # Whether to clean logs when GC'd

    startShiny = function(path, seed = NULL, loadTimeout = 10000,
                          shinyOptions = list())
      sd_startShiny(self, private, path, seed, loadTimeout, shinyOptions),

    getShinyUrl = function()
      sd_getShinyUrl(self, private),

    setShinyUrl = function(url)
      sd_setShinyUrl(self, private, url),

    setupDebugging = function(debug)
      sd_setupDebugging(self, private, debug),

    queueInputs = function(...)
      sd_queueInputs(self, private, ...),

    flushInputs = function(wait = TRUE, timeout = 1000)
      sd_flushInputs(self, private, wait, timeout),

    getTestSnapshotUrl = function(input = TRUE, output = TRUE,
      export = TRUE, format = "json")
    {
      sd_getTestSnapshotUrl(self, private, input, output, export,
        format)
    }
  )
)

ShinyDriver$debugLogTypes <- c(
  "shiny_console",
  "browser",
  "shinytest"
)

sd_stop <- function(self, private) {
  "!DEBUG sd_stop"

  if (private$state == "stopped")
    return(invisible(self))

  self$logEvent("Closing PhantomJS session")
  private$web$delete()

  # If the app is being hosted locally, kill the process.
  if (!is.null(private$shinyProcess)) {
    self$logEvent("Ending Shiny process")

    # Attempt soft-kill before hard-kill. This is a workaround for
    # https://github.com/r-lib/processx/issues/95
    # SIGINT quits the Shiny application, SIGTERM tells R to quit.
    # Unfortunately, SIGTERM isn't quite the same as `q()`, because
    # finalizers with onexit=TRUE don't seem to run.
    private$shinyProcess$signal(tools::SIGINT)
    private$shinyProcess$wait(500)
    private$shinyProcess$signal(tools::SIGTERM)
    private$shinyProcess$wait(250)
    private$shinyProcess$kill()
  }

  private$state <- "stopped"
  invisible(self)
}

sd_waitForValue <- function(self, private, name, ignore = list(NULL, ""), iotype = "input", timeout = 10000, checkInterval = 400) {
  "!DEBUG sd_waitForValue"

  timeoutSec <- as.numeric(timeout) / 1000
  if (!is.numeric(timeoutSec) || is.na(timeoutSec) || is.nan(timeoutSec)) {
    stop("timeout must be numeric")
  }
  checkInterval <- as.numeric(checkInterval)
  if (!is.numeric(checkInterval) || is.na(checkInterval) || is.nan(checkInterval)) {
    stop("checkInterval must be numeric")
  }

  now <- function() {
    as.numeric(Sys.time())
  }

  endTime <- now() + timeoutSec

  while (TRUE) {
    value <- try({
      # by default, do not retrieve anything
      args <- list(input = FALSE, output = FALSE, export = FALSE)
      # only retrieve `name` from `iotype`
      args[[iotype]] <- name
      do.call(self$getAllValues, args)[[iotype]][[name]]
    }, silent = TRUE)

    # if no error when trying ot retrieve the value..
    if (!inherits(value, "try-error")) {
      # check against all invalid values
      isInvalid <- vapply(ignore, identical, logical(1), x = value)
      # if no matches, then it's a success!
      if (!any(isInvalid)) {
        return(value)
      }
    }

    # if too much time has elapsed... throw
    if (now() > endTime) {
      stop("timeout reached when waiting for value: ", name)
    }

    # wait a little bit for shiny to do some work
    Sys.sleep(checkInterval / 1000)
  }
}

sd_listWidgets <- function(self, private) {
  "!DEBUG sd_listWidgets"
  res <- private$web$executeScript("return shinytest.listWidgets();")
  res$input <- unlist(res$input)
  res$output <- unlist(res$output)
  res
}

sd_checkUniqueWidgetNames <- function(self, private) {
  "!DEBUG sd_checkUniqueWidgetNames"
  widgets <- self$listWidgets()
  inputs <- widgets$input
  outputs <- widgets$output

  check <- function(what, ids) {
    if (any(duplicated(ids))) {
      dup <- paste(unique(ids[duplicated(ids)]), collapse = ", ")
      warning("Possible duplicate ", what, " widget ids: ", dup)
    }
  }

  if (any(inputs %in% outputs)) {
    dups <- unique(inputs[inputs %in% outputs])
    warning(
      "Widget ids both for input and output: ",
      paste(dups, collapse = ", ")
    )

    ## Otherwise the following checks report it, too
    inputs <- setdiff(inputs, dups)
    outputs <- setdiff(outputs, dups)
  }

  if (length(inputs) > 0) check("input", inputs)
  if (length(outputs) > 0) check("output", outputs)
}
