#!/usr/bin/env Rscript
# Drive a real sign-in against a SigRepo Shiny instance and assert the
# signed-in state.
#
#   Rscript scripts/smoke_test_ui.R <url> <username> <password> [screenshot-path]
#
# HTTP 200 from the Shiny port only proves Shiny Server is running. It says
# nothing about whether the app works, whether the database is reachable from
# it, or whether anyone can get in.
#
# Run this from a machine that has a browser. montilab.bu.edu does not have one
# installed, which is why this is separate from scripts/smoke_test.sh; against
# staging it runs from a laptop through the SSH tunnel:
#
#   Rscript scripts/smoke_test_ui.R http://127.0.0.1:9051 devadmin devadmin
#
# Exits 0 only if the app reached a signed-in state.

`%||%` <- function(a, b) if (is.null(a)) b else a

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("usage: smoke_test_ui.R <url> <username> <password> [screenshot-path]")
}
url  <- args[1]
user <- args[2]
pw   <- args[3]
shot <- if (length(args) > 3) args[4] else tempfile(fileext = ".png")

suppressPackageStartupMessages(library(chromote))

b <- ChromoteSession$new()
on.exit(try(b$close(), silent = TRUE), add = TRUE)

js <- function(expr) b$Runtime$evaluate(expr, awaitPromise = FALSE)$result$value

report_fail <- function(msg) {
  try(b$screenshot(filename = shot), silent = TRUE)
  cat("FAIL  ", msg, "\n", sep = "")
  cat("screenshot: ", shot, "\n", sep = "")
  quit(status = 1)
}

# A zero-width viewport suspends Shiny outputs: nothing renders, and the page
# looks broken for reasons that have nothing to do with the app. Size the window
# before asserting anything about it.
b$Browser$setWindowBounds(
  windowId = b$Browser$getWindowForTarget()$windowId,
  bounds = list(width = 1280, height = 900)
)
b$Emulation$setDeviceMetricsOverride(
  width = 1280, height = 900, deviceScaleFactor = 1, mobile = FALSE
)

b$Page$navigate(url)
Sys.sleep(12)

if (!isTRUE(js("typeof Shiny !== 'undefined' && !!Shiny.shinyapp"))) {
  report_fail(paste0("Shiny never connected at ", url))
}
if (!isTRUE(js("!!document.querySelector('#sign_in_btn')"))) {
  report_fail("no sign-in button on the page")
}

# Set the inputs directly rather than typing into them. #sign_in_btn carries an
# inline onclick="login_keypress(e)" helper for the Enter key, which throws on a
# plain click because there is no key event for it to read.
js(sprintf(
  "Shiny.setInputValue('username', '%s', {priority:'event'});
   Shiny.setInputValue('password', '%s', {priority:'event'}); 'set'",
  user, pw
))
Sys.sleep(1)
js("document.querySelector('#sign_in_btn').click(); 'clicked'")
Sys.sleep(15)

# Assert the signed-in state, not the absence of the login form. The login
# elements stay in the DOM after a successful sign-in, merely hidden, so
# querying for them proves nothing either way.
logout_links <- js("(document.body.innerText.match(/Log ?out/i) || []).length")
tabs <- js(
  "Array.from(document.querySelectorAll('.nav-tabs a, .navbar-nav a'))
     .map(function(a){return a.textContent.trim()}).filter(Boolean).join('|')"
) %||% ""

if (is.null(logout_links) || logout_links < 1) {
  report_fail(paste0("no signed-in state after sign-in; tabs seen: ", tabs))
}
if (!grepl("Signatures", tabs, fixed = TRUE)) {
  report_fail(paste0("Signatures tab missing after sign-in; tabs seen: ", tabs))
}

b$screenshot(filename = shot)
cat("PASS  signed in as ", user, "\n", sep = "")
cat("tabs: ", tabs, "\n", sep = "")
cat("screenshot: ", shot, "\n", sep = "")
