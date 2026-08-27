## insert_diag2.R ###############################################################
##
## Every raw eventimport attempt (insert_diag.R) returned a GENERIC Apache 422 --
## even the minimal one -- so the request is bounced before Smartabase's import
## logic runs. Reads work through smartabaseR though, so here we let smartabaseR
## itself perform a tiny 1-row insert with FULL HTTP verbosity, to capture the
## exact URL, method, headers and body it sends for a write, and the exact
## response it gets. That shows whether writes use a different endpoint than our
## raw call, or are being rejected at the instance level (permission / disabled).
##
## httr2 redacts the Authorization header as <REDACTED> in verbose output, so it
## is safe to share -- but still glance over the dump before pasting.
##
## Run:  Rscript insert_diag2.R
################################################################################

library(smartabaseR)
library(dotenv)
load_dot_env()

sb_url      <- Sys.getenv("SB_URL", "usopc.smartabase.com/athlete360-usss")
sb_username <- Sys.getenv("SB_USERNAME")
sb_password <- Sys.getenv("SB_PASSWORD")
if (!nzchar(sb_username) || !nzchar(sb_password)) {
  stop("Set SB_USERNAME and SB_PASSWORD before running.")
}

source_form  <- "Polar Summary - Training"
target_form  <- "DEV Polar Summary - Training - Time Series"
source_field <- "Split Heart Rates"

lookback_days <- suppressWarnings(as.integer(Sys.getenv("SB_LOOKBACK_DAYS", "1")))
if (is.na(lookback_days)) lookback_days <- 1L
end_date   <- Sys.getenv("SB_END_DATE", format(Sys.Date(), "%d/%m/%Y"))
start_env  <- Sys.getenv("SB_START_DATE", "")
start_date <- if (nzchar(start_env)) start_env else format(Sys.Date() - lookback_days, "%d/%m/%Y")

## Fetch one real source event for valid user_id / ID / a sample.
message("Fetching one source event ...")
events <- sb_get_event(
  form = source_form, date_range = c(start_date, end_date),
  url = sb_url, username = sb_username, password = sb_password
)
if (is.null(events) || nrow(events) == 0) stop("No source events in the window.")
ev <- events[events$event_id == events$event_id[[1]], ][1, , drop = FALSE]

raw   <- as.character(ev[[source_field]][[1]])
parts <- unlist(strsplit(raw, "\\s*<\\s*br\\s*/?\\s*>\\s*|\\r?\\n", perl = TRUE))
parts <- trimws(parts); parts <- parts[nzchar(parts)]
first <- strsplit(trimws(parts[nzchar(parts)][[1]]), ",")[[1]]

df <- tibble::tibble(
  user_id                     = as.integer(ev$user_id[[1]]),
  start_date                  = as.character(ev$start_date[[1]]),
  start_time                  = as.character(ev$start_time[[1]]),
  `ID - API`                  = as.character(ev[["ID - API"]][[1]]),
  `Detailed Sport Info - API` = as.character(ev[["Detailed Sport Info - API"]][[1]]),
  `Timestamp`                 = trimws(first[[1]]),
  `Heart Rate`                = trimws(first[[2]])
)
message("Insert frame:")
print(df)

## Perform the insert with full HTTP verbosity. with_verbosity works if
## smartabaseR uses httr2; we also flip httr's verbose config on as a fallback
## in case it uses httr.
do_insert <- function() {
  sb_insert_event(
    df = df,
    form = target_form,
    url = sb_url,
    username = sb_username,
    password = sb_password,
    option = sb_insert_event_option(
      table_field = c("Timestamp", "Heart Rate"),
      interactive_mode = FALSE
    )
  )
}

cat("\n==================== HTTP TRACE (smartabaseR write) ====================\n")
res <- tryCatch({
  if (requireNamespace("httr2", quietly = TRUE) &&
      "with_verbosity" %in% getNamespaceExports("httr2")) {
    httr2::with_verbosity(do_insert(), verbosity = 2)
  } else if (requireNamespace("httr", quietly = TRUE)) {
    httr::with_config(httr::verbose(), do_insert())
  } else {
    do_insert()
  }
}, error = function(e) {
  cat("THREW: ", conditionMessage(e), "\n"); NULL
})
cat("\n==================== END TRACE ====================\n")
cat("Returned object:\n")
print(utils::head(res, 5))
