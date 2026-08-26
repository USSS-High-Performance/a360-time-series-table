## insert_diag.R ################################################################
##
## One-off diagnostic: push ONE tiny (3-row) event to the target form by calling
## the Teamworks AMS v1 eventimport endpoint DIRECTLY, and print the raw HTTP
## status + response body. smartabaseR swallows the server's real error message
## behind an empty "! UNEXPECTED_ERROR"; this bypasses it so we can see exactly
## why the insert is rejected (permission, unknown field, form config, etc.).
##
## It reuses smartabaseR only to fetch one real source event (valid user_id + ID
## + a few heart-rate samples), then builds and sends the raw request itself.
##
## Run:  Rscript insert_diag.R
## Reads the same env vars as the main script (SB_URL/SB_USERNAME/SB_PASSWORD,
## SB_START_DATE/SB_END_DATE/SB_LOOKBACK_DAYS). Sends only 3 rows -- safe.
################################################################################

library(smartabaseR)
library(httr2)
library(dotenv)
load_dot_env()

sb_url      <- Sys.getenv("SB_URL", "usopc.smartabase.com/athlete360-usss")
sb_username <- Sys.getenv("SB_USERNAME")
sb_password <- Sys.getenv("SB_PASSWORD")
if (!nzchar(sb_username) || !nzchar(sb_password)) {
  stop("Set SB_USERNAME and SB_PASSWORD before running.")
}

source_form <- "Polar Summary - Training"
target_form <- "DEV Polar Summary - Training - Time Series"
source_field <- "Split Heart Rates"

lookback_days <- suppressWarnings(as.integer(Sys.getenv("SB_LOOKBACK_DAYS", "1")))
if (is.na(lookback_days)) lookback_days <- 1L
end_date   <- Sys.getenv("SB_END_DATE", format(Sys.Date(), "%d/%m/%Y"))
start_env  <- Sys.getenv("SB_START_DATE", "")
start_date <- if (nzchar(start_env)) start_env else format(Sys.Date() - lookback_days, "%d/%m/%Y")

## --- 1. Fetch one real source event -----------------------------------------
message("Fetching one source event from '", source_form, "' ", start_date, " .. ", end_date)
events <- sb_get_event(
  form = source_form, date_range = c(start_date, end_date),
  url = sb_url, username = sb_username, password = sb_password
)
if (is.null(events) || nrow(events) == 0) stop("No source events in the window.")

ev <- events[events$event_id == events$event_id[[1]], ][1, , drop = FALSE]

## Parse 3 samples out of the source field (same <br/> split as the main script).
raw <- as.character(ev[[source_field]][[1]])
parts <- unlist(strsplit(raw, "\\s*<\\s*br\\s*/?\\s*>\\s*|\\r?\\n", perl = TRUE))
parts <- trimws(parts); parts <- parts[nzchar(parts)]
parts <- utils::head(parts, 3)
split3 <- strsplit(parts, ",")
timestamps <- vapply(split3, function(x) trimws(x[[1]]), character(1))
heart_rates <- vapply(split3, function(x) trimws(x[[2]]), character(1))

user_id       <- as.character(ev$user_id[[1]])
id_api        <- as.character(ev[["ID - API"]][[1]])
sport_info    <- as.character(ev[["Detailed Sport Info - API"]][[1]])
ev_start_date <- as.character(ev$start_date[[1]])
ev_start_time <- as.character(ev$start_time[[1]])

message("  user_id=", user_id, "  ID - API=", id_api,
        "  start_date=", ev_start_date, "  start_time=", ev_start_time)
message("  sending timestamps: ", paste(timestamps, collapse = ", "))

## --- 2. Build the raw eventimport payload -----------------------------------
## Row 0 carries the non-table fields + first sample; rows 1..2 carry only the
## table fields (Timestamp / Heart Rate) -- the documented multi-row layout.
mk_pairs <- function(i) {
  base <- list(
    list(key = "Timestamp",  value = timestamps[[i]]),
    list(key = "Heart Rate", value = heart_rates[[i]])
  )
  if (i == 1) {
    base <- c(
      list(list(key = "ID - API", value = id_api),
           list(key = "Detailed Sport Info - API", value = sport_info)),
      base
    )
  }
  base
}
rows <- lapply(seq_along(timestamps), function(i) list(row = i - 1L, pairs = mk_pairs(i)))

payload <- list(
  formName    = target_form,
  startDate   = ev_start_date,
  finishDate  = ev_start_date,
  startTime   = ev_start_time,
  userId      = list(userId = as.integer(user_id)),
  existingEventId = "",
  rows        = rows
)

## --- 3. POST directly and print the raw response ----------------------------
endpoint <- paste0("https://", sb_url, "/api/v1/eventimport")
message("\nPOST ", endpoint)

resp <- request(endpoint) |>
  req_url_query(informat = "json", format = "json") |>
  req_auth_basic(sb_username, sb_password) |>
  req_headers("X-APP-ID" = "usss.integration.v1") |>
  req_body_json(payload, auto_unbox = TRUE) |>
  req_error(is_error = function(resp) FALSE) |>   # never throw -- we want the body
  req_perform()

cat("\n==== HTTP STATUS:", resp_status(resp), resp_status_desc(resp), "====\n")
cat("==== RESPONSE BODY ====\n")
cat(resp_body_string(resp), "\n")
cat("==== END ====\n")
