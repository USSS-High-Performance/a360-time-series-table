## insert_diag.R ################################################################
##
## One-off diagnostic for the 422 on eventimport. smartabaseR hides the real
## error behind an empty "! UNEXPECTED_ERROR", so we POST to the v1 eventimport
## endpoint DIRECTLY and print the raw HTTP status + body.
##
## It runs a SEQUENCE of escalating attempts against the target form -- minimal
## first, then adding one thing at a time -- so we can see exactly which piece
## the server rejects (userId type, a specific field, the Timestamp value
## format, or the multi-row table layout). Each successful attempt writes a
## small throwaway event to the DEV form (fine -- it is a dev form).
##
## Run:  Rscript insert_diag.R
## Reads SB_URL/SB_USERNAME/SB_PASSWORD + SB_START_DATE/SB_END_DATE/SB_LOOKBACK_DAYS.
################################################################################

library(smartabaseR)
library(httr2)
library(jsonlite)
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

## --- Fetch one real source event --------------------------------------------
message("Fetching one source event from '", source_form, "' ", start_date, " .. ", end_date)
events <- sb_get_event(
  form = source_form, date_range = c(start_date, end_date),
  url = sb_url, username = sb_username, password = sb_password
)
if (is.null(events) || nrow(events) == 0) stop("No source events in the window.")
ev <- events[events$event_id == events$event_id[[1]], ][1, , drop = FALSE]

raw   <- as.character(ev[[source_field]][[1]])
parts <- unlist(strsplit(raw, "\\s*<\\s*br\\s*/?\\s*>\\s*|\\r?\\n", perl = TRUE))
parts <- trimws(parts); parts <- parts[nzchar(parts)]
parts <- utils::head(parts, 3)
split3      <- strsplit(parts, ",")
timestamps  <- vapply(split3, function(x) trimws(x[[1]]), character(1))
heart_rates <- vapply(split3, function(x) trimws(x[[2]]), character(1))

user_id       <- as.character(ev$user_id[[1]])
id_api        <- as.character(ev[["ID - API"]][[1]])
sport_info    <- as.character(ev[["Detailed Sport Info - API"]][[1]])
ev_start_date <- as.character(ev$start_date[[1]])
ev_start_time <- as.character(ev$start_time[[1]])

message("  user_id=", user_id, "  ID - API=", id_api,
        "  start_date=", ev_start_date, "  start_time=", ev_start_time)
message("  raw timestamps: ", paste(timestamps, collapse = ", "),
        " | heart rates: ", paste(heart_rates, collapse = ", "))

## --- POST helper: print exact JSON, status, and body ------------------------
endpoint <- paste0("https://", sb_url, "/api/v1/eventimport")

pair <- function(k, v) list(key = k, value = v)

# uid_val: pass integer or string to test binding sensitivity.
build_payload <- function(rows, uid_val) {
  list(
    formName    = target_form,
    startDate   = ev_start_date,
    finishDate  = ev_start_date,
    startTime   = ev_start_time,
    userId      = list(userId = uid_val),
    existingEventId = "",
    rows        = rows
  )
}

send <- function(label, payload) {
  js <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  cat("\n########################################################\n")
  cat("#### ", label, "\n")
  cat("JSON: ", js, "\n", sep = "")
  resp <- request(endpoint) |>
    req_url_query(informat = "json", format = "json") |>
    req_auth_basic(sb_username, sb_password) |>
    req_headers("X-APP-ID" = "usss.integration.v1") |>
    req_body_raw(js, type = "application/json") |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  cat("STATUS: ", resp_status(resp), " ", resp_status_desc(resp), "\n", sep = "")
  body <- resp_body_string(resp)
  cat("BODY:   ", substr(body, 1, 700), "\n", sep = "")
  invisible(resp_status(resp))
}

## --- Escalating attempts ----------------------------------------------------
# 1: absolute minimum -- one non-table text field, userId as INTEGER.
send("A1 minimal, userId=int, [ID - API only]",
     build_payload(list(list(row = 0L, pairs = list(pair("ID - API", id_api)))),
                   as.integer(user_id)))

# 2: same but userId as STRING (tests the "everything must be a string" idea).
send("A2 minimal, userId=string, [ID - API only]",
     build_payload(list(list(row = 0L, pairs = list(pair("ID - API", id_api)))),
                   user_id))

# 3: add the other non-table field.
send("A3 both non-table fields [ID - API, Detailed Sport Info - API]",
     build_payload(list(list(row = 0L, pairs = list(
       pair("ID - API", id_api), pair("Detailed Sport Info - API", sport_info)))),
       as.integer(user_id)))

# 4: single row with the TABLE fields too (one Timestamp/Heart Rate sample).
send("A4 single row + table fields [+Timestamp, Heart Rate]",
     build_payload(list(list(row = 0L, pairs = list(
       pair("ID - API", id_api), pair("Detailed Sport Info - API", sport_info),
       pair("Timestamp", timestamps[[1]]), pair("Heart Rate", heart_rates[[1]])))),
       as.integer(user_id)))

# 5: Timestamp reformatted to h:mm AM/PM (in case it is a Time-type field).
to_ampm <- function(hms) {
  p <- as.integer(strsplit(hms, ":")[[1]]); h <- p[[1]]; m <- p[[2]]
  ap <- if (h < 12) "AM" else "PM"; h12 <- ((h + 11) %% 12) + 1
  sprintf("%d:%02d %s", h12, m, ap)
}
send("A5 single row, Timestamp as 'h:mm AM/PM'",
     build_payload(list(list(row = 0L, pairs = list(
       pair("ID - API", id_api), pair("Detailed Sport Info - API", sport_info),
       pair("Timestamp", to_ampm(timestamps[[1]])), pair("Heart Rate", heart_rates[[1]])))),
       as.integer(user_id)))

# 6: multi-row table (row 0 carries non-table + first sample; rows 1..2 table only).
rows_multi <- lapply(seq_along(timestamps), function(i) {
  p <- list(pair("Timestamp", timestamps[[i]]), pair("Heart Rate", heart_rates[[i]]))
  if (i == 1) p <- c(list(pair("ID - API", id_api),
                          pair("Detailed Sport Info - API", sport_info)), p)
  list(row = i - 1L, pairs = p)
})
send("A6 multi-row table (3 samples)", build_payload(rows_multi, as.integer(user_id)))

cat("\n==== Diagnosis: the FIRST attempt that returns 200/success is the good ",
    "shape; the first that flips to 422 names the culprit. ====\n", sep = "")
