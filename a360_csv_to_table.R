## a360_csv_to_table.R ##########################################################
##
## Convert a CSV-string field on Teamworks AMS (Smartabase) events into repeating
## table rows, and push the result back onto the SAME events using smartabaseR.
##
## R port of a360_csv_to_table.py. Same idea:
##   1. Pull events for a form over a date range      -> sb_get_event()
##   2. For each event whose target table fields are   -> parse the CSV string
##      still empty, split the source CSV field into      in `source_field`
##      Timestamp / Heart Rate rows
##   3. Append those rows to the event and overwrite   -> sb_update_event()
##      the event in place (existing data preserved)
##
## Why sb_update_event(): "updating" in Smartabase overwrites the whole event, so
## we build each event from its FULL sb_get_event() output (every existing field
## + event_id) and simply append the new table rows. Nothing else is lost.
##
## Credentials are read from environment variables -- never hardcode them here:
##   SB_URL       e.g. usopc.smartabase.com/athlete360-usss   (has a default)
##   SB_USERNAME  AMS username                                 (required)
##   SB_PASSWORD  AMS password                                 (required)
##
## Date range (configurable; dd/mm/YYYY). Defaults to the last SB_LOOKBACK_DAYS.
## To reach older records (e.g. 2020-dated events) widen it, for example:
##   SB_START_DATE=01/01/2015   or   SB_LOOKBACK_DAYS=4000
##
## SB_DRY_RUN=true (default) pulls + reports + writes a preview CSV, but does NOT
## call sb_update_event(). Set SB_DRY_RUN=false to actually push.
################################################################################

library(dplyr)
library(smartabaseR)

## ---- Configuration ---------------------------------------------------------
sb_url      <- Sys.getenv("SB_URL", "usopc.smartabase.com/athlete360-usss")
sb_username <- Sys.getenv("SB_USERNAME")
sb_password <- Sys.getenv("SB_PASSWORD")

if (!nzchar(sb_username) || !nzchar(sb_password)) {
  stop("Set SB_USERNAME and SB_PASSWORD environment variables before running.")
}

dry_run <- tolower(Sys.getenv("SB_DRY_RUN", "true")) %in% c("true", "1", "yes")
interactive_mode <- tolower(Sys.getenv("SB_INTERACTIVE", "true")) %in% c("true", "1", "yes")

## Form / source field / column mapping (mirrors the Python timeseries_dicts).
timeseries_specs <- list(
  list(
    form_name      = "Polar Summary - Training",  # Teamworks AMS form
    source_field   = "Split Heart Rates",         # CSV-string field to parse
    source_columns = c("Timestamp", "Heart Rate"),# header in that CSV string
    target_fields  = c("Timestamp", "Heart Rate") # table fields to populate
  )
)

## Date range (dd/mm/YYYY). Explicit SB_START_DATE/SB_END_DATE win; otherwise a
## rolling window of SB_LOOKBACK_DAYS ending today.
lookback_days <- suppressWarnings(as.integer(Sys.getenv("SB_LOOKBACK_DAYS", "7")))
if (is.na(lookback_days)) lookback_days <- 7L

end_date <- Sys.getenv("SB_END_DATE", format(Sys.Date(), "%d/%m/%Y"))
start_env <- Sys.getenv("SB_START_DATE", "")
start_date <- if (nzchar(start_env)) {
  start_env
} else {
  format(Sys.Date() - lookback_days, "%d/%m/%Y")
}

## ---- Helpers ---------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

log_status <- function(...) {
  message(...)
  flush.console()
}

## Split the CSV string held in a source field into a tibble whose columns are
## named after target_fields (index-aligned to source_columns).
parse_samples <- function(text, source_columns, target_fields) {
  text <- trimws(as.character(text %||% ""))
  if (identical(text, "") || is.na(text)) {
    return(NULL)
  }

  con <- textConnection(text)
  on.exit(close(con), add = TRUE)
  df <- tryCatch(
    utils::read.csv(
      con,
      header = TRUE,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      colClasses = "character"
    ),
    error = function(e) NULL
  )

  header_ok <- !is.null(df) && all(source_columns %in% names(df))

  if (!header_ok) {
    # Fall back to positional parsing, skipping a header line if present.
    lines <- strsplit(text, "\r?\n")[[1]]
    lines <- lines[nzchar(trimws(lines))]
    cells <- strsplit(lines, ",")
    header_row <- vapply(
      cells[1],
      function(r) identical(trimws(r)[seq_along(source_columns)], source_columns),
      logical(1)
    )
    if (isTRUE(header_row)) {
      cells <- cells[-1]
    }
    df <- as.data.frame(
      do.call(rbind, lapply(cells, function(r) {
        r <- trimws(r)
        length(r) <- length(source_columns)  # pad short rows with NA
        r
      })),
      stringsAsFactors = FALSE
    )
    names(df) <- source_columns
  }

  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  # Select the source columns, rename to the target field names.
  out <- df[, source_columns, drop = FALSE]
  names(out) <- target_fields
  tibble::as_tibble(out)
}

## TRUE when none of the target fields hold a value for this event's rows.
target_fields_empty <- function(event_rows, target_fields) {
  for (tf in target_fields) {
    if (tf %in% names(event_rows)) {
      vals <- trimws(as.character(event_rows[[tf]]))
      if (any(!is.na(vals) & vals != "")) {
        return(FALSE)
      }
    }
  }
  TRUE
}

## Build the overwrite frame for one event: its existing rows + appended table
## rows (metadata carried on every row, other fields blank on appended rows).
build_event_update <- function(event_rows, samples, target_fields) {
  n_new <- nrow(samples)

  meta_cols <- intersect(
    c("form", "start_date", "end_date", "start_time", "end_time",
      "user_id", "about", "username", "event_id"),
    names(event_rows)
  )

  # Make sure target columns exist on the existing rows (empty).
  for (tf in target_fields) {
    if (!tf %in% names(event_rows)) {
      event_rows[[tf]] <- NA
    }
  }

  # Appended rows: copy metadata from the first existing row, blank everything
  # else, then drop in the parsed Timestamp / Heart Rate values.
  new_rows <- event_rows[rep(1, n_new), , drop = FALSE]
  blank_cols <- setdiff(names(new_rows), c(meta_cols, target_fields))
  if (length(blank_cols) > 0) {
    new_rows[blank_cols] <- NA
  }
  for (tf in target_fields) {
    new_rows[[tf]] <- samples[[tf]]
  }

  dplyr::bind_rows(event_rows, new_rows)
}

## ---- Main ------------------------------------------------------------------
log_status("Mode: ", if (dry_run) "DRY RUN (no writes)" else "LIVE (will push back)")
log_status("Date range: ", start_date, " .. ", end_date)

for (spec in timeseries_specs) {
  form_name      <- spec$form_name
  source_field   <- spec$source_field
  source_columns <- spec$source_columns
  target_fields  <- spec$target_fields

  log_status("\nForm '", form_name, "': fetching events")
  events <- sb_get_event(
    form = form_name,
    date_range = c(start_date, end_date),
    url = sb_url,
    username = sb_username,
    password = sb_password
  )

  if (is.null(events) || nrow(events) == 0) {
    log_status("  no events returned for this window")
    next
  }

  if (!"event_id" %in% names(events)) {
    stop("sb_get_event() did not return an event_id column for ", form_name,
         "; cannot update in place.")
  }
  if (!source_field %in% names(events)) {
    log_status("  source field '", source_field, "' not present on this form; skipping")
    next
  }

  event_ids <- unique(events$event_id)
  log_status("  ", nrow(events), " row(s) across ", length(event_ids), " event(s)")

  updates <- list()
  for (eid in event_ids) {
    event_rows <- events %>% filter(.data$event_id == eid)

    # Skip events whose table is already populated.
    if (!target_fields_empty(event_rows, target_fields)) {
      next
    }

    csv_text <- event_rows[[source_field]][[1]]
    samples <- parse_samples(csv_text, source_columns, target_fields)
    if (is.null(samples) || nrow(samples) == 0) {
      next
    }

    updates[[as.character(eid)]] <-
      build_event_update(event_rows, samples, target_fields)
    log_status("  event ", eid, " (", event_rows$start_date[[1]], "): appending ",
               nrow(samples), " row(s)")
  }

  if (length(updates) == 0) {
    log_status("  nothing to update (targets already filled or no source data)")
    next
  }

  update_df <- dplyr::bind_rows(updates)

  # Preview to disk so the exact frame can be inspected before pushing.
  preview_path <- paste0(
    "update_preview_", gsub("[^A-Za-z0-9]+", "_", form_name), ".csv"
  )
  utils::write.csv(update_df, preview_path, row.names = FALSE, na = "")
  log_status("  wrote preview: ", preview_path,
             " (", nrow(update_df), " rows, ", length(updates), " events)")

  if (dry_run) {
    log_status("  DRY RUN -- not calling sb_update_event(). ",
               "Set SB_DRY_RUN=false to push.")
    next
  }

  # NOTE: table_field lists ONLY the fields we populate (Timestamp, Heart Rate).
  # The Polar Summary events observed are single-row (no other table fields), so
  # this is safe. If this form ever gains OTHER table fields, add their names
  # here too -- otherwise sb_update_event() would treat them as non-table and
  # keep only their first-row value.
  sb_update_event(
    df = update_df,
    form = form_name,
    url = sb_url,
    username = sb_username,
    password = sb_password,
    option = sb_update_event_option(
      table_field = target_fields,
      interactive_mode = interactive_mode
    )
  )
}

log_status("\nDone.")
