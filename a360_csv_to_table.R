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
## + event_id) and expand it into one table row per sample. Nothing else is lost.
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
library(dotenv)
load_dot_env()
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
##
## readonly_fields: fields we must NOT write back. "Split Heart Rates" is a
## LINKED field (its value comes from another form), so it is read-only on this
## form -- we read it to parse, but we drop it from the update payload so we
## don't attempt an illegal write. The link re-resolves on Smartabase's side, so
## the value is preserved untouched.
timeseries_specs <- list(
  list(
    form_name      = "Polar Summary - Training",  # Teamworks AMS form
    source_field   = "Split Heart Rates",         # CSV-string field to parse
    source_columns = c("Timestamp", "Heart Rate"),# header in that CSV string
    target_fields  = c("Timestamp", "Heart Rate"),# table fields to populate
    readonly_fields = c("Split Heart Rates")      # linked field: read, never write
  )
)

## Date range (dd/mm/YYYY). Explicit SB_START_DATE/SB_END_DATE win; otherwise a
## rolling window of SB_LOOKBACK_DAYS ending today.
lookback_days <- suppressWarnings(as.integer(Sys.getenv("SB_LOOKBACK_DAYS", "1")))
if (is.na(lookback_days)) lookback_days <- 1L

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

## Split the time-series string held in a source field into a tibble whose
## columns are named after target_fields (index-aligned to source_columns).
##
## AMS stores this field as one long string. Samples are NOT newline-separated:
## they are delimited by HTML break tags -- literally "< br/>" in the data (also
## handle <br>, <br/>, <br /> and real newlines just in case). Each sample is
## comma-separated, e.g. "07:41:18,105". There is usually no header row.
parse_samples <- function(text, source_columns, target_fields) {
  text <- as.character(text %||% "")
  if (!nzchar(trimws(text))) {
    return(NULL)
  }

  # Split rows on any <br> variant (with optional surrounding whitespace) or a
  # newline; then drop empties (the string often starts with a leading break).
  parts <- unlist(strsplit(text, "\\s*<\\s*br\\s*/?\\s*>\\s*|\\r?\\n", perl = TRUE))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) {
    return(NULL)
  }

  # Drop a leading header element if one is present (e.g. "Timestamp,Heart Rate").
  first_cells <- trimws(strsplit(parts[[1]], ",")[[1]])
  if (length(first_cells) >= length(source_columns) &&
      identical(first_cells[seq_along(source_columns)], source_columns)) {
    parts <- parts[-1]
  }
  if (length(parts) == 0) {
    return(NULL)
  }

  ncol <- length(target_fields)
  mat <- do.call(rbind, lapply(strsplit(parts, ","), function(cells) {
    cells <- trimws(cells)
    length(cells) <- ncol            # pad short rows / trim long ones to ncol
    cells
  }))

  out <- as.data.frame(mat, stringsAsFactors = FALSE)
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

## Build the overwrite frame for one event by EXPANDING it into N table rows
## (N = number of parsed samples). This matches the Smartabase table layout:
##   * metadata (start_date, user_id, about, event_id, ...) on EVERY row
##   * non-table fields only on the FIRST row (NA elsewhere)
##   * the table fields (Timestamp, Heart Rate) populated across all N rows
## The event's non-table data is preserved on row 1; the empty table is filled.
build_event_update <- function(event_rows, samples, target_fields) {
  n <- nrow(samples)

  meta_cols <- intersect(
    c("form", "start_date", "end_date", "start_time", "end_time",
      "user_id", "about", "username", "event_id"),
    names(event_rows)
  )

  # Base = the event's first (non-table) row, replicated N times.
  base <- event_rows[rep(1, n), , drop = FALSE]

  # Non-table fields repeat only on row 1; blank them on rows 2..N.
  blank_cols <- setdiff(names(base), c(meta_cols, target_fields))
  if (n > 1 && length(blank_cols) > 0) {
    base[2:n, blank_cols] <- NA
  }

  # Fill the table fields across every row (creates the columns if absent).
  for (i in seq_along(target_fields)) {
    base[[target_fields[[i]]]] <- samples[[target_fields[[i]]]]
  }

  base
}

## ---- Main ------------------------------------------------------------------
log_status("Mode: ", if (dry_run) "DRY RUN (no writes)" else "LIVE (will push back)")
log_status("Date range: ", start_date, " .. ", end_date)

for (spec in timeseries_specs) {
  form_name      <- spec$form_name
  source_field   <- spec$source_field
  source_columns <- spec$source_columns
  target_fields  <- spec$target_fields
  readonly_fields <- if (is.null(spec$readonly_fields)) character(0) else spec$readonly_fields

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
    log_status("  event ", eid, " (", event_rows$start_date[[1]], "): expanding to ",
               nrow(samples), " table row(s)")
  }

  if (length(updates) == 0) {
    log_status("  nothing to update (targets already filled or no source data)")
    next
  }

  update_df <- dplyr::bind_rows(updates)

  # Drop read-only / linked fields (e.g. "Split Heart Rates") from the payload:
  # we can't write them, and leaving them out keeps their link intact on the
  # event rather than attempting an illegal overwrite.
  drop_cols <- intersect(readonly_fields, names(update_df))
  if (length(drop_cols) > 0) {
    update_df <- update_df[, setdiff(names(update_df), drop_cols), drop = FALSE]
    log_status("  dropped read-only/linked field(s) from push: ",
               paste(drop_cols, collapse = ", "))
  }

  # Preview to disk so the frame can be inspected before pushing. IMPORTANT:
  # this is only for human inspection -- the value pushed to sb_update_event()
  # is the full update_df. We truncate oversized text cells (e.g. the preserved
  # raw "Split Heart Rates" blob, ~128 KB on row 1) because Excel caps a single
  # cell at 32,767 characters and otherwise spills it across columns, making the
  # preview look mangled even though the underlying data is correct.
  preview_path <- paste0(
    "update_preview_", gsub("[^A-Za-z0-9]+", "_", form_name), ".csv"
  )
  preview_max_chars <- 80L
  preview_df <- update_df
  preview_df[] <- lapply(preview_df, function(col) {
    if (is.character(col)) {
      long <- !is.na(col) & nchar(col) > preview_max_chars
      col[long] <- paste0(substr(col[long], 1, preview_max_chars),
                          "...[", nchar(col[long]), " chars total]")
    }
    col
  })
  utils::write.csv(preview_df, preview_path, row.names = FALSE, na = "")
  log_status("  wrote preview: ", preview_path,
             " (", nrow(update_df), " rows, ", length(updates), " events; ",
             "long cells truncated for readability)")

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
