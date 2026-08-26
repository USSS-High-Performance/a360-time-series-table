## a360_csv_to_table.R ##########################################################
##
## Convert a CSV-string field on Teamworks AMS (Smartabase) events into repeating
## table rows, and push the result into a SEPARATE "time series" form using
## smartabaseR.
##
## Flow:
##   1. Pull events for the SOURCE form over a date range   -> sb_get_event()
##   2. For each event, split the source CSV field into      -> parse_samples()
##      Timestamp / Heart Rate rows
##   3. Create a NEW event in the TARGET form carrying:       -> sb_insert_event()
##        * ID, Detailed Sport Info   (copied from the source event, row 1 only)
##        * Timestamp, Heart Rate      (table fields, one row per sample)
##        * the same athlete + date    (user_id / start_date from the source)
##
## Why a separate form + sb_insert_event(): the Timestamp/Heart Rate table can be
## thousands of rows, so it lives in its own "Polar Summary - Training - Time
## Series" form instead of being pushed back onto the training-summary event.
## We create fresh events there (insert, not update); the source form is only
## read, never modified.
##
## Idempotency: before inserting we read the target form over the same window and
## skip any source event whose ID already exists there, so re-running does not
## create duplicates.
##
## Credentials are read from environment variables -- never hardcode them here:
##   SB_URL       e.g. usopc.smartabase.com/athlete360-usss   (has a default)
##   SB_USERNAME  AMS username                                 (required)
##   SB_PASSWORD  AMS password                                 (required)
##
## Date range (configurable; dd/mm/YYYY). Defaults to the last SB_LOOKBACK_DAYS.
## To reach older records widen it, e.g. SB_START_DATE=01/01/2015.
##
## SB_DRY_RUN=true (default) pulls + reports + writes a preview CSV, but does NOT
## call sb_insert_event(). Set SB_DRY_RUN=false to actually push.
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
# interactive_mode = FALSE is required when running under Rscript: TRUE makes
# smartabaseR show a menu() "Are you sure?" confirmation, which errors out
# non-interactively ("menu() cannot be used non-interactively"). SB_DRY_RUN is
# our real safety gate; set SB_INTERACTIVE=true only inside an interactive
# RStudio session if you want the confirmation prompt.
interactive_mode <- tolower(Sys.getenv("SB_INTERACTIVE", "false")) %in% c("true", "1", "yes")

## Source form / target form / field mapping.
##
##   source_field  : long CSV-string field on the source form to parse
##   source_columns: the header order encoded in that string
##   target_fields : table columns to create in the target form
##   carry_fields  : non-table fields copied verbatim from source -> target
##                   (must exist with the SAME name on both forms)
##   dedup_field   : a carry field that uniquely identifies the source session;
##                   an existing target event with this value is not re-inserted
timeseries_specs <- list(
  list(
    source_form    = "Polar Summary - Training",
    target_form    = "Polar Summary - Training - Time Series",
    source_field   = "Split Heart Rates",           # CSV-string field to parse
    source_columns = c("Timestamp", "Heart Rate"),  # header order in that string
    target_fields  = c("Timestamp", "Heart Rate"),  # table fields on target form
    carry_fields   = c("ID", "Detailed Sport Info"),# non-table fields to copy
    dedup_field    = "ID"                            # skip if already in target
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

## Build the INSERT frame for one target event by expanding it into N table rows
## (N = number of parsed samples). Matches the Smartabase table layout:
##   * user_id + date metadata on EVERY row (keeps athlete/date aligned)
##   * carry (non-table) fields only on the FIRST row (NA elsewhere)
##   * the table fields (Timestamp, Heart Rate) populated across all N rows
## Only these columns are kept -- the source form's other fields (incl. the raw
## "Split Heart Rates" blob and the source event_id) are intentionally dropped.
build_event_insert <- function(event_rows, samples, target_fields, carry_fields) {
  n <- nrow(samples)

  meta_cols <- intersect(
    c("start_date", "end_date", "start_time", "end_time", "user_id"),
    names(event_rows)
  )
  carry_present <- intersect(carry_fields, names(event_rows))
  keep_cols <- c(meta_cols, carry_present)

  # Base = the event's first row (metadata + carry fields), replicated N times.
  base <- event_rows[rep(1, n), keep_cols, drop = FALSE]

  # Carry (non-table) fields belong only on row 1; blank them on rows 2..N.
  if (n > 1 && length(carry_present) > 0) {
    base[2:n, carry_present] <- NA
  }

  # Fill the table fields across every row.
  for (i in seq_along(target_fields)) {
    base[[target_fields[[i]]]] <- samples[[target_fields[[i]]]]
  }

  base
}

## Read the IDs already present in the target form so we don't re-insert them.
## A brand-new/empty form may make sb_get_event() error or return nothing --
## treat that as "no existing events".
existing_target_ids <- function(target_form, dedup_field, start_date, end_date) {
  existing <- tryCatch(
    sb_get_event(
      form = target_form,
      date_range = c(start_date, end_date),
      url = sb_url,
      username = sb_username,
      password = sb_password
    ),
    error = function(e) NULL
  )
  if (is.null(existing) || nrow(existing) == 0 ||
      !dedup_field %in% names(existing)) {
    return(character(0))
  }
  ids <- unique(trimws(as.character(existing[[dedup_field]])))
  ids[nzchar(ids) & !is.na(ids)]
}

## ---- Main ------------------------------------------------------------------
log_status("Mode: ", if (dry_run) "DRY RUN (no writes)" else "LIVE (will insert)")
log_status("Date range: ", start_date, " .. ", end_date)

for (spec in timeseries_specs) {
  source_form    <- spec$source_form
  target_form    <- spec$target_form
  source_field   <- spec$source_field
  source_columns <- spec$source_columns
  target_fields  <- spec$target_fields
  carry_fields   <- if (is.null(spec$carry_fields)) character(0) else spec$carry_fields
  dedup_field    <- spec$dedup_field %||% NA_character_

  log_status("\nSource form '", source_form, "' -> target form '", target_form, "'")
  events <- sb_get_event(
    form = source_form,
    date_range = c(start_date, end_date),
    url = sb_url,
    username = sb_username,
    password = sb_password
  )

  if (is.null(events) || nrow(events) == 0) {
    log_status("  no source events returned for this window")
    next
  }
  if (!"event_id" %in% names(events)) {
    stop("sb_get_event() did not return an event_id column for ", source_form)
  }
  if (!source_field %in% names(events)) {
    log_status("  source field '", source_field, "' not present on this form; skipping")
    next
  }

  missing_carry <- setdiff(carry_fields, names(events))
  if (length(missing_carry) > 0) {
    log_status("  WARNING: carry field(s) not found on source form (will be blank): ",
               paste(missing_carry, collapse = ", "))
  }

  # IDs already in the target form -> skip those source events (dedup).
  skip_ids <- character(0)
  if (!is.na(dedup_field) && nzchar(dedup_field)) {
    skip_ids <- existing_target_ids(target_form, dedup_field, start_date, end_date)
    if (length(skip_ids) > 0) {
      log_status("  target form already has ", length(skip_ids),
                 " event(s) in this window; those IDs will be skipped")
    }
  }

  event_ids <- unique(events$event_id)
  log_status("  ", nrow(events), " source row(s) across ", length(event_ids), " event(s)")

  inserts <- list()
  for (eid in event_ids) {
    event_rows <- events %>% filter(.data$event_id == eid)

    # Dedup: skip if this source session already exists in the target form.
    if (!is.na(dedup_field) && dedup_field %in% names(event_rows)) {
      dedup_val <- trimws(as.character(event_rows[[dedup_field]][[1]]))
      if (nzchar(dedup_val) && dedup_val %in% skip_ids) {
        log_status("  event ", eid, " (", dedup_field, "=", dedup_val,
                   "): already in target; skipping")
        next
      }
    }

    csv_text <- event_rows[[source_field]][[1]]
    samples <- parse_samples(csv_text, source_columns, target_fields)
    if (is.null(samples) || nrow(samples) == 0) {
      next
    }

    inserts[[as.character(eid)]] <-
      build_event_insert(event_rows, samples, target_fields, carry_fields)
    log_status("  event ", eid, " (", event_rows$start_date[[1]], "): ",
               nrow(samples), " sample row(s) -> target insert")
  }

  if (length(inserts) == 0) {
    log_status("  nothing to insert (all skipped or no source data)")
    next
  }

  insert_df <- dplyr::bind_rows(inserts)

  # Preview to disk so the frame can be inspected before pushing. This is only
  # for human inspection -- the value pushed to sb_insert_event() is insert_df.
  # We truncate oversized text cells because Excel caps a single cell at 32,767
  # characters and otherwise spills long values across columns.
  preview_path <- paste0(
    "insert_preview_", gsub("[^A-Za-z0-9]+", "_", target_form), ".csv"
  )
  preview_max_chars <- 80L
  preview_df <- insert_df
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
             " (", nrow(insert_df), " rows, ", length(inserts), " event(s))")

  if (dry_run) {
    log_status("  DRY RUN -- not calling sb_insert_event(). ",
               "Set SB_DRY_RUN=false to push.")
    next
  }

  # table_field lists ONLY the fields that vary per row (Timestamp, Heart Rate).
  # ID / Detailed Sport Info are non-table and stay on row 1. user_id ties each
  # new event to the source athlete; start_date keeps it on the session's date.
  sb_insert_event(
    df = insert_df,
    form = target_form,
    url = sb_url,
    username = sb_username,
    password = sb_password,
    option = sb_insert_event_option(
      table_field = target_fields,
      interactive_mode = interactive_mode
    )
  )
}

log_status("\nDone.")
