"""
a360_csv_to_table.py

Convert a CSV-string field on a Teamworks AMS (Smartabase) event into repeating
table rows, and push the result back onto the same event.

Flow
----
1. Authenticate with HTTP Basic Auth (v1 API).
2. Sync users (/api/v1/usersynchronise) to get the list of user IDs the account
   can see.
3. Search events (/api/v1/eventsearch) for each configured form, limited to
   events DATED within the last LOOKBACK_HOURS. (We use eventsearch, not
   synchronise: synchronise filters by last-modified time, not event date.)
4. For every event whose target table fields are still empty, parse the CSV text
   in the source field into rows/columns and APPEND them to the event's existing
   rows (keeping all the original data intact).
5. Push the merged event back with /api/v1/eventimport, using existingEventId so
   AMS updates the event in place instead of creating a new one.

Configuration comes from environment variables (see below). Nothing is written
back while SB_DRY_RUN is truthy (the default) -- the script pulls data down,
dumps it to events_dump.json for inspection, and prints what it *would* push.
Set SB_DRY_RUN=false once you've confirmed the JSON looks right.
"""

import os
from loading import load_dotenv
load_dotenv()  # load .env file if present
import csv
import io
import json
import datetime

import requests
from base64 import b64encode

# --------------------------------------------------------------------------- #
# Configuration
# --------------------------------------------------------------------------- #
AMS_SERVER = os.getenv("SB_SERVER", "your-server.smartabase.com")  # host only
AMS_APP = os.getenv("SB_APP", "your-site")                         # site / app id
USERNAME = os.getenv("SB_USERNAME")
PASSWORD = os.getenv("SB_PASSWORD")
X_APP_ID = os.getenv("SB_APP_ID", "usss.integration.v1")

# How far back to look for events to process.
LOOKBACK_HOURS = int(os.getenv("SB_LOOKBACK_HOURS", "24"))

# Safety switch: while true, pull + report only, never write back.
DRY_RUN = os.getenv("SB_DRY_RUN", "true").lower() not in ("false", "0", "no")

BASE_URL = f"https://{AMS_SERVER}/{AMS_APP}/api/v1"

# Define Form Name, Source Field, Columns, Target Field
timeseries_dicts = [  # list of dictionaries
    {
        "form_name": "Polar Summary - Training",   # Teamworks AMS Form
        "source_field": "Heart Rate Series",       # csv string field
        "source_csv_columns": ["Timestamp", "Heart Rate"],  # csv string header
        "target_fields": ["Timestamp", "Heart Rate"],       # table fields to push to
    }
]

# --------------------------------------------------------------------------- #
# Auth
# --------------------------------------------------------------------------- #
if not USERNAME or not PASSWORD:
    raise SystemExit("Set SB_USERNAME and SB_PASSWORD environment variables.")

_credentials = b64encode(f"{USERNAME}:{PASSWORD}".encode()).decode()
HEADERS = {
    "Authorization": f"Basic {_credentials}",
    "Content-Type": "application/json",
    "X-APP-ID": X_APP_ID,
}


# --------------------------------------------------------------------------- #
# Step 1: Sync users  (POST /api/v1/usersynchronise)
# --------------------------------------------------------------------------- #
def sync_users(last_sync_time=0, cached_user_ids=None):
    """Return every user the account can see, following cursor pagination.

    100 users per page. Returns (user_cache, last_synchronisation_time).
    """
    user_cache = {}
    cursor = ""
    last_sync = last_sync_time

    while True:
        payload = {
            "lastSynchronisationTimeOnServer": last_sync_time,
            "userIds": cached_user_ids or [],
            "paginate": "True",
            "cursor": cursor,
        }
        resp = requests.post(
            f"{BASE_URL}/usersynchronise?informat=json&format=json",
            headers=HEADERS,
            json=payload,
        )
        resp.raise_for_status()
        data = resp.json()

        for user in data.get("users", []):
            user_cache[user["userId"]] = user

        # Merges / deletions (only relevant on incremental syncs).
        for merged in data.get("mergedUsers", []):
            user_cache.pop(merged.get("oldId"), None)
            user_cache[merged["newId"]] = merged
        for deleted_id in data.get("idsOfDeletedUsers", []):
            user_cache.pop(deleted_id, None)

        last_sync = data.get("lastSynchronisationTimeOnServer", last_sync)

        cursor = data.get("cursor")
        if not cursor:  # null / "" on the final page
            break

    return user_cache, last_sync


# --------------------------------------------------------------------------- #
# Step 2: Search events by date  (POST /api/v1/eventsearch)
#
# NOTE: We deliberately use /eventsearch, not /synchronise.
#   * /synchronise filters by lastSynchronisationTimeOnServer, which is a
#     *last-modified* cursor for incremental caching -- it does NOT filter by
#     the event's own date. Passing "now - 24h" there returns only events
#     edited in the last 24h, so events dated today but entered earlier are
#     missed (this is why a date-window query returned 0 events).
#   * /eventsearch does real server-side date filtering via startDate /
#     finishDate in dd/mm/yyyy, which is what we want here.
# --------------------------------------------------------------------------- #
def search_events(form_name, user_ids, start_date, finish_date):
    """Return events for a form whose date falls within [start_date, finish_date].

    Dates are dd/mm/yyyy strings. Follows cursor pagination. Returns event dicts.
    """
    events = []
    cursor = None  # omit on first page

    while True:
        payload = {
            "formNames": [form_name],   # eventsearch takes a LIST of form names
            "startDate": start_date,    # dd/mm/yyyy, inclusive lower bound
            "finishDate": finish_date,  # dd/mm/yyyy, inclusive upper bound
            "userIds": user_ids,
            "paginate": True,           # top-level pagination for eventsearch
        }
        if cursor:
            payload["cursor"] = cursor

        resp = requests.post(
            f"{BASE_URL}/eventsearch?informat=json&format=json",
            headers=HEADERS,
            json=payload,
        )
        resp.raise_for_status()
        data = resp.json()

        events.extend(data.get("events", []))

        # Docs prose says next_cursor; OpenAPI schema says cursor. Accept either.
        cursor = data.get("next_cursor") or data.get("cursor")
        if not cursor:
            break

    return events


# --------------------------------------------------------------------------- #
# Event helpers
#
# AMS events use the same row/pairs shape as the eventimport body:
#   "rows": [ { "row": 0, "pairs": [ {"key": <field>, "value": <value>} ] } ]
# --------------------------------------------------------------------------- #
def get_rows(event):
    """Existing rows for an event, normalised to a list."""
    return event.get("rows") or []


def pair_value(event, field_name):
    """First non-empty value for `field_name` across all rows of an event."""
    for row in get_rows(event):
        for pair in row.get("pairs", []):
            if pair.get("key") == field_name and str(pair.get("value", "")).strip():
                return pair["value"]
    return None


def target_fields_empty(event, target_fields):
    """True when none of the target fields hold a value yet (nothing appended)."""
    return all(pair_value(event, field) is None for field in target_fields)


def parse_csv_field(text, source_columns, target_fields):
    """Turn the CSV string in a source field into a list of pairs-lists.

    Each output element is the `pairs` list for one new table row:
        [ {"key": target_fields[j], "value": <cell>}, ... ]

    Columns are matched by header name (source_columns), then written out under
    the index-aligned target_fields name.
    """
    text = (text or "").strip()
    if not text:
        return []

    reader = csv.DictReader(io.StringIO(text))
    # If the header doesn't match what we expect, fall back to positional parsing.
    header_ok = reader.fieldnames and all(c in reader.fieldnames for c in source_columns)

    rows_pairs = []
    if header_ok:
        for record in reader:
            pairs = [
                {"key": target_fields[j], "value": str(record.get(source_columns[j], "")).strip()}
                for j in range(len(target_fields))
            ]
            rows_pairs.append(pairs)
    else:
        # No usable header: treat every line as positional data.
        for line in text.splitlines():
            cells = [c.strip() for c in line.split(",")]
            if not any(cells):
                continue
            # Skip a leading header row if it matches the column names.
            if cells[: len(source_columns)] == source_columns:
                continue
            pairs = [
                {"key": target_fields[j], "value": cells[j] if j < len(cells) else ""}
                for j in range(len(target_fields))
            ]
            rows_pairs.append(pairs)

    return rows_pairs


def build_import_payload(event, form_name, new_rows_pairs):
    """Build the /eventimport body: existing rows preserved + new rows appended."""
    existing_rows = get_rows(event)
    next_index = (max((r.get("row", 0) for r in existing_rows), default=-1) + 1)

    appended = []
    for offset, pairs in enumerate(new_rows_pairs):
        appended.append({"row": next_index + offset, "pairs": pairs})

    # userId can arrive as a bare int or as {"userId": <int>} depending on export.
    raw_user = event.get("userId")
    user_id = raw_user.get("userId") if isinstance(raw_user, dict) else raw_user

    payload = {
        "formName": form_name,
        "startDate": event.get("startDate"),
        "finishDate": event.get("finishDate", event.get("startDate")),
        "startTime": event.get("startTime"),
        "userId": {"userId": user_id},
        "existingEventId": str(event.get("id")),  # update in place
        "rows": existing_rows + appended,
    }
    # Only include finishTime if the event has one (API defaults it otherwise).
    if event.get("finishTime"):
        payload["finishTime"] = event["finishTime"]

    return payload


def import_event(payload):
    """POST an event back to AMS (create/update)."""
    resp = requests.post(
        f"{BASE_URL}/eventimport?informat=json&format=json",
        headers=HEADERS,
        json=payload,
    )
    resp.raise_for_status()
    return resp.json() if resp.content else {}


# --------------------------------------------------------------------------- #
# Main
# --------------------------------------------------------------------------- #
def main():
    now = datetime.datetime.now()
    window_start = now - datetime.timedelta(hours=LOOKBACK_HOURS)
    # /eventsearch filters by the event's own date (dd/mm/yyyy). Date granularity
    # means we widen to whole days so nothing in the window is missed; if the
    # lookback crosses midnight this naturally spans yesterday..today.
    start_date = window_start.strftime("%d/%m/%Y")
    finish_date = now.strftime("%d/%m/%Y")

    print(f"Mode: {'DRY RUN (no writes)' if DRY_RUN else 'LIVE (will push back)'}")
    print(f"Searching events dated {start_date} .. {finish_date} "
          f"(last {LOOKBACK_HOURS}h)")

    # Step 1: users
    user_cache, _ = sync_users()
    user_ids = list(user_cache.keys())
    print(f"Synced {len(user_ids)} users")

    dump = {}          # form_name -> raw events, saved for inspection
    total_updates = 0

    for spec in timeseries_dicts:
        form_name = spec["form_name"]
        source_field = spec["source_field"]
        source_columns = spec["source_csv_columns"]
        target_fields = spec["target_fields"]

        # Step 2: events for this form in the date window
        events = search_events(form_name, user_ids, start_date, finish_date)
        dump[form_name] = events
        print(f"\nForm '{form_name}': {len(events)} event(s) in window")

        for event in events:
            event_id = event.get("id")

            # Only process events whose table fields are still empty.
            if not target_fields_empty(event, target_fields):
                print(f"  event {event_id}: target fields already populated -> skip")
                continue

            csv_text = pair_value(event, source_field)
            if not csv_text:
                print(f"  event {event_id}: source field '{source_field}' empty -> skip")
                continue

            new_rows_pairs = parse_csv_field(csv_text, source_columns, target_fields)
            if not new_rows_pairs:
                print(f"  event {event_id}: nothing parsed from source field -> skip")
                continue

            payload = build_import_payload(event, form_name, new_rows_pairs)

            print(f"  event {event_id}: appending {len(new_rows_pairs)} row(s) "
                  f"(existing rows: {len(get_rows(event))})")

            if DRY_RUN:
                continue

            import_event(payload)
            total_updates += 1
            print(f"  event {event_id}: pushed back successfully")

    # Save what we pulled so the exact JSON shape can be inspected / shared.
    with open("events_dump.json", "w") as fh:
        json.dump(dump, fh, indent=2)
    print(f"\nSaved pulled events to events_dump.json")

    if DRY_RUN:
        print("DRY RUN complete -- no events were written. "
              "Set SB_DRY_RUN=false to push.")
    else:
        print(f"Done. Pushed {total_updates} event(s).")


if __name__ == "__main__":
    main()
