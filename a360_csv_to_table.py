"""
a360_csv_to_table.py

Convert a CSV-string field on a Teamworks AMS (Smartabase) event into repeating
table rows, and push the result back onto the same event.

Flow
----
1. Authenticate with HTTP Basic Auth (v1 API).
2. Sync users (/api/v1/usersynchronise) to get the list of user IDs the account
   can see.
3. Sync events (/api/v1/synchronise) for each configured form using the
   documented "preferred" incremental pattern: a persisted
   lastSynchronisationTimeOnServer means each run returns only events created or
   changed since the previous run.
4. For every event whose target table fields are still empty, parse the CSV text
   in the source field into rows/columns and APPEND them to the event's existing
   rows (keeping all the original data intact).
5. Push the merged event back with /api/v1/eventimport, using existingEventId so
   AMS updates the event in place instead of creating a new one.

Why /synchronise and not /eventsearch
-------------------------------------
/eventsearch filters by the event's own date (startDate/finishDate). In this
instance the server returned the full history oldest-first and did not honour the
startDate lower bound, so a "last 24h" window never reached recent records.
/synchronise filters by *modification* time instead: anything you just added or
edited -- even an event dated years ago -- comes back as a recent change. That is
exactly "find the new data", and it avoids paging through all of history.

State
-----
lastSynchronisationTimeOnServer is persisted per-form in SB_STATE_FILE
(default sync_state.json). The docs are explicit: persist it and pass it back;
never hardcode 0 on every run (that forces a full history scan and can 500).

  * First run (no state): full pull. Or set SB_SINCE_HOURS to seed the lower
    bound to "now - N hours" for a quick recent-changes-only run.
  * Later runs: only changed/new events. Fast.
  * SB_FULL_RESYNC=true ignores stored state and pulls everything once.

Configuration comes from environment variables (see below). Nothing is written
back while SB_DRY_RUN is truthy (the default) -- the script pulls data down,
dumps it to events_dump.json for inspection, and prints what it *would* push.
Set SB_DRY_RUN=false once you've confirmed the JSON looks right.
"""

import os
from dotenv import load_dotenv
load_dotenv()
import csv
import io
import json
import datetime

import requests
from base64 import b64encode

# --------------------------------------------------------------------------- #
# Configuration
# --------------------------------------------------------------------------- #
AMS_SERVER = os.getenv("SB_SERVER", "usopc.smartabase.com")  # host only
AMS_APP = os.getenv("SB_APP", "athlete360-usss")                         # site / app id
USERNAME = os.getenv("SB_USERNAME")
PASSWORD = os.getenv("SB_PASSWORD")
X_APP_ID = os.getenv("SB_APP_ID", "usss.integration.v1")

# Incremental-sync state (persisted lastSynchronisationTimeOnServer per form).
STATE_FILE = os.getenv("SB_STATE_FILE", "sync_state.json")

# Optional lower bound for the FIRST run only (when there is no stored state):
#   set e.g. SB_SINCE_HOURS=24 to pull only events changed in the last 24h.
#   Leave unset to pull the full history on first run.
SINCE_HOURS = os.getenv("SB_SINCE_HOURS")  # None or an int-as-string

# Ignore stored state and pull everything (then re-persist a fresh timestamp).
FULL_RESYNC = os.getenv("SB_FULL_RESYNC", "false").lower() in ("true", "1", "yes")

# Optional client-side guard: only PROCESS events whose event-date falls in the
# last SB_LOOKBACK_HOURS. Off by default -- incremental sync already limits to
# recent changes. Turn on with SB_DATE_FILTER=true if you also want to restrict
# by the event's own startDate.
DATE_FILTER = os.getenv("SB_DATE_FILTER", "false").lower() in ("true", "1", "yes")
LOOKBACK_HOURS = int(os.getenv("SB_LOOKBACK_HOURS", "24"))

# Safety cap so a first full sync can't loop forever on a runaway cursor.
MAX_PAGES = int(os.getenv("SB_MAX_PAGES", "1000"))

# Safety switch: while true, pull + report only, never write back.
DRY_RUN = os.getenv("SB_DRY_RUN", "true").lower() not in ("false", "0", "no")

BASE_URL = f"https://{AMS_SERVER}/{AMS_APP}/api/v1"

# Define Form Name, Source Field, Columns, Target Field
timeseries_dicts = [  # list of dictionaries
    {
        "form_name": "Polar Summary - Training",   # Teamworks AMS Form
        "source_field": "Split Heart Rates",       # csv string field
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
# Persisted sync state
# --------------------------------------------------------------------------- #
def load_state():
    try:
        with open(STATE_FILE) as fh:
            return json.load(fh)
    except (FileNotFoundError, ValueError):
        return {}


def save_state(state):
    with open(STATE_FILE, "w") as fh:
        json.dump(state, fh, indent=2)


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
# Step 2: Sync events  (POST /api/v1/synchronise)
#
# NOTE on pagination shape: /synchronise takes a NESTED pagination object
#   {"pagination": {"paginate": true, "cursor": "..."}}
# (this differs from /eventsearch, which uses top-level paginate/cursor).
# --------------------------------------------------------------------------- #
def extract_events(data):
    """Pull the events list out of a /synchronise response.

    Different AMS versions nest events differently; handle the known shapes.
    """
    if isinstance(data.get("export"), dict) and "events" in data["export"]:
        return data["export"]["events"]
    if "events" in data:
        return data["events"]
    return []


def next_cursor(data):
    """Return the pagination cursor from a response, or None on the last page."""
    if isinstance(data.get("pagination"), dict):
        cur = data["pagination"].get("cursor")
        if cur:
            return cur
    return data.get("cursor") or data.get("next_cursor")


def sync_events(form_name, user_ids, last_sync_time):
    """Return (events, new_last_sync_time) for a form since last_sync_time.

    Follows cursor pagination to completion. last_sync_time is a server epoch-ms
    timestamp; 0 pulls the full history.
    """
    events = []
    cursor = None
    new_last_sync = last_sync_time
    page = 0

    while True:
        page += 1
        pagination = {"paginate": True}
        if cursor:
            pagination["cursor"] = cursor

        payload = {
            "formNames": [form_name],   # eventsearch takes a LIST of form names
            "startDate": start_date,    # dd/mm/yyyy, inclusive lower bound
            "finishDate": finish_date,  # dd/mm/yyyy, inclusive upper bound
            "resultsPerUser": 1,
            "userIds": user_ids,
            "pagination": pagination,
        }
        resp = requests.post(
            f"{BASE_URL}/synchronise?informat=json&format=json",
            headers=HEADERS,
            json=payload,
        )
        resp.raise_for_status()
        data = resp.json()

        if page == 1:
            # One-time diagnostic: show the real top-level response shape so the
            # events key / cursor key can be confirmed against your instance.
            print(f"    [debug] response keys: {sorted(data.keys())}")

        batch = extract_events(data)
        events.extend(batch)
        new_last_sync = data.get("lastSynchronisationTimeOnServer", new_last_sync)

        cursor = next_cursor(data)
        if not cursor:
            break
        if page >= MAX_PAGES:
            print(f"    [warn] hit MAX_PAGES={MAX_PAGES}; stopping early. "
                  f"Raise SB_MAX_PAGES to pull the rest.")
            break
        if page % 10 == 0:
            print(f"    ...page {page}, {len(events)} events so far")

    return events, new_last_sync


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


def event_date(event):
    """Parse an event's startDate (dd/mm/yyyy) into a date, or None."""
    raw = event.get("startDate")
    if not raw:
        return None
    try:
        return datetime.datetime.strptime(raw, "%d/%m/%Y").date()
    except ValueError:
        return None


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
def resolve_last_sync(state, form_name):
    """Decide the lastSynchronisationTimeOnServer to send for a form."""
    if FULL_RESYNC:
        return 0
    if form_name in state:
        return state[form_name]
    if SINCE_HOURS:
        since = datetime.datetime.now() - datetime.timedelta(hours=int(SINCE_HOURS))
        return int(since.timestamp() * 1000)
    return 0  # first run, full history


def main():
    now = datetime.datetime.now()
    window_start = now - datetime.timedelta(hours=LOOKBACK_HOURS)

    print(f"Mode: {'DRY RUN (no writes)' if DRY_RUN else 'LIVE (will push back)'}")
    if DATE_FILTER:
        print(f"Client-side date filter ON: keeping events dated "
              f"{window_start:%d/%m/%Y}..{now:%d/%m/%Y}")

    state = load_state()

    # Step 1: users
    user_cache, _ = sync_users()
    user_ids = list(user_cache.keys())
    print(f"Synced {len(user_ids)} users")

    dump = {}          # form_name -> processed events, saved for inspection
    total_updates = 0

    for spec in timeseries_dicts:
        form_name = spec["form_name"]
        source_field = spec["source_field"]
        source_columns = spec["source_csv_columns"]
        target_fields = spec["target_fields"]

        last_sync = resolve_last_sync(state, form_name)
        mode = ("full history" if last_sync == 0
                else f"changes since server ts {last_sync}")
        print(f"\nForm '{form_name}': syncing ({mode})")

        # Step 2: events for this form
        events, new_last_sync = sync_events(form_name, user_ids, last_sync)

        # Optional client-side date-window guard.
        if DATE_FILTER:
            before = len(events)
            events = [
                e for e in events
                if (d := event_date(e)) and window_start.date() <= d <= now.date()
            ]
            print(f"  date filter kept {len(events)}/{before} events")

        # Newest-first so the most recent records surface first in logs/dump.
        events.sort(key=lambda e: event_date(e) or datetime.date.min, reverse=True)
        dump[form_name] = events
        print(f"  {len(events)} event(s) to consider")

        for event in events:
            event_id = event.get("id")
            # Only process events whose table fields are still empty.
            if not target_fields_empty(event, target_fields):
                continue

            csv_text = pair_value(event, source_field)
            if not csv_text:
                continue

            new_rows_pairs = parse_csv_field(csv_text, source_columns, target_fields)
            if not new_rows_pairs:
                continue

            payload = build_import_payload(event, form_name, new_rows_pairs)
            print(f"  event {event_id} ({event.get('startDate')}): appending "
                  f"{len(new_rows_pairs)} row(s) (existing rows: {len(get_rows(event))})")

            if DRY_RUN:
                continue

            import_event(payload)
            total_updates += 1
            print(f"  event {event_id}: pushed back successfully")

        # Advance the persisted sync timestamp only on a real (live) run, so
        # repeated dry-runs stay reproducible and keep pulling the same window.
        if new_last_sync and not DRY_RUN:
            state[form_name] = new_last_sync

    if not DRY_RUN:
        save_state(state)

    # Save what we pulled so the exact JSON shape can be inspected / shared.
    with open("events_dump.json", "w") as fh:
        json.dump(dump, fh, indent=2)
    print(f"\nSaved pulled events to events_dump.json")

    if DRY_RUN:
        print("DRY RUN complete -- no events were written, sync state NOT "
              "advanced. Set SB_DRY_RUN=false to push and enable incremental sync.")
    else:
        print(f"Done. Pushed {total_updates} event(s). "
              f"Persisted sync state to {STATE_FILE}.")


if __name__ == "__main__":
    main()
