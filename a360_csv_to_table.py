import os
import requests

# Authenticate
auth = (os.getenv("SB_USERNAME"), os.getenv("SB_PASSWORD"))

# Define Form Name, Source Field, Columns, Target Field
timeseries_dicts = [ # list of dictionaries
    {
        "form_name": "Polar Summary - Training", # Teamworks AMS Form
        "source_field": "Heart Rate Series", # csv string field
        "source_csv_columns": ["Timestamp", "Heart Rate"], # csv string header
        "target_fields": ["Timestamp", "Heart Rate"] # table fields to push to 
    }
]

# Search Time period for new data in the form name

# Extract Form Name and event-id 

# For each event, If Target Fields are empty, then...
# break up txt in source_field into rows and columns, and push to target_fields, they fields are index aligned to the source_csv_columns

# push back up against the event_id