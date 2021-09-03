#!/bin/bash
curl  -X 'DELETE' -G "http://localhost:8000/drafts/delete_draft" --data-urlencode 'token'='qwerty5' --data-urlencode 'draft_id'='3'