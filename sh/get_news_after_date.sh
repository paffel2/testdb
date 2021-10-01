#!/bin/bash
curl -X 'GET' -G "http://localhost:8000/news" --data-urlencode 'after_date=2020-10-03'