#!/bin/bash
curl -X 'GET' -G "http://localhost:8000/news/2/comments" --data-urlencode 'page=10'