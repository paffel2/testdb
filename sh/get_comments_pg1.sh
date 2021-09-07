#!/bin/bash
curl -X 'GET' -G "http://localhost:8000/news/1/comments" --data-urlencode 'page=1'