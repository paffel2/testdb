#!/bin/bash
curl -X 'POST' -G "http://localhost:8000/tags/create_tag" --data-urlencode 'tag_name=science' --data-urlencode 'token=qwerty1'