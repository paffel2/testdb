#!/bin/bash
curl -X 'DELETE' -G "http://localhost:8000/tags/delete_tag" --data-urlencode 'tag_name=science' --data-urlencode 'token=qwerty1'