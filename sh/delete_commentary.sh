#!/bin/bash
curl  -X 'DELETE' -G "http://localhost:8000/news/1/comments/delete_comment?token=qwerty1" --data-urlencode 'comment_id=1'