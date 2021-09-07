#!/bin/bash
curl -X 'POST' -G "http://localhost:8000/news/1/comments/add_comment?token=qwerty7" -F 'comment_text=lets rock'