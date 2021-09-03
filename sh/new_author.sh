#!/bin/bash
curl  -X 'POST' -G "http://localhost:8000/new_author?token=qwerty1" -F 'author_login=cybelin' -F 'description=cook'