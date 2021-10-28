#!/bin/bash
curl  -X 'DELETE' -G "http://localhost:8000/authors/delete_author?token=qwerty1" -F 'author_login=cybelin'