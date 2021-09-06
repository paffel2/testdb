#!/bin/bash
curl  -X 'PUT' "http://localhost:8000/authors/edit_author?token=qwerty1" -F 'author_id=3' -F 'new_description=Cook'