#!/bin/bash
curl -X 'PUT' "http://localhost:8000/tags/edit_tag?token=qwerty1" -F 'old_tag_name=victory' -F 'new_tag_name=big victory'

