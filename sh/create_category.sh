#!/bin/bash
curl  -X 'POST' "http://localhost:8000/categories/create_category?token=qwerty1" -F 'category_name=comedy' -F 'maternal_category_name=films'