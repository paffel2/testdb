#!/bin/bash
curl  -X 'PUT' "http://localhost:8000/categories/edit_category?token=qwerty1" -F 'category_name=comedy' -F 'new_maternal=' -F 'new_name=drama'