#!/bin/bash
curl  -X 'DELETE' "http://localhost:8000/categories/delete_category?token=qwerty1" -F 'category_name=drama'