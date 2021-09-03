#!/bin/bash
curl -X 'POST' -G "http://localhost:8000/registration" -F 'login=abc789' -F 'password=abc789' -F 'f_name=Aaron' -F 'l_name=Aaronson' -F "avatar"="@img500.jpg"