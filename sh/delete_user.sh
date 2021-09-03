#!/bin/bash
curl -X 'DELETE' -G "http://localhost:8000/deleteUser" --data-urlencode 'login=abc789' --data-urlencode 'token=qwerty1'