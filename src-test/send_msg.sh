#/!bin/bash

curl -H "Content-Type: application/json" -d "{\"msg\":\"test\"}" "http://localhost:8000/send"
