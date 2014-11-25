#!/bin/bash

curl -H "Content-Type: application/json" -d "{\"location\":\"http://localhost:5581/req\",\"name\":\""${1}"\"}" "http://localhost:8000/register"
