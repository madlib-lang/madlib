#!/bin/bash
for i in {1..500000}
do
#   curl -X POST -d '{ "code": "import IO from \"IO\"\n" }' http://localhost:3000/api/playgrounds
  sleep 0.02
#   curl http://localhost:3000
  curl https://madlib.space
done