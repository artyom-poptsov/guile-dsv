#!/bin/bash

TEST_FILE="$1"
TEST_FILE_NAME=$(basename ${TEST_FILE})
TEST_FILE_FORMAT="$2"
GIT_COMMIT=$(git rev-parse --short HEAD)
TIMESTAMP=$(date -Iseconds)
OUTPUT="${TEST_FILE_NAME}-${GIT_COMMIT}-${TIMESTAMP}.log"
COUNT=5

for i in $(seq $COUNT); do
    /usr/bin/time -o "$OUTPUT" -a -f "%e" \
                  ./pre-inst-env ./utils/dsv \
                  --log-driver=null \
                  --file-format="$TEST_FILE_FORMAT" \
                  --summary \
                  "$TEST_FILE" > /dev/null
done
