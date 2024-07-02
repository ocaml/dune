#!/bin/sh
#
# Wait until a file exists, then delete the file and exit
#

set -u

FILE=$1
MAX_WAIT=20 # Maximum number of attempts
SLEEP_DURATION=0.1 # How long to sleep between attempts

for i in $(seq 1 $MAX_WAIT); do
    if test -e "$FILE"; then
        rm "$FILE"
        exit 0
    fi
    sleep $SLEEP_DURATION
done

echo "Timeout waiting for file $FILE"
exit 1
