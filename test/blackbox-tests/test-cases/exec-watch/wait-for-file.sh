#!/bin/sh
#
# Wait until a file exists, then delete the file and exit
#

set -u
FILE=$1

# Note that exec watch mode has a chance to miss filesystem events, so tests of
# watch mode can be flaky. This script is used to detect the execution of a
# program launched by dune in exec watch mode (the program creates the file
# passed as $FILE). If dune missed a filesystem event intended to trigger the
# execution of the program, the program will never run and the file will never
# be created. This timeout prevents the test from hanging in such a scenario.
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
