#!/bin/sh
#
# Wait until a file exists, then delete the file and exit
#

set -u
FILE=$1
until test -e $FILE
do
    sleep 0.1
done
rm $FILE
