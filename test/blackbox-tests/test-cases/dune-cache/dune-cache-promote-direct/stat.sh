#!/usr/bin/env bash
stat="stat"
uname=$(uname -s)
if [ "$uname" == "Darwin" ]; then
    stat="gstat"
fi;
$stat $@
