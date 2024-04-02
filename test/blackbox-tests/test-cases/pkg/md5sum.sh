#!/bin/bash

if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS
  md5 "$1" | awk '{ print $4 }'
else
  # Linux
  md5sum "$1" | awk '{ print $1 }'
fi
