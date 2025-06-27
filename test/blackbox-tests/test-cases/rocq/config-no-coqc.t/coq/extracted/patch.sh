#!/bin/sh
patch -p0 -r 0 -s --read-only=ignore --follow-symlinks -o - $1 -i extracted/CRelationClasses.mli.patch

