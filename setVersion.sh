#!/bin/bash
if [ "$#" != "1" ] ; then
    echo "ERROR: Version missing or wrong arguments!"
    exit 1
fi
mvn versions:set -DnewVersion="${1}" -DallowSnapshots=true -DgenerateBackupPoms=false
