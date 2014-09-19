#!/bin/bash
if [ "$#" != "1" ] ; then
    echo "ERROR: Version missing or wrong arguments!"
    exit 1
fi

if ! mvn -f pom.xml versions:set -DnewVersion="${1}" -DallowSnapshots=true -DgenerateBackupPoms=false ; then
    echo "ERROR: Failed to change version in root!"
    exit 1
fi
if ! mvn -f versioning_eitchnet_pom.xml versions:set -DnewVersion="${1}" -DallowSnapshots=true -DgenerateBackupPoms=false ; then
    echo "ERROR: Failed to change version in submodule!"
    exit 1
fi

