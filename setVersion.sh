#!/bin/bash

if [ $# != 1 ] ; then
  echo "Usage: ${0} <version>"
  exit 1
fi

projectVersion=${1}

mvn versions:set -DgenerateBackupPoms=false -DnewVersion=${projectVersion}

exit 0
