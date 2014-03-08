#!/bin/bash

projectsFile="${PWD}/projects.lst"

function logAheadCount() {
  aheadCount="$(git rev-list origin..master --count)"
#  if [ "${aheadCount}" -ne 0 ] ; then
    project="${PWD}"
  	project="${project##*/}"
    echo "${aheadCount} commits need pushing for ${project}"
#  fi
}

cd ..
while read project; do
  cd "${project}"
  logAheadCount
  cd ..
done < ${projectsFile}

echo "Done."
exit 0