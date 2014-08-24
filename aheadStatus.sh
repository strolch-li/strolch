#!/bin/bash

projectsFile="${PWD}/projects.lst"

function logAheadCount() {
  aheadCount="$(git rev-list origin..master --count)"
#  if [ "${aheadCount}" -ne 0 ] ; then
    project="${PWD}"
  	project="${project##*/}"
  	echo "== Status: ${project}..."
    echo "${aheadCount} commits need pushing for ${project}"
#  fi
}

cd ..
while read project; do
  if [ "${project}" == "" ] ; then
  	continue;
  fi

  array=(${project//:/ })
  name="${array[0]}"
  tag="${array[1]}"

  if [ "${name}" == "" ] ||  [ "${tag}" == "" ] ; then
    echo -e "ERROR: Invalid project: ${project}! Must have form <git_name:tag>"
    exit 1
  fi

  cd "${name}"
  logAheadCount
  cd ..
done < ${projectsFile}

echo "Done."
exit 0
