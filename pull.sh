#!/bin/bash

projectsFile="${PWD}/projects.lst"

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

  echo "Pulling ${project}..."
  cd ${name}
  git pull
  git checkout ${tag}
  cd ..
  echo
done < ${projectsFile}

echo "Done."
exit 0
