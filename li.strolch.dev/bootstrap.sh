#!/bin/bash

projectsFile="${PWD}/projects.lst"

echo "Cloning projects using SSH..."

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

  echo "== Cloning ${project}..."
  if [ -d "${name}" ] ; then
    echo "Project ${name} already cloned."
  else
    if ! git clone "git@github.com:eitchnet/${name}.git" "${name}" ;then
      exit 1
    fi
    echo
  fi
done < ${projectsFile}

echo "Done."
exit 0
