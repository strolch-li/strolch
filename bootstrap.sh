#!/bin/bash

projectsFile="${PWD}/projects.lst"

echo "Cloning projects using SSH..."

cd ..
while read project; do
  if [ "${project}" == "" ] ; then
  	continue;
  fi
  if [ -d "${project}" ] ; then
    echo "Project ${project} already cloned."
  else
    if ! git clone "git@github.com:eitchnet/${project}.git" "${project}" ;then
      exit 1
    fi
    echo
  fi
done < ${projectsFile}

echo "Done."
exit 0