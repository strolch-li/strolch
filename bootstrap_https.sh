#!/bin/bash

projectsFile="${PWD}/projects.lst"

echo "Cloning projects using HTTPS..."

cd ..
while read project; do
  if [ -d "${project}" ] ; then
    echo "Project ${project} already cloned."
  else
    if ! git clone "https://github.com/eitch/${project}.git" "${project}" ;then
      exit 1
    fi
    echo
  fi
done < ${projectsFile}

echo "Done."
exit 0