#!/bin/bash

projectsFile="${PWD}/projects.lst"

cd ..
while read project; do
  if [ "${project}" == "" ] ; then
  	continue;
  fi
  echo "Status of ${project}..."
  cd ${project}
  git status -s
  cd ..
  echo
done < ${projectsFile}

echo "Done."
exit 0
