#!/bin/bash

projectsFile="${PWD}/projects.lst"

cd ..
while read project; do
  if [ "${project}" == "" ] ; then
  	continue;
  fi
  echo "Pulling ${project}..."
  cd ${project}
  git pull
  cd ..
  echo
done < ${projectsFile}

echo "Done."
exit 0