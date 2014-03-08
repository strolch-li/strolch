#!/bin/bash

projectsFile="${PWD}/projects.lst"

cd ..
while read project; do
  echo "Pulling ${project}..."
  cd ${project}
  git pull
  cd ..
  echo
done < ${projectsFile}

echo "Done."
exit 0