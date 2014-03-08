#!/bin/bash

projectsFile="${PWD}/projects.lst"

cd ..
while read project; do
  echo "Pushing ${project}..."
  cd ${project}
  git push
  cd ..
  echo
done < ${projectsFile}

echo "Done."
exit 0