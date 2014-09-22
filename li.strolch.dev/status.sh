#!/bin/bash

projectsFile="${PWD}/projects.lst"

cd ..

git status
git submodule foreach git status

echo "Done."
exit 0
