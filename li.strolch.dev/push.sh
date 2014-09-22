#!/bin/bash

function fail() {
  echo -e "\nERROR: Failed to push to upstream"
  exit 1
}

current_branch="$(git branch --quiet | grep "*" | cut -d ' ' -f 2)"
if [ "${current_branch}" == "" ] ; then
  echo -e "\nERROR: No local branch found!"
  fail
fi

cd ..

echo "INFO: Pushing Strolch..."
if ! git push origin "${current_branch}:${current_branch}" ; then
  fail
fi
echo "INFO: Pushing submodules..."
if ! git submodule foreach git push origin "${current_branch}:${current_branch}" ; then
  fail
fi

echo -e "\nINFO: Pushed all Strolch projects."
exit 0
