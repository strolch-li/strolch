#!/bin/bash

function fail() {
  echo -e "\nERROR: Failed to pull from upstream"
  exit 1
}

current_branch="$(git branch --quiet | grep "*" | cut -d ' ' -f 2)"
if [ "${current_branch}" == "" ] ; then
  echo -e "\nERROR: No local branch found!"
  fail
fi

cd ..

echo "INFO: Pulling Strolch..."
if ! git pull origin "${current_branch}:${current_branch}" ; then
  fail
fi
echo "INFO: Pulling submodules..."
if ! git submodule foreach git pull origin "${current_branch}:${current_branch}" ; then
  fail
fi

echo -e "\nINFO: Pulled all Strolch projects."
exit 0
