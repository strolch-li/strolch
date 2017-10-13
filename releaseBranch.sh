#!/bin/bash

# usage
if [ $# != 2 ] ; then
  echo -e "Usage: ${0} <source_branch> <version>"
  exit 1
fi

sourceBranch=${1}
releaseVersion=${2}

# cleanup trap
function cleanup {
  echo -e "\nINFO: Cleaning up..."
  git checkout ${sourceBranch}
  git checkout .
  git branch -D temp
}
trap cleanup EXIT

# Confirm
echo -e "INFO: Do you want to release version ${releaseVersion} from branch ${sourceBranch}? y/n"
read a
if [[ "${a}" != "y" && "${a}" != "Y" ]] ; then
  exit 0;
fi

# validate tag and release branch do not exist
echo -e "INFO: Fetching tags and branches and validating they do not exist..."
if ! git fetch --all --tags > /dev/null ; then
  echo -e "ERROR: Tags and branches could not be fetched!"
  exit 1
fi
if git tag | grep "${releaseVersion}" > /dev/null ; then
  echo -e "ERROR: Tag already exists!"
  exit 1
fi
if git branch --all | grep "release/${releaseVersion}" > /dev/null ; then
  echo -e "ERROR: Tag already exists!"
  exit 1
fi

# make sure gpg-agent is available and loaded
echo -e "\nINFO: Searching for gpg-agent..."
if ! which gpg-agent ; then
  echo -e "ERROR: gpg-agent missing!"
fi
if ! gpg-agent 2>&1 | grep "running and available" ; then
  echo -e "WARN: gpg-agent not running, trying to start..."
  if ! gpg-agent --daemon ; then
    echo -e "ERROR: Failed to initialize gpg-agent, please make sure it is up and running before continuing!"
    exit 1
  fi
  if ! gpg-agent 2>&1 | grep "running and available" ; then
    echo -e "ERROR: Failed to initialize gpg-agent, please make sure it is up and running before continuing!"
    exit 1
  fi
fi

# Checkout source branch
echo -e "\nINFO: Checking out source branch ${sourceBranch}"
if ! git checkout ${sourceBranch} ; then
  echo -e "ERROR: Failed to checkout branch ${sourceBranch}"
  exit 1
fi

# create new release branch, and temp tag
echo -e "\nINFO: Creating release and temp branches..."
if ! git checkout -b release/${releaseVersion} ; then 
  echo -e "ERROR: Failed to create release branch!"
  exit 1
fi
if ! git checkout -b temp ; then
  echo -e "ERROR: Failed to create temp branch!"
  exit 1
fi

# set release version
echo -e "\nINFO: Setting version..."
if ! mvn versions:set -DgenerateBackupPoms=false -DnewVersion=${releaseVersion} > /dev/null ; then
  echo -e "ERROR: Failed to set new version!"
  exit 1
fi

# build
echo -e "\nINFO: Doing a build with new version..."
if ! mvn clean package -DskipTests > /dev/null ; then
  echo -e "ERROR: Failed to build with new version!"
  exit 1
fi

# commit to tag
echo -e "\nINFO: Creating tag..."
if ! git add . ; then
  echo -e "ERROR: Failed to git add"
  exit 1
fi
if ! git commit -m "[Project] Set new version ${releaseVersion}" ; then
  echo -e "ERROR: Failed to git commit"
  exit 1
fi
if ! git tag --sign --message "[Project] New Version ${releaseVersion}" ${releaseVersion} ; then
  echo -e "ERROR: Failed to git tag"
  exit 1
fi

# local install
echo -e "\nINFO: Installing new version..."
if ! mvn source:jar install -DskipTests > /dev/null ; then
  echo -e "ERROR: Failed to install new version!"
  exit 1
fi

# git push
echo -e "INFO: Pushing to origin..."
if ! git push origin release/${releaseVersion} ; then
  echo -e "ERROR: Failed to push release branch"
  exit 1
fi
if ! git push origin ${releaseVersion} ; then
  echo -e "ERROR: Failed to push tag"
  exit 1
fi
echo -e "\nINFO: Pushed release branch and tag for release version ${releaseVersion}"

echo -e "\nINFO: Release ${releaseVersion} created."
echo -e "INFO: Don't forget to update snapshot version!"
exit 0
