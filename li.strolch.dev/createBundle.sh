#!/bin/bash

projectName=strolch_bundle
projectVersion=1.1.0-SNAPSHOT
bundle_name="${projectName}-${projectVersion}"
DIST_STROLCH="/var/www/eitch/strolch.li/dist/snapshot"
DEPLOY_SERVER="hosting.eitchnet.ch"
ROOT="${PWD}"
workDir="${ROOT}/target/${bundle_name}"
projectsFile="${ROOT}/projects_all.lst"

# first we create all needed packages
cd ..
if ! mvn -DskipTests clean package ; then
  echo "ERROR: Failed to build packages!"
  exit 1
fi

# Make sure the work directory exists
if [ -d "${workDir}" ] ; then
  if ! rm -rf "${workDir}" ; then
    echo "ERROR: Failed to clean work dir ${workDir}."
    exit 1
  fi
fi
if ! mkdir -p "${workDir}" ; then
  echo "ERROR: Failed to create work dir ${workDir}."
  exit 1
fi

echo "INFO: Bundling projects..."

# Now copy those packages to our work dir
while read project; do
  if [ "${project}" == "" ] ; then
  	continue;
  fi

  array=(${project//:/ })
  name="${array[0]}"
  tag="${array[1]}"

  if [ "${name}" == "" ] ||  [ "${tag}" == "" ] ; then
    echo -e "ERROR: Invalid project: ${project}! Must have form <git_name:tag>"
    exit 1
  fi

  if [ "${name}" == "li.strolch.dev" ] ; then
  	continue;
  fi

  if ! cd "${ROOT}/../${name}" ; then
    echo "ERROR: Could not switch to directory ${name}"
    exit 1
  fi

  echo "INFO: Copying ${name} packages..."
  if ls target/*.jar 2>/dev/null ; then
    if ! cp target/*.jar "${workDir}" ; then
      echo "ERROR: Failed to copy package for project ${name}."
      exit 1
    fi
  fi
  if ls target/*.war 2>/dev/null ; then
    if ! cp target/*.war "${workDir}" ; then
      echo "ERROR: Failed to copy wars for project ${name}."
      exit 1
    fi
  fi
  if ls target/*.tar.gz 2>/dev/null ; then
    if ! cp target/*.tar.gz "${workDir}" ; then
      echo "ERROR: Failed to copy tarballs for project ${name}."
      exit 1
    fi
  fi

done < ${projectsFile}

cd ${workDir}/..
if ! tar -cvzf ${bundle_name}.tar.gz ${bundle_name} ; then
  echo "ERROR: Failed to make bundle."
  exit 1
fi

if [ "$(hostname -f)" == "${DEPLOY_SERVER}" ] ; then
  if ! cp ${workDir}/* "${DIST_STROLCH}" ; then
    echo "ERROR: Failed to publish packages."
    exit 1
  fi

  if ! mv ${bundle_name}.tar.gz "${DIST_STROLCH}" ; then
    echo "ERROR: Failed to publish bundle."
    exit 1
  fi
fi

echo "Done."
exit 0
