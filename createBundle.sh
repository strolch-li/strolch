#!/bin/bash

if [ $# != 1 ] ; then
  echo "Usage: ${0} <version>"
  exit 1
fi

projectName=strolch_bundle
projectVersion=${1}
bundle_name="${projectName}-${projectVersion}"
DIST_STROLCH="/var/www/eitch/strolch.li/dist/snapshot"
DEPLOY_SERVER="hosting.eitchnet.ch"
ROOT="${PWD}"
workDir="${ROOT}/target/${bundle_name}"

# Create all needed packages
#echo "INFO: Building projects..."
#if ! mvn -DskipTests clean package ; then
#  echo "ERROR: Failed to build packages!"
#  exit 1
#fi

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

echo "INFO: Copying bundles..."
cd ${ROOT}
for project in ls * ; do
  if [ ! -d ${project} ] ; then
    continue 
  fi
  if [ ${project} == "dev" ] || [ ${project} == "target" ] ; then
    continue 
  fi

  echo "INFO: Copying ${project} packages..."
  if ls ${project}/target/*.jar 2>/dev/null ; then
    if ! cp ${project}/target/*.jar "${workDir}" ; then
      echo "ERROR: Failed to copy package for project ${project}."
      exit 1
    fi
  fi
  if ls ${project}/target/*.war 2>/dev/null ; then
    if ! cp ${project}/target/*.war "${workDir}" ; then
      echo "ERROR: Failed to copy wars for project ${project}."
      exit 1
    fi
  fi
  if ls ${project}/target/*.tar.gz 2>/dev/null ; then
    if ! cp ${project}/target/*.tar.gz "${workDir}" ; then
      echo "ERROR: Failed to copy tarballs for project ${project}."
      exit 1
    fi
  fi

done

echo "INFO: Creating tarball..."
cd ${workDir}/..
if ! tar -cvzf ${bundle_name}.tar.gz ${bundle_name} ; then
  echo "ERROR: Failed to make bundle."
  exit 1
fi

if [ "$(hostname -f)" == "${DEPLOY_SERVER}" ] ; then
  echo "INFO: Publishing..."
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
