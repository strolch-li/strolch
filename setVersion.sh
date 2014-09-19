#!/bin/bash
if [ "$#" != "2" ] ; then
    echo "ERROR: Wrong arguments!"
    echo "Usage: $0 <old_version> <new_version>"
    exit 1
fi

#if ! mvn -f pom.xml versions:set -DnewVersion="${1}" -DallowSnapshots=true -DgenerateBackupPoms=false ; then
#    echo "ERROR: Failed to change version in root!"
#    exit 1
#fi
#if ! mvn -f versioning_eitchnet_pom.xml versions:set -DnewVersion="${1}" -DallowSnapshots=true -DgenerateBackupPoms=false ; then
#    echo "ERROR: Failed to change version in submodule!"
#    exit 1
#fi

old_version="$1"
new_version="$2"

root="${PWD}"

function fail() {
  echo "ERROR: Failed to set version of $1"
  exit 1
}

cd "${root}"
if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:version" -v ${new_version} pom.xml 2>/dev/null ; then
  fail "${root}/pom.xml"
fi
if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:strolch.version/my:version" -v ${new_version} pom.xml 2>/dev/null ; then
  fail "${root}/pom.xml"
fi
if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:parent/my:version" -v ${new_version} pom.xml 2>/dev/null ; then
  fail "${root}/pom.xml"
fi
for project in li.* ; do
  cd "${root}/${project}"
  if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:version" -v ${new_version} pom.xml 2>/dev/null ; then
    fail "${root}/${project}/pom.xml"
  fi
  if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:strolch.version/my:version" -v ${new_version} pom.xml 2>/dev/null ; then
    fail "${root}/${project}/pom.xml"
  fi
  if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:parent/my:version" -v ${new_version} pom.xml 2>/dev/null ; then
    fail "${root}/${project}/pom.xml"
  fi
done

cd "${root}"
for project in ch.* ; do
  cd "${root}/${project}"
  if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:version" -v ${new_version} pom.xml 2>/dev/null ; then
    fail "${root}/${project}/pom.xml"
  fi
  if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:strolch.version/my:version" -v ${new_version} pom.xml 2>/dev/null ; then
    fail "${root}/${project}/pom.xml"
  fi
  if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:parent/my:version" -v ${new_version} pom.xml 2>/dev/null ; then
    fail "${root}/${project}/pom.xml"
  fi
done

cd "${root}"
if ! sed --in-place "s/${old_version}/${new_version}/" li.strolch.dev/createBundle.sh 2>/dev/null
  fail "${root}/li.strolch.dev/createBundle.sh"
fi
