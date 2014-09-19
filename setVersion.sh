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

cd "${root}"
xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:version" -v ${new_version} pom.xml
xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:strolch.version/my:version" -v ${new_version} pom.xml
xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:parent/my:version" -v ${new_version} pom.xml
for project in li.* ; do
  cd "${root}/${project}"
  xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:version" -v ${new_version} pom.xml
  xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:strolch.version/my:version" -v ${new_version} pom.xml
  xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:parent/my:version" -v ${new_version} pom.xml
done

cd "${root}"
for project in ch.* ; do
  cd "${root}/${project}"
  xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:version" -v ${new_version} pom.xml
  xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:strolch.version/my:version" -v ${new_version} pom.xml
  xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:parent/my:version" -v ${new_version} pom.xml
done

cd "${root}"
sed --in-place "s/${old_version}/${new_version}/" li.strolch.dev/createBundle.sh
