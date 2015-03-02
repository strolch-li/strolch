#!/bin/bash

if [ ! $(which xmlstarlet) ] ; then
  echo "ERROR: xmlstarlet is missing!"
  exit 1
fi

function usage() {
  echo "Usage: $0 [-c] [-o <old_version>] [-n <new_version>]" 1>&2;
  echo "  -c                    commit"
  echo "  -o <old_version>      old version e.g. 1.0.0-SNAPSHOT"
  echo "  -n <new_version>      new version e.g. 1.0.0-RC3"
  echo ""
  exit 1;
}

# get arguments
while getopts ":b:o:n:r:cp" arg; do
    case "${arg}" in
        c)
            c="true"
            ;;
        o)
            o=${OPTARG}
            ;;
        n)
            n=${OPTARG}
            ;;
        *)
            echo "ERROR: Unknown arg ${arg}"
            usage
            ;;
    esac
done
shift $((OPTIND-1))

# validate arguments
if [ -z "${o}" ] || [ -z "${n}" ] ; then
  echo "ERROR: Missing an argument!"
  usage
fi

#if ! mvn -f pom.xml versions:set -DnewVersion="${1}" -DallowSnapshots=true -DgenerateBackupPoms=false ; then
#    echo "ERROR: Failed to change version in root!"
#    exit 1
#fi
#if ! mvn -f versioning_eitchnet_pom.xml versions:set -DnewVersion="${1}" -DallowSnapshots=true -DgenerateBackupPoms=false ; then
#    echo "ERROR: Failed to change version in submodule!"
#    exit 1
#fi

old_version="${o}"
new_version="${n}"
commit="${c}"

declare SCRIPT_NAME="${0##*/}"
declare SCRIPT_DIR="$(cd ${0%/*} ; pwd)"
root="$(cd ${SCRIPT_DIR}/.. ; pwd)"
echo "old_version=${old_version}"
echo "new_version=${new_version}"
echo "root=${root}"

function fail() {
  echo -e "\nERROR: Failed to set version of $1"
  exit 1
}


cd "${root}"
if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:version" -v ${new_version} pom.xml  ; then
  fail "${root}/pom.xml"
fi
if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:parent/my:version" -v ${new_version} pom.xml 2>/dev/null ; then
  fail "${root}/pom.xml"
fi
if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:strolch.version" -v ${new_version} pom.xml 2>/dev/null ; then
  fail "${root}/pom.xml"
fi
if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:eitchnet.xmlpers.version" -v ${new_version} pom.xml 2>/dev/null ; then
  fail "${root}/pom.xml"
fi
if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:eitchnet.utils.version" -v ${new_version} pom.xml 2>/dev/null ; then
  fail "${root}/pom.xml"
fi
if ! xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:eitchnet.privilege.version" -v ${new_version} pom.xml 2>/dev/null ; then
  fail "${root}/pom.xml"
fi


cd "${root}" 
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
create_script="li.strolch.dev/createBundle.sh"
if ! sed -i.bck "s/${old_version}/${new_version}/" "${create_script}" 2>/dev/null ; then
  fail "${root}/${create_script}"
fi
rm -f "${create_script}.bck"


# Commit new version
if [ -n "${commit}" ] ; then
  echo "INFO: Committing new version ${new_version}"
  git submodule foreach git add .
  git submodule foreach git commit -m "[Project] Bumped version from ${old_version} to ${new_version}"
  git add .
  git commit -m "[Project] Bumped version from ${old_version} to ${new_version}"
else
  echo "INFO: NOT committing new version"
fi


echo -e "\nINFO: Bumped version from ${old_version} to ${new_version}"

exit 0
