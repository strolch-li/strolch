#!/bin/bash

if [ ! $(which xmlstarlet) ] ; then
  echo "ERROR: xmlstarlet is missing!"
  exit 1
fi

function usage() {
  echo "Usage: $0 [-c] [-p] [-o <old_version>] [-n <new_version>]" 1>&2;
  echo "  -c                    commit"
  echo "  -p                    push to origin"
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
        p)
            p="true"
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

old_version="${o}"
new_version="${n}"
commit="${c}"
push="${p}"

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

function setVersion() {

  if [ "$#" != 2 ] ; then
    echo -e "ERROR: Missing version and/or file name"
    exit 1
  fi

  version="$1"
  file="$2"

  if ! [ -f "${file}" ] ; then
    echo -e "ERROR: The file does not exist: ${file}"
    exit 1
  fi

  i=0

  if xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:version" -v ${version} "${file}" 2>/dev/null ; then
    i=$((i+1))
  fi
  if xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:parent/my:version" -v ${version} "${file}" 2>/dev/null ; then
    i=$((i+1))
  fi
  if xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:strolch.version/my:version" -v ${version} "${file}" 2>/dev/null ; then
    i=$((i+1))
  fi
  if xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:strolch.version" -v ${version} "${file}" 2>/dev/null ; then
    i=$((i+1))
  fi
  if xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:eitchnet.xmlpers.version" -v ${version} "${file}" 2>/dev/null ; then
    i=$((i+1))
  fi
  if xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:eitchnet.utils.version" -v ${version} "${file}" 2>/dev/null ; then
    i=$((i+1))
  fi
  if xmlstarlet ed --ps -L -N my=http://maven.apache.org/POM/4.0.0 -u "/my:project/my:properties/my:eitchnet.privilege.version" -v ${version} "${file}" 2>/dev/null ; then
    i=$((i+1))
  fi

  if [ $i -eq 0 ] ; then
    echo -e "ERROR: Not one version was change for file ${file}"
    exit 1
  fi

  return 0
}

cd "${root}"

setVersion "${new_version}" "${root}/pom.xml"
for project in li.* ; do
  setVersion "${new_version}" "${root}/${project}/pom.xml"
done
for project in ch.* ; do
  setVersion "${new_version}" "${root}/${project}/pom.xml"
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
  if [ -n "${push}" ] ; then
    echo "INFO: Pushing submodules to origin..."
    git submodule foreach git push origin
  fi
  git add .
  git commit -m "[Project] Bumped version from ${old_version} to ${new_version}"
  if [ -n "${push}" ] ; then
    echo "INFO: Pushing to origin..."
    git push origin
  fi
else
  echo "INFO: NOT committing new version"
fi


echo -e "\nINFO: Bumped version from ${old_version} to ${new_version}"

exit 0
