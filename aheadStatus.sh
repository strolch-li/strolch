#!/bin/bash
#######################################################################
##
## Shows how far ahead each project is against origin
##
#######################################################################

function logAheadCount() {
  aheadCount="$(git rev-list origin..master --count)"
  if [ "${aheadCount}" -ne 0 ] ; then
    project="${PWD}"
  	project="${project##*/}"
    echo "${project} is ahead of origin by ${aheadCount} commits"
  fi
}

echo "Checking how far ahead each project is against origin..."
cd ..
cd "li.strolch.dev" ; logAheadCount ; cd ..
cd "li.strolch.parent" ; logAheadCount ; cd ..
cd "li.strolch.bom" ; logAheadCount ; cd ..
cd "li.strolch.model" ; logAheadCount ; cd ..
cd "li.strolch.testbase" ; logAheadCount ; cd ..
cd "li.strolch.runtime" ; logAheadCount ; cd ..
cd "li.strolch.service" ; logAheadCount ; cd ..
cd "li.strolch.persistence.api" ; logAheadCount ; cd ..
cd "li.strolch.persistence.xml" ; logAheadCount ; cd ..
cd "li.strolch.persistence.postgresql" ; logAheadCount ; cd ..
cd "li.strolch.tutorialapp" ; logAheadCount ; cd ..

echo "Done."
exit 0