#!/bin/bash
#######################################################################
##
## Update all projects by pulling from upstream
##
#######################################################################

echo "Updating li.strolch.dev..." ; git pull ; cd .. ; echo
echo "Updating li.strolch.parent..." ; cd li.strolch.parent ; git pull ; cd .. ; echo
echo "Updating li.strolch.bom..." ; cd li.strolch.bom ; git pull ; cd .. ; echo
echo "Updating li.strolch.model..." ; cd li.strolch.model ; git pull ; cd .. ; echo
echo "Updating li.strolch.testbase..." ; cd li.strolch.testbase ; git pull ; cd .. ; echo
echo "Updating li.strolch.runtime..." ; cd li.strolch.runtime ; git pull ; cd .. ; echo
echo "Updating li.strolch.service..." ; cd li.strolch.service ; git pull ; cd .. ; echo
echo "Updating li.strolch.persistence.api..." ; cd li.strolch.persistence.api ; git pull ; cd .. ; echo
echo "Updating li.strolch.persistence.xml..." ; cd li.strolch.persistence.xml ; git pull ; cd .. ; echo
echo "Updating li.strolch.persistence.postgresql..." ; cd li.strolch.persistence.postgresql ; git pull ; cd .. ; echo
echo "Updating li.strolch.tutorialapp..." ; cd li.strolch.tutorialapp ; git pull ; cd .. ; echo

echo "Done."
exit 0