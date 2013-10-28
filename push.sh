#!/bin/bash
#######################################################################
##
## Distribute all projects by pushing to upstream
##
#######################################################################

echo "Pushing li.strolch.dev..." ; git push ; cd .. ; echo
echo "Pushing li.strolch.parent..." ; cd li.strolch.parent ; git push ; cd .. ; echo
echo "Pushing li.strolch.bom..." ; cd li.strolch.bom ; git push ; cd .. ; echo
echo "Pushing li.strolch.model..." ; cd li.strolch.model ; git push ; cd .. ; echo
echo "Pushing li.strolch.testbase..." ; cd li.strolch.testbase ; git push ; cd .. ; echo
echo "Pushing li.strolch.runtime..." ; cd li.strolch.runtime ; git push ; cd .. ; echo
echo "Pushing li.strolch.service..." ; cd li.strolch.service ; git push ; cd .. ; echo
echo "Pushing li.strolch.persistence.api..." ; cd li.strolch.persistence.api ; git push ; cd .. ; echo
echo "Pushing li.strolch.persistence.xml..." ; cd li.strolch.persistence.xml ; git push ; cd .. ; echo

echo "Done."
exit 0