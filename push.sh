#!/bin/bash
#######################################################################
##
## Distribute all projects by pushing to upstream
##
#######################################################################

echo "Updating li.strolch.dev..." ; git push
echo "Updating li.strolch.parent..." ; cd li.strolch.parent ; git push ; cd ..
echo "Updating li.strolch.bom..." ; cd li.strolch.bom ; git push ; cd ..
echo "Updating li.strolch.model..." ; cd li.strolch.model ; git push ; cd ..
echo "Updating li.strolch.testbase..." ; cd li.strolch.testbase ; git push ; cd ..
echo "Updating li.strolch.runtime..." ; cd li.strolch.runtime ; git push ; cd ..
echo "Updating li.strolch.service..." ; cd li.strolch.service ; git push ; cd ..
echo "Updating li.strolch.persistence.api..." ; cd li.strolch.persistence.api ; git push ; cd ..
echo "Updating li.strolch.persistence.xml..." ; cd li.strolch.persistence.xml ; git push ; cd ..

echo "Done."
exit 0