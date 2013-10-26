#!/bin/bash
#######################################################################
##
## Bootstrap developing of strolch projects by cloning all the 
## projects.
##
#######################################################################

echo "Updating li.strolch.model..." ; cd li.strolch.model ; git pull ; cd ..
echo "Updating li.strolch.testbase..." ; cd li.strolch.testbase ; git pull ; cd ..
echo "Updating li.strolch.runtime..." ; cd li.strolch.runtime ; git pull ; cd ..
echo "Updating li.strolch.service..." ; cd li.strolch.service ; git pull ; cd ..
echo "Updating li.strolch.bom..." ; cd li.strolch.bom ; git pull ; cd ..
echo "Updating li.strolch.persistence.api..." ; cd li.strolch.persistence.api ; git pull ; cd ..
echo "Updating li.strolch.persistence.xml..." ; cd li.strolch.persistence.xml ; git pull ; cd ..

echo "Done."
exit 0