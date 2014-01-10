#!/bin/bash
#######################################################################
##
## Show status of all projects
##
#######################################################################

echo "Showing status of li.strolch.dev..." ; git status -s ; cd .. ; echo
echo "Showing status of li.strolch.parent..." ; cd li.strolch.parent ; git status -s ; cd .. ; echo
echo "Showing status of li.strolch.bom..." ; cd li.strolch.bom ; git status -s ; cd .. ; echo
echo "Showing status of li.strolch.model..." ; cd li.strolch.model ; git status -s ; cd .. ; echo
echo "Showing status of li.strolch.testbase..." ; cd li.strolch.testbase ; git status -s ; cd .. ; echo
echo "Showing status of li.strolch.runtime..." ; cd li.strolch.runtime ; git status -s ; cd .. ; echo
echo "Showing status of li.strolch.service..." ; cd li.strolch.service ; git status -s ; cd .. ; echo
echo "Showing status of li.strolch.persistence.xml..." ; cd li.strolch.persistence.xml ; git status -s ; cd .. ; echo
echo "Showing status of li.strolch.persistence.postgresql..." ; cd li.strolch.persistence.postgresql ; git status -s ; cd .. ; echo
echo "Showing status of li.strolch.tutorialapp..." ; cd li.strolch.tutorialapp ; git status -s ; cd .. ; echo
echo "Showing status of li.strolch.tutorialwebapp..." ; cd li.strolch.tutoriwebalapp ; git status -s ; cd .. ; echo

echo "Done."
exit 0
