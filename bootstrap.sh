#!/bin/bash
#######################################################################
##
## Bootstrap developing of strolch projects by cloning all the 
## projects.
##
#######################################################################

echo "Cloning all strolch projects..."
if ! git clone git@github.com:eitch/li.strolch.model.git ; then
	exit 1
fi
echo
if ! git clone git@github.com:eitch/li.strolch.testbase.git ; then
	exit 1
fi
echo
if ! git clone git@github.com:eitch/li.strolch.runtime.git ; then
	exit 1
fi
echo
if ! git clone git@github.com:eitch/li.strolch.service.git ; then
	exit 1
fi
echo
if ! git clone git@github.com:eitch/li.strolch.bom.git ; then
	exit 1
fi
echo
if ! git clone git@github.com:eitch/li.strolch.persistence.api.git ; then
	exit 1
fi
echo
if ! git clone git@github.com:eitch/li.strolch.persistence.xml.git ; then
	exit 1
fi
echo

echo "Done."
exit 0