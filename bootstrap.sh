#!/bin/bash
#######################################################################
##
## Bootstrap developing of strolch projects by cloning all the 
## projects.
##
#######################################################################

echo "Cloning all strolch projects..."

if [ -d ../li.strolch.parent ] ; then
  echo "Project li.strolch.parent already cloned."
else
  if ! git clone git@github.com:eitch/li.strolch.parent.git ../li.strolch.parent ;then
  	exit 1
  fi
  echo
fi

if [ -d ../li.strolch.bom ] ; then
  echo "Project li.strolch.bom already cloned."
else
  if ! git clone git@github.com:eitch/li.strolch.bom.git ../li.strolch.bom ;then
    exit 1
  fi
  echo
fi

if [ -d ../li.strolch.model ] ; then
  echo "Project li.strolch.model already cloned."
else
  if ! git clone git@github.com:eitch/li.strolch.model.git ../li.strolch.model ;then
    exit 1
  fi
  echo
fi

if [ -d ../li.strolch.testbase ] ; then
  echo "Project li.strolch.testbase already cloned."
else
  if ! git clone git@github.com:eitch/li.strolch.testbase.git ../li.strolch.testbase ;then
    exit 1
  fi
  echo
fi

if [ -d ../li.strolch.runtime ] ; then
  echo "Project li.strolch.runtime already cloned."
else
  if ! git clone git@github.com:eitch/li.strolch.runtime.git ../li.strolch.runtime ;then
    exit 1
  fi
  echo
fi

if [ -d ../li.strolch.service ] ; then
  echo "Project li.strolch.service already cloned."
else
  if ! git clone git@github.com:eitch/li.strolch.service.git ../li.strolch.service ;then
    exit 1
  fi
  echo
fi

if [ -d ../li.strolch.persistence.xml ] ; then
  echo "Project li.strolch.persistence.xml already cloned."
else
  if ! git clone git@github.com:eitch/li.strolch.persistence.xml.git ../li.strolch.persistence.xml ;then
    exit 1
  fi
  echo
fi

if [ -d ../li.strolch.persistence.postgresql ] ; then
  echo "Project li.strolch.persistence.postgresql already cloned."
else
  if ! git clone git@github.com:eitch/li.strolch.persistence.postgresql.git ../li.strolch.persistence.postgresql ;then
    exit 1
  fi
  echo
fi

if [ -d ../li.strolch.tutorialapp ] ; then
  echo "Project li.strolch.tutorialapp already cloned."
else
  if ! git clone git@github.com:eitch/li.strolch.tutorialapp.git ../li.strolch.tutorialapp ;then
    exit 1
  fi
  echo
fi

if [ -d ../li.strolch.tutorialwebapp ] ; then
  echo "Project li.strolch.tutorialwebapp already cloned."
else
  if ! git clone git@github.com:eitch/li.strolch.tutorialwebapp.git ../li.strolch.tutorialwebapp ;then
    exit 1
  fi
  echo
fi

echo "Done."
exit 0