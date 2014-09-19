#!/bin/bash

VERVEINE="/data/programs/verveinej/verveinej.sh"
DIST_STROLCH="/var/www/eitch/www.strolch.li/dist/snapshot"

## Prepare the environment
echo "INFO Bootstrapping..."
if ! ./bootstrap_https.sh ; then
  echo "ERROR: Failed to boostrap!"
  exit 1
fi

echo "INFO: Copying dependencies"
mvn -DskipTests=true clean dependency:copy-dependencies

echo "INFO Running verveinej..."
cd ..
if ! ${VERVEINE} -- -autocp . . ; then
  echo "ERROR: Failed to run verveinej!"
  exit 1
fi 
mv output.mse Strolch.mse

echo "INFO Cleaning sources..."
if ! mvn clean ; then
  echo "ERROR: Failed to run mvn clean!"
  exit 1
fi

echo "INFO Packaging..."
cd ../
if ! tar --exclude="*/target" --exclude="*/.git*" -cvzf strolch_mse.tar.gz * ; then
  echo "ERROR: Failed to build package!"
  exit 1
fi

if ! mv strolch_mse.tar.gz ${DIST_STROLCH}/strolch_mse.tar.gz ; then
  echo "ERROR: Failed to publish MSE file!"
  exit 1
fi

exit 0