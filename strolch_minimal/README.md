Strolch Minimal
======================

A minimal Strolch project which builds using Maven and the end result starts a console based Java Application.

Build
-------------------
On the console run:
$ mvn clean package
This creates a tarball in target/strolch_minimal-${project.version}-bin.tar.gz

Run
---------------------
Extract the tarball:
$ cd target
$ tar -xvzf strolch_minimal-1.0.0-SNAPSHOT-bin.tar.gz
$ cd strolch_minimal-1.0.0-SNAPSHOT
$ chmod u+x ./startup.sh
$ ./startup.sh

Eclipse
--------------------
In Eclipse create a runtime configuration with the following:
- Name: Strolch Minimal
- Main Class: li.strolch.minimal.main.Main
- Program Arguments:
  --env dev
  --root-path ${project_loc}/src/runtime

Starting the application should now work.