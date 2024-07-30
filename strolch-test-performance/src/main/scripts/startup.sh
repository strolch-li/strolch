#!/bin/bash
java -classpath lib/ -jar "${artifactId}-${project.version}.jar" --root-path=./ --env=dev
