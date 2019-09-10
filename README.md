Strolch
==================
[![Build Status](https://ci.4trees.ch/buildStatus/icon?job=li.strolch)](https://ci.4trees.ch/job/li.strolch/)

The main repository which contains all of Strolch.

Strolch consists of the following modules:
- li.strolch.utils
- li.strolch.privilege
- li.strolch.agent
- li.strolch.model
- li.strolch.persistence.postgresql
- li.strolch.persistence.xml
- li.strolch.rest
- li.strolch.service
- li.strolch.testbase

And of course the website itself:
- li.strolch.website

Getting Started
----------------
Either use a version on Maven Central: https://mvnrepository.com/artifact/li.strolch/li.strolch.agent

Or install locally first:

    git clone https://github.com/4treesCH/strolch.git
    cd strolch
    mvn clean install -DskipTests

Then you can create your own project. Please read the README files in the generated projects.

Java App:

    mvn archetype:generate                                  \
      -DarchetypeGroupId=li.strolch                         \
      -DarchetypeArtifactId=li.strolch.mvn.archetype.main   \
      -DarchetypeVersion=1.6.0-SNAPSHOT                     \
      -DgroupId=<my.groupid>                                \
      -DartifactId=<my-artifactId>                          \
      -Dversion=<my.version>                                \
      -DappName="<my app name>"


    mvn archetype:generate                                  \
      -DarchetypeGroupId=li.strolch                         \
      -DarchetypeArtifactId=li.strolch.mvn.archetype.main   \
      -DarchetypeVersion=1.6.0-SNAPSHOT                     \
      -DgroupId=com.example                                 \
      -DartifactId=strolch-test-app                         \
      -Dversion=0.1.0-SNAPSHOT                              \
      -DappName="My Test App"

WebApp:

    mvn archetype:generate                                  \
      -DarchetypeGroupId=li.strolch                         \
      -DarchetypeArtifactId=li.strolch.mvn.archetype.webapp \
      -DarchetypeVersion=1.6.0-SNAPSHOT                     \
      -DgroupId=<my.groupid>                                \
      -DartifactId=<my-artifactId>                          \
      -Dversion=<my.version>                                \
      -DappName="<my app name>"


    mvn archetype:generate                                  \
      -DarchetypeGroupId=li.strolch                         \
      -DarchetypeArtifactId=li.strolch.mvn.archetype.webapp \
      -DarchetypeVersion=1.6.0-SNAPSHOT                     \
      -DgroupId=com.example                                 \
      -DartifactId=strolch-test-webapp                      \
      -Dversion=0.1.0-SNAPSHOT                              \
      -DappName="My Test Web App"


More Information
-----------------

Find more to Strolch at our website: http://strolch.li
