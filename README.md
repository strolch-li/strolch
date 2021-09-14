# Strolch
[![Build Status](https://ci.4trees.ch/buildStatus/icon?job=strolch)](https://ci.4trees.ch/job/strolch/)

The main repository which contains all of Strolch.

Strolch consists of the following modules:
* li.strolch.utils
* li.strolch.privilege
* li.strolch.xmlpers
* li.strolch.model
* li.strolch.agent
* li.strolch.service
* li.strolch.persistence.postgresql
* li.strolch.persistence.xml
* li.strolch.rest
* li.strolch.websocket
* li.strolch.soql
* li.strolch.mvn.archetype.main
* li.strolch.mvn.archetype.webapp
* li.strolch.bom
* li.strolch.testbase
* li.strolch.performancetest

And of course the website itself:
* li.strolch.website

## Getting Started
Either use a version on Maven Central: https://mvnrepository.com/artifact/li.strolch/li.strolch.agent

Or install locally first:

    git clone https://github.com/4treesCH/strolch.git
    cd strolch
    mvn clean install -DskipTests

Then you can create your own project. Please read the README files in the generated projects.

To create a Strolch project see https://strolch.li/development/

## More Information

Find more to Strolch at our website: https://strolch.li
