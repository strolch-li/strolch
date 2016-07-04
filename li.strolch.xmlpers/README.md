li.strolch.java.xmlpers
========================

[![Build Status](http://jenkins.eitchnet.ch/buildStatus/icon?job=li.strolch.xmlpers)](http://jenkins.eitchnet.ch/view/li.strolch/job/li.strolch.xmlpers/)

Generic Java XML persistence layer. Implemented to be light-weight and simple to use

Dependencies
------------------------
XmlPers is built by Maven3 and has very few external dependencies. The current dependencies are:
* the Java Runtime Environment 6
* li.strolch.utils
* slf4j 1.7.2
* slf4j-log4j bindings (only during tests)
* JUnit 4.10 (only during tests)

Features
------------------------
The idea behind XmlPers is to have a very lightweight database where each object is saved in its own XML file.

The model for XmlPers is that for each persistable class the following information is available:
* Type (e.g. the class name)
* Optional Sub Type (e.g. some type in your class)
* Id

This is not forced on the model itself, but in the DAO. Persisting changes is done by delegating to XmlFilePersister and the DAO must convert the object to a XML Document.

See the tests for a reference implementation.

Building
------------------------
*Prerequisites:
  * JDK 6 is installed and JAVA_HOME is properly set and ../bin is in path
  * Maven 3 is installed and MAVEN_HOME is properly set and ../bin is in path
  * li.strolch.utils is installed in your local Maven Repository
* Clone repository and change path to root
* Run maven:
  * mvn clean install
