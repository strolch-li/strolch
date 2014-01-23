ch.eitchnet.utils
======================

[![Build Status](http://jenkins.eitchnet.ch/buildStatus/icon?job=ch.eitchnet.utils)](http://jenkins.eitchnet.ch/view/ch.eitchnet/job/ch.eitchnet.utils/)

Java Utilites which ease daily work when programming in the Java language

Dependencies
----------------------
This utility package is built by Maven3 and has very few external dependencies. The current dependencies are:
* the Java Runtime Environment 6
* JUnit 4.10 (only during tests)
* slf4j 1.7.2
* slf4j-log4j bindings (only during tests)

Features
----------------------
* RMI File client/server
  * This is a small RMI client server which allows to fetch files from a server which exposes the RmiFileHandler class via RMI
* ObjectFilter
  * The ObjectFilter allows to keep track of modifications to objects. The modifications are add/update/remove.
  * You register the modification of an object on the filter and when all is done, you query the filter for all the add/update/remove modifications so that you only persist the required changes to your database
* ArraysHelper
  * The ArraysHelper contains methods to handling arrays
* BaseEncoding
  * The BaseEncoding class implements RFC4648 and thus implements Base64, Base32, Base16 in all their different alphabets and also implementes the D-Base32 encoding
* ByteHelper
  * The ByteHelper contains methods to print, convert and manipulate bytes 
* FileHelper
  * The FileHelper contains methods relevant to files. E.g. recursively deleting directories, copying files, reading/writing files etc.
* ProcessHelper
  * The ProcessHelper abstracts away OS specific process tasks
* StringHelper
  * The StringHelper contains methods for handling Strings
* SystemHelper
  * The SystemHelper contains methods to get system specific information
* XmlHelper
  * The XmlHelper contains methods to handle XML files

Building
-------------------------
* Prerequisites:
  * JDK 6 is installed and JAVA_HOME is properly set and ../bin is in path
  * Maven 3 is installed and MAVEN_HOME is properly set and ../bin is in path
* Clone repository and change path to root
* Run maven:
  * mvn clean install
