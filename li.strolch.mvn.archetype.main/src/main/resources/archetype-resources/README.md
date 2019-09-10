Strolch Project: ${appName} / ${artifactId} 
======================================

This is a Strolch project which is started by a main()-method. It has an example
test class to show how to start an agent for tests.

The project's runtime directory is in the root folder and contains the 
configuration and data files needed to start via the main()-method.

The test has it's own runtime directory, with its own configuration, but the 
model file in the data directory points to the main runtime directory and uses 
that model file, so that one does not need to duplicate resource files.

Preparation
------------------

Please change the SCM connection in the pom.xml:

    <scm>
        <!-- TODO: Change this to your SCM URL -->
        <connection>scm:git:https://github.com/4treesCH/strolch.git</connection>
        <developerConnection>scm:git:https://github.com/4treesCH/strolch.git</developerConnection>
        <url>https://github.com/4treesCH/strolch</url>
    </scm>

Running tests
-------------------------

    mvn clean test


Running App
--------------------------

    mvn clean compile
    mvn exec:java

