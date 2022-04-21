Strolch Project: ${appName} / ${artifactId} 
======================================

This is a Strolch project which is started by a servlet container as this
project builds WARs.

The project's runtime directory is in the root folder and contains the 
configuration and data files needed to start via the `StartupListener`.

The test has it's own runtime directory, with its own configuration, but the 
model file in the data directory points to the main runtime directory and uses 
that model file, so that one does not need to duplicate resource files.

Preparation
------------------

Please change the SCM connection in the pom.xml:

    <scm>
        <!-- TODO: Change this to your SCM URL -->
        <connection>scm:git:https://github.com/strolch-li/strolch.git</connection>
        <developerConnection>scm:git:https://github.com/strolch-li/strolch.git</developerConnection>
        <url>https://github.com/strolch-li/strolch</url>
    </scm>

Before you are able to start the app, you need to update the path in the
`StrolchBootstrap.xml` file in web `src/main/webapp/WEB-INF`. There add a new
environment with the following format:

    <env id="dev.${username}" default="true">
   	    <root>${path.to.runtime.directory}</root>
   	    <environment>dev</environment>
    </env>

Now install web dependencies:

    cd src/main/webapp
    npm install
    gulp


Running tests
-------------------------

    mvn clean test


Building WAR
--------------------------
Without compressing the web files:

    mvn clean package

With compressing the web files for local runtime:

    mvn clean package -Prelease -Pdev.local

And if you want to prepare for deployment, which uses a different environment:

    mvn clean package -Prelease -Dstrolch.env=<env from StrolchBootstrap.xml>

