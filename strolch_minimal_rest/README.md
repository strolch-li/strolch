Strolch Minimal Rest
======================

A minimal Strolch project for Restful Applications which builds using Maven and the end result starts a
servlet based web application which runs in e.g. Apache Tomcat 8.0

Build
-------------------
On the console run:
$ mvn clean package -Dstrolch.env=dev
This creates a WAR in target/strolch_minimal.war

Run
---------------------
Copy the WAR to your webapps/ folder in Tomcat and start Tomcat.
If everything goes find, then you should find a line as follows:
    ComponentContainerImpl start - strolch_minimal_rest:dev All 4 Strolch Components started. Strolch is now ready to be used. Have fun =))

Eclipse
--------------------
One should only need to add the web application to a server configuration.
