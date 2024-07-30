li.strolch.privilege
====================

Overview
=======================================================================

Privilege is a light weight library to secure access or grant privileges to 
users in an application. Privilege allows a developer to secure the application 
in different levels of the application providing API's for different
contexts. 

Privilege is implemented in the Java language and is light weight in that it has
no external dependencies other than a Java runtime environment version 6. Since
the JRE 6 has an LDAP implementation it is possible to store Privilege data in
a LDAP repository with only the Privilege JAR.

Privilege is distributed under the GNU Lesser General Public License on 
Github.com and can be downloaded at 

   https://github.com/4treesCH/strolch/li.strolch.privilege

The main developer is Robert von Burg <eitch@eitchnet.ch> who also maintains the
Github repository. He is available for all questions regarding Privilege

Motivation
=======================================================================
In some cases a developer might want to restrict access to an application 
depending on the role which an authenticated user has. In other cases the 
developer would need a more finely grained control by restricting access to a 
certain object, or a certain method call.

We were looking for an API which would allows us to restrict access to a given 
object in different ways. For instance it was our intention to not simply 
restrict access to a specific object type, or instance, but to restrict access
to an instance of the object if it had fields set to a specific value.

Evaluations on existing libraries which implement access restriction did not 
provide an API which suited our needs or which were not easily implemented, thus
leading to the design of Privilege.

Design Goals
=======================================================================
When a developer needs to implement access restriction an application there are
different questions which the developer will ask:
- Does the user have a specific role?
- Does the user have a specific privilege i.e. is the user allowed to perform a 
specific action?
- Is a user allowed to access a specific type of object?
- Is a user allowed to access a specific instance of a type?
- Is a user allowed to access a field on a specific object?

Privilege's design goals are to allow the developer to answer these questions 
with an API which does not mean implementing a lot of additional project 
specific code.

Further in Privilege it should be possible to perform the normal CRUD functions:
- Create users, roles, privileges, etc.
- Read existing users, roles, privileges, etc.
- Update users, roles, privileges, etc.
- Delete users, roles, privileges, etc.

It should be possible to store Privilege's data in different databases, 
depending on the application. For example it should be able to store the data in
XML files, in a LDAP directory and so forth.

Documentation
=======================================================================

The current documentation, though a bit outdated, can be found in the docs/
directory of the Repository

Compiling
=======================================================================

Privilege is a Maven3 project and can be built by simply performing the
following command:

$ mvn package

Using
=======================================================================

To use Privilege see the li.strolch.privilege.test.PrivilegeTest.java class
which contains a few test cases including showing how to load Privilege.

This documentation is still in need of more work, but for any questions please
don't hesitate to write an e-mail to the developer and we'll find a solution.

	Switzerland, the 29. July 2012
	Robert von Burg



