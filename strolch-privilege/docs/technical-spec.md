# Privilege Technical Specification

## Overview
The `strolch-privilege` module is a lightweight, extensible Role-Based Access Control (RBAC) library for the Strolch framework. It provides mechanisms for user authentication, authorization, and multi-factor authentication (MFA).

## Architecture

### PrivilegeHandler
The central component is `PrivilegeHandler`, which manages the lifecycle of sessions (Certificates), user authentication, and authorization checks. The default implementation is `DefaultPrivilegeHandler`.

### Certificates and Contexts
- **Certificate**: A session token representing an authenticated user. It contains the username, login time, and a unique session ID.
- **PrivilegeContext**: A wrapper around a Certificate that provides methods for validating actions against the user's privileges.

### Restrictable
Any action or object that needs to be secured must implement the `Restrictable` interface. It provides a privilege name and a privilege value, which are used by policies to determine access.

### PrivilegePolicy
A `PrivilegePolicy` defines the logic for whether a user has a specific privilege. Policies are extensible and can be configured per privilege in a role.

## Data Model

The data model consists of Users, Roles, and Groups.

### User
A user represents an identity in the system.
- **User ID**: Unique internal identifier.
- **Username**: Unique login identifier.
- **Firstname/Lastname**: User's name.
- **State**: `NEW`, `ENABLED`, `REMOTE`, `DISABLED`, `EXPIRED`, `SYSTEM`.
- **Roles**: A set of roles assigned to the user.
- **Groups**: A set of groups the user belongs to.
- **Locale**: The user's preferred language/region.
- **History**: Tracks login times and password changes.
- **Properties**: Arbitrary key-value pairs for additional user data (e.g., email, organisation, realm).
- **Password Change Requested**: Boolean flag indicating if a password change is required.

### Role
A role is a collection of privileges.
- **Name**: Unique identifier.
- **Privileges**: A map of `Privilege` objects.

### Privilege
A privilege defines access to a specific resource or action.
- **Name**: The name of the privilege (e.g., `Service`, `Resource`).
- **Policy**: The name of the `PrivilegePolicy` implementation to use.
- **AllAllowed**: Boolean flag to grant full access.
- **AllowList**: A set of values specifically allowed.
- **DenyList**: A set of values specifically denied.

### Group
A group is a collection of roles and properties that can be associated with users. This allows for defining common roles and properties once and assigning them to multiple users, reducing duplication.
- **Name**: Unique identifier.
- **Roles**: A set of role names assigned to the group.
- **Properties**: Arbitrary key-value pairs for additional group data. Commonly used properties include `organisation`, `location`, and `realm`.
- **Validation**: A group must have at least one role assigned.

## Built-in Policies

| Policy | Description |
| --- | --- |
| `DefaultPrivilege` | Performs a simple allow/deny check based on the `privilegeValue` (string). |
| `RoleAccessPrivilege` | Restricts access to roles based on the user's assigned roles. |
| `UserAccessPrivilege` | Restricts access to user data. |
| `UserAccessWithSameOrganisationPrivilege` | Restricts access to users within the same organisation. |
| `UsernameFromCertificatePrivilege` | Validates that the username in the request matches the certificate's username. |
| `UsernameFromCertificateWithSameOrganisationPrivilege` | Validates username from certificate and ensures both users are in the same organisation. |

## Persistence

The `PersistenceHandler` interface defines how the model is stored.

### XML Persistence
`XmlPersistenceHandler` stores the model in XML files.
- `PrivilegeUsers.xml`: User definitions and history.
- `PrivilegeRoles.xml`: Role and privilege definitions.
- `PrivilegeGroups.xml`: Group definitions.
- `PrivilegeTokens.xml`: Personal Access Tokens.

#### Properties

| Property | Description | Default |
| --- | --- | --- |
| `basePath` | Base path to the XML configuration files. | **Required** |
| `usersXmlFile` | Name of the users XML file. | `PrivilegeUsers.xml` |
| `groupsXmlFile` | Name of the groups XML file. | `PrivilegeGroups.xml` |
| `rolesXmlFile` | Name of the roles XML file. | `PrivilegeRoles.xml` |
| `tokensXmlFile` | Name of the tokens XML file. | `PrivilegeTokens.xml` |
| `caseInsensitiveUsername` | Treats usernames as case-insensitive. | `true` |

## Configuration

The `PrivilegeHandler` is configured via a `PrivilegeConfig.xml` file or within the `StrolchConfiguration.xml`.

### PrivilegeHandler Properties

| Property | Description | Default |
| --- | --- | --- |
| `verbose` | Enables verbose logging. | `false` |
| `autoPersistOnUserChangesData` | Automatically persists the model when a user changes their data. | `false` |
| `persistSessions` | Enables persistence of active sessions across restarts. | `false` |
| `persistSessionsPath` | Path to the file where sessions are persisted. | **Required if `persistSessions` is true** |
| `allowSessionRefresh` | Allows refreshing an expired session. | `false` |
| `allowPasswordReset` | Allows password resets via `UserChallengeHandler`. | `false` |
| `disallowSourceChange` | Prevents a session from being used from a different IP/source. | `false` |
| `secretKey` | Secret key used for encryption (AES). | **Required** |
| `secretSalt` | Secret salt used for encryption (AES). | **Required** |
| `privilegeConflictResolution` | Resolution strategy when multiple roles define the same privilege (`STRICT`, `MERGE`). | `MERGE` |

### EncryptionHandler Properties
Configure the `DefaultEncryptionHandler` for password hashing:

| Property | Description | Default |
| --- | --- | --- |
| `hashAlgorithm` | The PBKDF2 algorithm to use for salted hashes. | `PBKDF2WithHmacSHA512` |
| `hashAlgorithmNonSalt` | Hashing algorithm for legacy/non-salted hashes. | `SHA-256` |
| `hashIterations` | Number of iterations for PBKDF2. | `200000` |
| `hashKeyLength` | Key length for the generated hash. | `256` |

## Authentication Handlers

### UserChallengeHandler
Handles challenge-response mechanisms, such as sending a code via email for password resets or MFA.
- `ConsoleUserChallengeHandler`: Prints the challenge to the console (for development).
- `MailUserChallengeHandler`: Sends the challenge via email.

### SingleSignOnHandler
Integrates with external SSO providers.

## Password Strength Validation

The `PasswordStrengthHandler` interface allows for validating the strength of a user's password when it is set or changed.

### BasicPasswordStrengthHandler
A configurable handler that validates password length and character types.

| Property | Description | Default |
| --- | --- | --- |
| `minLength` | Minimum required length (must be at least 8). | `8` |
| `maxLength` | Maximum allowed length (up to 1024). | `1024` |
| `needsNumbers` | Requires at least one digit. | `true` |
| `needsLowerCase` | Requires at least one lowercase character. | `true` |
| `needsUpperCase` | Requires at least one uppercase character. | `true` |
| `needsSpecialChars` | Requires at least one special character. | `false` |

### SimplePasswordStrengthHandler
A minimal handler that only requires the password to be at least 3 characters long. No additional configuration is supported.

## LDAP Support
`LdapPrivilegeHandler` allows authenticating users against an LDAP directory. It can map LDAP groups to Strolch roles.

## Personal Access Tokens
Personal Access Tokens (PATs) allow for programmatic access with restricted scopes.
See [Personal Access Tokens](PersonalAccessToken.md) for details.

## Example XML Files

### PrivilegeConfig.xml
The main configuration file for the `PrivilegeHandler`.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Privilege>

    <Container>

        <Parameters>
            <!-- parameters for the container itself -->
            <Parameter name="secretKey" value="secret-key"/>
            <Parameter name="secretSalt" value="secret-key"/>
            <Parameter name="persistSessions" value="true"/>
            <Parameter name="persistSessionsPath" value="target/${target}/sessions.dat"/>
            <Parameter name="autoPersistOnUserChangesData" value="true"/>
            <Parameter name="privilegeConflictResolution" value="STRICT"/>
            <Parameter name="allowPasswordReset" value="true"/>
        </Parameters>

        <EncryptionHandler class="li.strolch.privilege.handler.DefaultEncryptionHandler">
            <Parameters>
                <!-- WARNING: If you change iterations or keyLength, then all passwords are invalid -->
                <!-- default algorithm is: PBKDF2WithHmacSHA512 -->
                <Parameter name="hashAlgorithm" value="PBKDF2WithHmacSHA512"/>
                <!-- default iterations: 200000 -->
                <Parameter name="hashIterations" value="10000"/>
                <!-- default key length: 256 -->
                <Parameter name="hashKeyLength" value="256"/>
            </Parameters>
        </EncryptionHandler>

        <PersistenceHandler class="li.strolch.privilege.handler.XmlPersistenceHandler">
            <Parameters>
                <Parameter name="basePath" value="target/${target}"/>
                <Parameter name="usersXmlFile" value="PrivilegeUsers.xml"/>
                <Parameter name="rolesXmlFile" value="PrivilegeRoles.xml"/>
                <Parameter name="tokensXmlFile" value="PrivilegeTokens.xml"/>
            </Parameters>
        </PersistenceHandler>

        <UserChallengeHandler class="li.strolch.privilege.test.model.TestUserChallengeHandler">
        </UserChallengeHandler>

        <SsoHandler class="li.strolch.privilege.test.model.DummySsoHandler"/>

    </Container>

    <Policies>
        <Policy name="DefaultPrivilege" class="li.strolch.privilege.policy.DefaultPrivilege" />
        <Policy name="RoleAccessPrivilege" class="li.strolch.privilege.policy.RoleAccessPrivilege" />
        <Policy name="UserAccessPrivilege" class="li.strolch.privilege.policy.UserAccessPrivilege" />
        <Policy name="UserSessionAccessPrivilege" class="li.strolch.privilege.policy.UsernameFromCertificatePrivilege"/>
    </Policies>

</Privilege>
```

### PrivilegeUsers.xml
Defines the users in the system.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Users>

    <User userId="1" username="admin" password="$PBKDF2WithHmacSHA512,10000,256$61646d696e$cb69962946617da006a2f95776d78b49e5ec7941d2bdb2d25cdb05f957f64344">
        <Firstname>Application</Firstname>
        <Lastname>Administrator</Lastname>
        <State>ENABLED</State>
        <Locale>en-GB</Locale>
        <Groups>
            <Group>GroupA</Group>
        </Groups>
        <Roles>
            <Role>PrivilegeAdmin</Role>
            <Role>AppUser</Role>
        </Roles>
        <Properties>
            <Property name="organization" value="eitchnet.ch"/>
            <Property name="organizationalUnit" value="Development"/>
        </Properties>
    </User>

    <User userId="2" username="admin2" password="$PBKDF2WithHmacSHA512,10000,256$61646d696e32$c2f8c30e18c52ab374b4aa2040c0ea1837a6cfbfa0377e673ca735ce8e8e6f75">
        <Firstname>Application</Firstname>
        <Lastname>Administrator</Lastname>
        <State>ENABLED</State>
        <Locale>en-GB</Locale>
        <Groups>
            <Group>AppUserLocationA</Group>
        </Groups>
        <Roles>
            <Role>PrivilegeAdmin</Role>
        </Roles>
        <Properties>
            <Property name="organization" value="eitchnet.ch"/>
            <Property name="organizationalUnit" value="Development"/>
        </Properties>
    </User>

    <User userId="3" username="system_admin">
        <Firstname>System User</Firstname>
        <Lastname>Administrator</Lastname>
        <State>SYSTEM</State>
        <Locale>en-GB</Locale>
        <Roles>
            <Role>system_admin_privileges</Role>
        </Roles>
    </User>

    <User userId="4" username="system_admin2">
        <Firstname>System User</Firstname>
        <Lastname>Administrator</Lastname>
        <State>SYSTEM</State>
        <Locale>en-GB</Locale>
        <Roles>
            <Role>system_admin_privileges</Role>
        </Roles>
    </User>

    <User userId="5" username="jill" password="8c6976e5b5410415bde908bd4dee15dfb167a9c873fc4bb8a81f6f2ab448a918">
        <Firstname>Jill</Firstname>
        <Lastname>Smith</Lastname>
        <State>ENABLED</State>
        <Locale>en-GB</Locale>
        <Roles>
            <Role>AppUser</Role>
        </Roles>
    </User>

</Users>
```

### PrivilegeRoles.xml
Defines the roles and their associated privileges.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Roles>

    <Role name="PrivilegeAdmin">
        <Privilege name="GetSession" policy="UserSessionAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="InvalidateSession" policy="UserSessionAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeAction" policy="DefaultPrivilege">
            <Allow>GetCertificates</Allow>
            <Allow>GetPolicies</Allow>
            <Allow>Persist</Allow>
            <Allow>PersistSessions</Allow>
            <Allow>Reload</Allow>
        </Privilege>
        <Privilege name="PrivilegeAddRole" policy="RoleAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeAddUser" policy="UserAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeGetRole" policy="RoleAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeGetGroup" policy="GroupAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeAddGroup" policy="GroupAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeModifyGroup" policy="GroupAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeRemoveGroup" policy="GroupAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeGetUser" policy="UserAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeGetUserPrivileges" policy="UserAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeGetGroupPrivileges" policy="UserAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeModifyRole" policy="RoleAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeModifyUser" policy="UserAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeRemoveRole" policy="RoleAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeRemoveUser" policy="UserAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeSetUserLocale" policy="UserAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeSetUserPassword" policy="UserAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegeSetUserState" policy="UserAccessPrivilege">
            <Deny>SYSTEM</Deny>
            <Allow>DISABLED</Allow>
            <Allow>ENABLED</Allow>
        </Privilege>
        <Privilege name="RequirePasswordChange" policy="UserAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="PrivilegePersonalAccessToken" policy="DefaultPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="li.strolch.privilege.model.internal.PersonalAccessToken" policy="UserAccessPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
        <Privilege name="li.strolch.service.api.Service" policy="DefaultPrivilege">
            <Allow>li.strolch.service.privilege.roles.PrivilegeAddRoleService</Allow>
            <Allow>li.strolch.service.privilege.roles.PrivilegeRemoveRoleService</Allow>
            <Allow>li.strolch.service.privilege.roles.PrivilegeUpdateRoleService</Allow>
            <Allow>li.strolch.service.privilege.users.PrivilegeAddUserService</Allow>
            <Allow>li.strolch.service.privilege.users.PrivilegeRemoveUserService</Allow>
            <Allow>li.strolch.service.privilege.users.PrivilegeSetUserLocaleService</Allow>
            <Allow>li.strolch.service.privilege.users.PrivilegeSetUserPasswordService</Allow>
            <Allow>li.strolch.service.privilege.users.PrivilegeSetUserStateService</Allow>
            <Allow>li.strolch.service.privilege.users.PrivilegeUpdateUserRolesService</Allow>
            <Allow>li.strolch.service.privilege.users.PrivilegeUpdateUserService</Allow>
        </Privilege>
    </Role>

    <Role name="AppUser">
        <Privilege name="li.strolch.privilege.test.model.TestRestrictable" policy="DefaultPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
    </Role>

    <Role name="MyRole">
        <Privilege name="Foo" policy="DefaultPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
    </Role>

    <Role name="MyRole2">
        <Privilege name="Foo" policy="DefaultPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
    </Role>

    <Role name="system_admin_privileges">
        <Privilege name="li.strolch.privilege.handler.SystemAction" policy="DefaultPrivilege">
            <Allow>li.strolch.privilege.test.model.TestSystemUserAction</Allow>
            <Deny>li.strolch.privilege.test.model.TestSystemUserActionDeny</Deny>
        </Privilege>
        <Privilege name="li.strolch.privilege.test.model.TestSystemRestrictable" policy="DefaultPrivilege">
            <AllAllowed>true</AllAllowed>
        </Privilege>
    </Role>

    <Role name="restrictedRole">
        <Privilege name="li.strolch.privilege.handler.SystemAction" policy="DefaultPrivilege">
            <Allow>hello</Allow>
            <Deny>goodbye</Deny>
        </Privilege>
    </Role>

</Roles>
```

### PrivilegeGroups.xml
Defines the groups and the roles associated with them.

```xml
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<Groups>
    <Group name="GroupA"/>
    <Group name="AppUserLocationA">
        <Roles>
            <Role>AppUser</Role>
            <Role>MyRole</Role>
        </Roles>
        <Properties>
            <Property name="location" value="LocationA"/>
        </Properties>
    </Group>
    <Group name="AppSuperUser">
        <Roles>
            <Role>AppUser</Role>
            <Role>MyRole</Role>
        </Roles>
    </Group>
</Groups>
```

### PrivilegeTokens.xml
Defines Personal Access Tokens (PATs).

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<Tokens>
    <Token username="admin" tokenId="50b31270-bc49-4940-97ec-d4aa0d1ad649" name="Test Token"
           token="$PBKDF2WithHmacSHA512,10000,256$61646d696e$cb69962946617da006a2f95776d78b49e5ec7941d2bdb2d25cdb05f957f64344"
           validFrom="2024-02-12T08:00:00.000+01:00" validTo="3000-01-01T01:00:00.000+01:00"
           lastUsed="2024-02-12T09:00:00.000+01:00">
        <Privilege name="Foo" policy="DefaultPrivilege">
            <Allow>allow1</Allow>
            <Deny>deny1</Deny>
        </Privilege>
        <Privilege name="Bar" policy="DefaultPrivilege">
            <Allow>allow2</Allow>
            <Deny>deny2</Deny>
        </Privilege>
    </Token>
</Tokens>
```
