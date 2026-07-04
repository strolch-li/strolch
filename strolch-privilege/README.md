# strolch-privilege

Role-Based Access Control (RBAC) Implementation for Strolch.

## Overview
The `strolch-privilege` module is a lightweight and extensible library for securing access to resources and actions within the Strolch framework. It provides a robust API for authentication, authorization, session management, and multi-factor authentication.

## Features
- **RBAC**: Flexible Role-Based Access Control with support for Users, Roles, and Groups.
- **Extensible Policies**: Custom logic for privilege validation via `PrivilegePolicy`.
- **Multi-Factor Authentication**: Support for challenge-response mechanisms (e.g., email-based MFA).
- **Password Strength Validation**: Configurable rules for password complexity.
- **Personal Access Tokens (PATs)**: Secure programmatic access with restricted scopes.
- **SSO Integration**: Pluggable Single Sign-On handlers.
- **LDAP Support**: Authenticate users and map groups from an LDAP directory.
- **Persistence**: File-based (XML) or custom persistence for security data.
- **Session Persistence**: Optional persistence of active user sessions across application restarts.

## Documentation
- [Technical Specification](docs/technical-spec.md) - Detailed architecture and configuration guide.
- [Personal Access Tokens](docs/PersonalAccessToken.md) - Guide for using and managing PATs.

## Setup

### 1. Strolch Configuration
Configure the `PrivilegeHandler` and its sub-components in your `StrolchConfiguration.xml`:

```xml
<Component>
    <name>PrivilegeHandler</name>
    <api>li.strolch.privilege.handler.PrivilegeHandler</api>
    <impl>li.strolch.privilege.handler.DefaultPrivilegeHandler</impl>
    <Properties>
        <persistSessions>true</persistSessions>
        <persistSessionsPath>data/sessions.dat</persistSessionsPath>
    </Properties>
</Component>
```

### 2. Encryption and Persistence
You must also configure handlers for encryption and persistence:

```xml
<Component>
    <name>EncryptionHandler</name>
    <api>li.strolch.privilege.handler.EncryptionHandler</api>
    <impl>li.strolch.privilege.handler.DefaultEncryptionHandler</impl>
</Component>

<Component>
    <name>PersistenceHandler</name>
    <api>li.strolch.privilege.handler.PersistenceHandler</api>
    <impl>li.strolch.privilege.handler.XmlPersistenceHandler</impl>
    <Properties>
        <basePath>config</basePath>
    </Properties>
</Component>
```

## Development and Testing

To run the tests for this module:
```bash
mvn test
```

## References
- [Strolch Framework](https://strolch.li)



