# Per-Element XML Persistence Specification (`PerElementXmlPersistenceHandler`)

## Overview & Motivation

The default `XmlPersistenceHandler` stores all privilege data in monolithic XML files (`PrivilegeUsers.xml`, `PrivilegeRoles.xml`, `PrivilegeGroups.xml`, `PrivilegeTokens.xml`). While simple, this architecture suffers from significant performance and concurrency drawbacks in active environments:

1. **Global Write Lock on Authentication:** Every successful user login updates the user's `UserHistory` (`lastLogin`, `firstLogin`), marking the users collection dirty. Every Personal Access Token (PAT) authentication updates `lastUsed`. Under `XmlPersistenceHandler`, this triggers a complete rewrite of the entire `PrivilegeUsers.xml` or `PrivilegeTokens.xml` file under a global lock.
2. **Write Amplification:** Updating an 8-byte timestamp for a single user forces the serialization and disk I/O of all users in the system.
3. **Blast Radius & Fault Tolerance:** An I/O error or system crash during a monolithic rewrite risks corrupting the entire privilege database.
4. **VCS Noise & Merge Conflicts:** Storing static configuration (roles, users, credentials) and dynamic runtime statistics (login timestamps) in the same file results in continuous Git diff churn and merge conflicts when configuration is version-controlled.

`PerElementXmlPersistenceHandler` resolves these limitations by:
- Storing static authorization model entities (`User`, `Role`, `Group`, `PersonalAccessToken`) as **individual XML files**.
- Storing dynamic runtime state (`UserHistory`, token `lastUsed`) in separate **per-element `.properties` state files**.
- Eliminating global write locks on authentication, enabling concurrent logins and token verifications with zero write contention on static configuration.
- Ensuring atomic file operations via temporary files and atomic moves.
- Providing seamless auto-migration from legacy monolithic XML files.

---

## Architecture & Separation of Concerns

Privilege data is partitioned into two distinct categories:

| Dimension | Static Model / Configuration | Dynamic Runtime State |
| :--- | :--- | :--- |
| **Entities** | `User` (credentials, roles, groups, state, properties), `Role`, `Group`, `PersonalAccessToken` (name, privileges, validity) | `UserHistory` (`firstLogin`, `lastLogin`), `PersonalAccessToken` (`lastUsed`), Active Sessions (`Certificate`) |
| **Storage Format** | Individual XML files (`.xml`) | Individual Java Properties files (`.properties`) |
| **Storage Directory** | `<basePath>/model/` | `<basePath>/state/` |
| **Mutation Triggers** | Administrative actions (add/replace user, modify role, create PAT) | Authentication events (user login, PAT usage, session refresh/logout) |
| **Frequency** | Low (infrequent admin operations) | High (every login, token verification, or session access) |
| **Concurrency / Lock** | Per-element lock during modification | Fine-grained lock per `userId` / `tokenId` / `sessionId` |
| **Durability** | Immediate atomic file replacement | Immediate atomic file replacement |
| **Version Control** | Tracked in Git / configuration management | Excluded via `.gitignore` |

---

## Directory Layout & File Naming

```text
privilege/
├── model/                          <-- Static Configuration (Version Controlled)
│   ├── users/
│   │   ├── admin.xml
│   │   └── jdoe.xml                <-- Named by userId (sanitized)
│   ├── roles/
│   │   ├── PrivilegeAdmin.xml
│   │   └── AppUser.xml             <-- Named by roleName (sanitized)
│   ├── groups/
│   │   └── Management.xml          <-- Named by groupName (sanitized)
│   └── tokens/
│       └── 550e8400-e29b-41d4.xml  <-- Named by tokenId
└── state/                          <-- Volatile Runtime State (Git-ignored)
    ├── users/
    │   ├── admin.properties        <-- firstLogin, lastLogin
    │   └── jdoe.properties
    ├── tokens/
    │   └── 550e8400-e29b-41d4.properties <-- lastUsed
    └── sessions/
        └── b2742967-bce4-4ea0.properties <-- active session state
```

### File Naming Rules
- **Users:** Files in `model/users/` are named after the immutable `userId` (e.g. `FileHelper.toSafeFilename(user.getUserId()) + ".xml"`).
- **Roles:** Files in `model/roles/` are named after `role.getName()` (e.g. `FileHelper.toSafeFilename(role.getName()) + ".xml"`).
- **Groups:** Files in `model/groups/` are named after `group.name()` (e.g. `FileHelper.toSafeFilename(group.name()) + ".xml"`).
- **Tokens:** Files in `model/tokens/` are named after `token.tokenId()` (e.g. `FileHelper.toSafeFilename(token.tokenId()) + ".xml"`).
- **User/Token State Files:** State files in `state/users/` and `state/tokens/` mirror the corresponding entity ID with the `.properties` extension.
- **Sessions:** State files in `state/sessions/` are named after `certificate.getSessionId()` (e.g. `FileHelper.toSafeFilename(certificate.getSessionId()) + ".properties"`).

---

## Configuration Properties

The `PerElementXmlPersistenceHandler` is configured in `StrolchConfiguration.xml` or `PrivilegeConfig.xml` under the `PersistenceHandler` component:

```xml
<Component>
    <name>PersistenceHandler</name>
    <api>li.strolch.privilege.handler.PersistenceHandler</api>
    <impl>li.strolch.privilege.handler.PerElementXmlPersistenceHandler</impl>
    <Properties>
        <basePath>config/privilege</basePath>
        <modelDir>model</modelDir>
        <stateDir>state</stateDir>
        <caseInsensitiveUsername>true</caseInsensitiveUsername>
        <autoMigrateMonolithic>true</autoMigrateMonolithic>
        <verbose>false</verbose>
    </Properties>
</Component>
```

### Parameter Reference

| Parameter | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `basePath` | String | *(Required)* | Root directory containing the privilege configuration and state folders. |
| `modelDir` | String | `model` | Name of the subdirectory under `basePath` holding static XML configuration. |
| `stateDir` | String | `state` | Name of the subdirectory under `basePath` holding dynamic runtime state. |
| `caseInsensitiveUsername` | boolean | `true` | When `true`, username lookups and duplicate checks are case-insensitive. |
| `autoMigrateMonolithic` | boolean | `true` | When `true`, automatically imports legacy monolithic XML files if the `model/` directory is missing or empty. |
| `verbose` | boolean | `false` | Enables verbose logging during SAX parsing. |

---

## State File Format

State files use standard Java Properties format (`.properties`), storing ISO-8601 formatted timestamps with timezone information.

### User State (`state/users/<userId>.properties`)
```properties
# Strolch User Runtime State
firstLogin=2026-01-15T08:30:00.123+01:00[Europe/Zurich]
lastLogin=2026-09-29T09:27:45.789+01:00[Europe/Zurich]
```

### Token State (`state/tokens/<tokenId>.properties`)
```properties
# Strolch Token Runtime State
lastUsed=2026-09-29T09:27:45.789+01:00[Europe/Zurich]
```

### Session State (`state/sessions/<sessionId>.properties`)
```properties
# Strolch Session Runtime State
sessionId=b2742967-bce4-4ea0-9832-6a7f92020e3a
username=admin
usage=ANY
authToken=e93f8e58-64c8-4720-9cb5-ff8ef51ef942
source=127.0.0.1
locale=en-US
loginTime=2026-09-29T09:00:00.000+02:00[Europe/Zurich]
lastAccess=2026-09-29T09:27:45.789+02:00[Europe/Zurich]
keepAlive=false
```

### Security Considerations & Unencrypted Storage
Session state files are stored unencrypted in plain text (similar to token secrets and password hashes stored on disk). Storing symmetric keys in plaintext configuration files (`PrivilegeConfig.xml`) within the same directory offered no real defense-in-depth security barrier over standard operating system filesystem access permissions (`chmod 600` / `700`). Unencrypted storage enables direct inspectability, simplifies troubleshooting and monitoring of active sessions, and avoids cryptographic overhead on high-frequency session access.

---

## Lifecycle & Persistence Semantics

### 1. Initialization and Reload (`reload()`)
1. **Directory Preparation:** Ensure `model/users`, `model/roles`, `model/groups`, `model/tokens`, `state/users`, `state/tokens`, and `state/sessions` exist under `basePath`.
2. **Auto-Migration Check:** If `autoMigrateMonolithic` is `true` and the `model/` directory contains no entity XML files, check for legacy monolithic XML files (`PrivilegeUsers.xml`, `PrivilegeRoles.xml`, `PrivilegeGroups.xml`, `PrivilegeTokens.xml`, `PrivilegeSessions.xml`, or `sessions.dat`) in `basePath`. If found, parse them and write out individual model and state files.
3. **Model Parsing:**
   - Iterate and parse each `.xml` file in `model/roles/` via `PrivilegeRolesSaxReader`.
   - Iterate and parse each `.xml` file in `model/groups/` via `PrivilegeGroupsSaxReader`.
   - Iterate and parse each `.xml` file in `model/tokens/` via `PrivilegeTokensSaxReader`.
   - Iterate and parse each `.xml` file in `model/users/` via `PrivilegeUsersSaxReader`.
4. **State Hydration:**
   - For each user loaded from `model/users/`, check for `state/users/<userId>.properties`. If present, read `firstLogin` and `lastLogin` and attach the resulting `UserHistory` to the in-memory `User`.
   - For each token loaded from `model/tokens/`, check for `state/tokens/<tokenId>.properties`. If present, read `lastUsed` and update the in-memory `PersonalAccessToken`.
5. **Session Hydration (`getAllSessions()`):**
   - Read all `.properties` files in `state/sessions/`.
   - Construct `CertificateStub` instances for all valid sessions to allow `DefaultPrivilegeHandler` to restore active privilege contexts.
6. **Consistency & Reference Validation:**
   - Validate that all roles and groups assigned to users exist.
   - Validate that all roles assigned to groups exist.
   - Validate that all users referenced by access tokens exist (orphan tokens are logged and removed).

### 2. Runtime Mutations & State Synchronization

#### A. Model Modifications (`addUser`, `replaceUser`, `addRole`, `replaceRole`, `addGroup`, `replaceGroup`, `addAccessToken`)
- Synchronously updates the in-memory lookup maps (`ConcurrentHashMap`).
- Identifies the specific entity as dirty (or immediately persists using atomic writes).
- Writes only the modified element to its respective XML file (`model/<type>/<id>.xml`).

#### B. State Modifications (`updateAccessTokenLastUsed`, `updateUserState` / Login `UserHistory` Updates, `addSession`, `updateSession`)
- Authentication updates the in-memory entity and immediately writes the `.properties` file (`state/<type>/<id>.properties`).
- Session creation (`addSession`) and session access refresh (`updateSession`) immediately write the session's `.properties` file (`state/sessions/<sessionId>.properties`).
- **Zero Model Dirtying:** Updating user history, token usage, or session state does **not** mark the XML model as dirty or trigger XML serialization.
- **Zero Global Contention:** Writes are isolated to the specific user's, token's, or session's state file under an entity-specific lock.

#### C. Entity Deletions (`removeUserById`, `removeRole`, `removeGroup`, `removeAccessToken`, `removeSession`)
- Removes the element from all in-memory lookup maps.
- Deletes the corresponding XML file from `model/`.
- For users and tokens, also deletes the corresponding `.properties` file from `state/`.
- For sessions (`removeSession`), deletes the corresponding `.properties` file from `state/sessions/`.

### 3. Safe / Atomic File Operations
All file writes (both XML model files and `.properties` state files) must execute using atomic file replacement:
1. Write contents to a temporary file in the target directory: `<targetFile>.tmp`.
2. Flush and close the stream.
3. Replace the target file using `java.nio.file.Files.move`:
   ```java
   Files.move(tmpPath, targetPath, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
   ```

---

## Backward Compatibility & Migration

- `XmlPersistenceHandler` remains unchanged in `li.strolch.privilege.handler` to ensure 100% backward compatibility for existing deployments.
- Existing installations can transition to `PerElementXmlPersistenceHandler` simply by updating their `StrolchConfiguration.xml` / `PrivilegeConfig.xml`:
  ```xml
  <impl>li.strolch.privilege.handler.PerElementXmlPersistenceHandler</impl>
  ```
- With `autoMigrateMonolithic=true`, the handler will automatically convert legacy monolithic files on first startup without requiring manual migration scripts.
