# REST Resources

Strolch Web Rest provides several REST resources to interact with the agent. Most resources are prefixed with `strolch/`.

## Core Resources

### `InspectorResource`
Used to browse and inspect the state of the agent and perform CRUD operations on Strolch elements.
- **Base Path:** `strolch/inspector`

#### Realms & Components
- `GET strolch/inspector`: Returns an overview of the agent, including version and realms.
- `GET strolch/inspector/components`: Returns the list of registered components.
- `GET strolch/inspector/realms`: Returns the list of realms.
- `GET strolch/inspector/{realm}`: Returns an overview of the specified realm.
- `GET strolch/inspector/{realm}/export`: Exports the specified realm to XML.
- `POST strolch/inspector/{realm}/import`: Imports elements into the specified realm from XML.

#### Element Overview
- `GET strolch/inspector/{realm}/resources`: Returns an overview of resource types in the realm.
- `GET strolch/inspector/{realm}/orders`: Returns an overview of order types in the realm.
- `GET strolch/inspector/{realm}/activities`: Returns an overview of activity types in the realm.

#### Type Overview & Querying
- `GET strolch/inspector/{realm}/resources/{type}`: Queries elements of the specified type using `QueryData`.
- `DELETE strolch/inspector/{realm}/resources/{type}?ids={id1,id2,...}`: Deletes multiple elements of the specified type.

#### Element CRUD
- `GET strolch/inspector/{realm}/resources/{type}/{id}`: Retrieves a single element (supports XML or JSON via `Accept` header).
- `POST strolch/inspector/{realm}/resources`: Creates a new element from XML or JSON.
- `PUT strolch/inspector/{realm}/resources/{type}/{id}`: Updates an existing element from XML or JSON.
- `DELETE strolch/inspector/{realm}/resources/{type}/{id}`: Deletes a single element.

*Note: Similar endpoints exist for `orders` and `activities`.*

### `ModelResource`
Provides advanced querying capabilities.
- `POST strolch/model/soql`: Executes a SOQL (Strolch Object Query Language) query.

## Authentication & Security

### `AuthenticationResource`
Handles session management.
- `POST strolch/authentication`: Login with username and password.
- `POST strolch/authentication/sso`: Single Sign-On login.
- `DELETE strolch/authentication/{authToken}`: Logout.
- `HEAD strolch/authentication/{authToken}`: Validates a session token.
- `GET strolch/authentication/{authToken}`: Returns session details.
- `PUT strolch/authentication/{authToken}`: Refreshes a session token.
- `GET strolch/authentication/supportedOptions`: Returns supported authentication options (e.g. password reset).

### `Privilege Resources`
Manage users, roles, and groups.
- `strolch/privilege/users`: Manage user accounts.
  - `GET /`: List all users (supports `QueryData` parameters).
  - `POST /query`: Advanced user query using a `UserRep` JSON in the body.
- `strolch/privilege/roles`: Manage privilege roles.
- `strolch/privilege/groups`: Manage user groups.
- `strolch/privilege/policies`: List available privilege policies.

### `PersonalAccessTokenResource`
Manage long-lived personal access tokens.
- `GET strolch/privilege/personal-access-tokens`: List tokens.
- `POST strolch/privilege/personal-access-tokens`: Create a new token.
- `DELETE strolch/privilege/personal-access-tokens/{tokenId}`: Revoke a token.

### `UserSessionsResource`
Query and manage active user sessions.
- `GET strolch/sessions`: Query active sessions.
- `DELETE strolch/sessions/{sessionId}`: Invalidate a session.
- `PUT strolch/sessions/{sessionId}/locale/{locale}`: Update session locale.

## Utility Resources

### `AgentResource`
General agent information and actions.
- `GET strolch/agent`: Returns agent status.
- `POST strolch/agent/reloadConfiguration`: Reloads the agent configuration.

### `ReportResource`
Execute and manage reports.
- `GET strolch/reports`: List available reports.
- `GET strolch/reports/{id}/facets`: Retrieve filter facets for a report.
- `POST strolch/reports/{id}`: Execute a report and return JSON rows (supports filtering and paging in the request body).
- `POST strolch/reports/{id}/csv`: Execute a report and return CSV data.

### `ControlResource`
Manage execution activities and handler states.
- `GET strolch/control`: List active execution activities.
- `GET strolch/control/executionHandler/state`: Get execution handler state.
- `PUT strolch/control/executionHandler/state?state={state}`: Set execution handler state (e.g. `halt`, `resume`).

### `StrolchJobsResource`
Manage and execute Strolch jobs.
- `GET strolch/jobs`: List all jobs and their status.
- `PUT strolch/jobs/{name}?action={action}`: Perform action on a job (`runNow`, `schedule`, `cancel`).

### `OperationsLogResource`
Query the operations log.
- `GET strolch/operationslog/{realm}`: Returns log entries for the specified realm.

### `AuditsResource`
Query audit trails.
- `GET strolch/audits/{realm}/{type}`: Returns audit entries.

### `NotificationResource`
Manage system notifications.
- `GET strolch/notifications`: Get notifications for the current user.
- `POST strolch/notifications`: Create a new notification (Admin).

### `VersionResource`
Returns version information for the agent and its components.
- `GET strolch/version`

### `LanguagesResource`
Returns the list of supported languages.
- `GET strolch/languages`

### `I18nResource`
Retrieve and update internationalization messages.
- `GET strolch/i18n/data`: Returns `locales.json` content.
- `PUT strolch/i18n/data`: Updates `locales.json` content.

### `EnumResource`
Retrieve Strolch enumerations.
- `GET strolch/enums/{name}`: Returns the specified enumeration.
